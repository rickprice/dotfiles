-- timing-trainer.lua
--
-- Measures how well a keyboard player keeps time with a metronome.
-- Pan left  = too fast (playing ahead of the beat)
-- Pan right = too slow (playing behind the beat)
-- Center    = on time, or auto-reset after idle silence
--
-- Connect the metronome.lua note output to the "metronome" input and your
-- keyboard to the "keyboard" input.  Route the "pan" output to whatever
-- plugin controls panning in your signal chain.
--
-- ALSA ports created:
--   inputs:  midi-daemon:timing-trainer/metronome-in
--             midi-daemon:timing-trainer/keyboard-in
--   outputs: midi-daemon:timing-trainer/pan-out
--
-- config.toml example:
--   [timing-trainer]
--   pan_channel             = 1     -- MIDI channel for the pan CC output
--   pan_controller          = 10    -- CC number (10 = standard MIDI pan)
--   max_error_ms            = 200   -- ±ms where pan reaches full L or R
--   history_size            = 8     -- how many recent hits to average
--   idle_seconds            = 3     -- seconds of silence before resetting to center
--   start_stop_channel      = 1     -- MIDI channel for the enable/disable CC
--   start_stop_controller   = 22    -- CC controller that enables/disables (≥64 on, <64 off)
--   start_running           = true  -- whether training is active on launch
--   osc_receive_port        = 9002  -- optional: enable OSC control on this UDP port

local PAN_CHANNEL    = config.pan_channel    or 1
local PAN_CONTROLLER = config.pan_controller or 10
local MAX_ERROR_MS   = config.max_error_ms   or 200
local HISTORY_SIZE   = config.history_size   or 8
local IDLE_SECONDS   = config.idle_seconds   or 3

local START_STOP_CHANNEL    = config.start_stop_channel    or 1
local START_STOP_CONTROLLER = config.start_stop_controller or 22

-- ── State ─────────────────────────────────────────────────────────────────────
-- Declared before init() so that closures in the params table can close over them.

local running       = (config.start_running ~= false)  -- default true
local current_tick  = 0
local beat_tick     = nil   -- tick at which the last metronome beat arrived
local last_key_tick = nil   -- tick at which the last keyboard note arrived
local error_history = {}    -- rolling window of signed errors in ms
local current_pan   = 64    -- last pan value sent (avoids redundant CC spam)
local initialized   = false -- send initial center CC on first tick

local function send_pan(value)
    if value == current_pan then return end
    current_pan = value
    send("pan", { type = "cc", channel = PAN_CHANNEL, controller = PAN_CONTROLLER, value = value })
end

local function set_running(state)
    if running == state then return end
    running = state
    if not running then
        error_history = {}
        beat_tick     = nil
        last_key_tick = nil
        send_pan(64)
    end
    log(running and "Enabled" or "Disabled")
end

-- ─────────────────────────────────────────────────────────────────────────────

function init()
    return {
        inputs  = {"metronome", "keyboard"},
        outputs = {"pan"},
        osc = {
            receive = config.osc_receive_port,  -- optional OSC control
            params = {
                running = {
                    set = function(v) set_running(v ~= 0) end,
                    get = function() return running and 1 or 0 end,
                    -- CC value ≥ 64 → enable, < 64 → disable
                    midi = {
                        { type = "cc", channel = START_STOP_CHANNEL,
                          controller = START_STOP_CONTROLLER, threshold = 64 },
                    },
                },
                start = {
                    set = function() set_running(true) end,
                    midi = { { type = "start" }, { type = "continue" } },
                },
                stop = {
                    set = function() set_running(false) end,
                    midi = { { type = "stop" } },
                },
            },
        },
    }
end

-- ── Helpers ───────────────────────────────────────────────────────────────────

local function tick_ms(bpm, ppqn)
    return 60000.0 / (bpm * ppqn)
end

-- Wrap the raw tick delta into [-ppqn/2, +ppqn/2] so early notes are negative.
local function signed_error_ticks(key_t, beat_t, ppqn)
    local delta = key_t - beat_t
    local half  = ppqn / 2
    if delta > half then
        delta = delta - ppqn
    elseif delta < -half then
        delta = delta + ppqn
    end
    return delta
end

local function average(t)
    if #t == 0 then return 0 end
    local s = 0
    for _, v in ipairs(t) do s = s + v end
    return s / #t
end

-- Map a signed error in ms to a 0-127 pan value.
-- Negative = early = left (0), positive = late = right (127), zero = center (64).
local function error_to_pan(err_ms)
    local norm = math.max(-1.0, math.min(1.0, err_ms / MAX_ERROR_MS))
    return math.max(0, math.min(127, math.floor(64 + norm * 63 + 0.5)))
end

-- ── Callbacks ─────────────────────────────────────────────────────────────────

function on_tick(tick, bpm, ppqn)
    current_tick = tick

    -- Send initial center pan so the connected plugin starts in a known state.
    if not initialized then
        initialized = true
        send("pan", { type = "cc", channel = PAN_CHANNEL, controller = PAN_CONTROLLER, value = 64 })
    end

    -- Reset to center after idle silence.
    if running and last_key_tick then
        local idle_ticks = math.ceil(IDLE_SECONDS * bpm * ppqn / 60.0)
        if tick - last_key_tick >= idle_ticks then
            error_history = {}
            last_key_tick = nil
            send_pan(64)
            log("Idle: pan reset to center")
        end
    end
end

function on_midi(msg)
    -- CC enable/disable and transport start/stop/continue are handled by params.
    if not running then return end

    -- Track when each metronome beat arrives.
    if msg.port == "metronome" then
        if msg.type == "note_on" then
            beat_tick = current_tick
        end
        return
    end

    -- Keyboard: only care about note_on (attack = the timing moment).
    if msg.port == "keyboard" and msg.type == "note_on" then
        last_key_tick = current_tick

        if not beat_tick then return end

        local bpm  = get_bpm()
        local ppqn = get_ppqn()

        local err_ticks = signed_error_ticks(current_tick, beat_tick, ppqn)
        local err_ms    = err_ticks * tick_ms(bpm, ppqn)

        table.insert(error_history, err_ms)
        if #error_history > HISTORY_SIZE then
            table.remove(error_history, 1)
        end

        local avg = average(error_history)
        local pan = error_to_pan(avg)
        send_pan(pan)

        log(string.format(
            "error: %+.0f ms  avg: %+.0f ms  pan: %d (%s)",
            err_ms, avg, pan,
            pan < 60 and "early" or pan > 68 and "late" or "on time"
        ))
    end
end
