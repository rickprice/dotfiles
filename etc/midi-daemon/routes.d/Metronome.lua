-- metronome.lua
-- Configurable metronome with MIDI and optional OSC control/output.
--
-- OSC is handled by the global listener/sender configured in config.toml:
--   osc_receive_port = 9000        (all routes share one UDP port)
--   osc_send_addr    = "host:port" (all routes share one send destination)
--
-- OSC output:
--   When a named send target is configured, /metronome/beat and /metronome/running
--   are sent there. When no send target is defined, these messages are automatically
--   fanned out to any clients that have sent /metronome/subscribe.
--   /metronome/beat    <beat:int> <beats_per_bar:int> <bpm:float>
--   /metronome/running <1|0>
--
-- OSC params (handled automatically by Rust, declared in init()):
--   /metronome/bpm <value>   — set BPM (20–200)
--   /metronome/running <1|0> — start (1) or stop (0)
--   /metronome/start         — reset to beat 1 and start
--   /metronome/stop          — stop and reset
--   /metronome/continue      — resume from current position
--   /metronome/bpm           — query current BPM (no args → reply)
--   /metronome/running       — query running state (no args → reply)

local BEAT_1_NOTE   = config.beat_1_note   or 37   -- Side Stick (GM)
local BEAT_N_NOTE   = config.beat_n_note   or 56   -- Cowbell (GM)
local CHANNEL       = config.channel       or 10   -- GM percussion channel
local VELOCITY      = config.velocity      or 100
local BEATS_PER_BAR = config.beats_per_bar or 4
local NOTE_LEN_MS   = config.note_len_ms   or 20   -- fixed note duration in ms

-- MIDI binding config — read in init() for the param midi arrays.
local CC_CHANNEL             = config.cc_channel             or 1
local CC_CONTROLLER          = config.cc_controller          or 21
local START_STOP_CHANNEL     = config.start_stop_channel     or 1
local START_STOP_CONTROLLER  = config.start_stop_controller  or 22

set_bpm(config.bpm   or 120)
set_ppqn(config.ppqn or 24)

local beat        = 0
local note_off_at = {}  -- tick -> list of {note, channel}
local running     = (config.start_running ~= false)  -- default true

local function flush_notes()
    for _, evs in pairs(note_off_at) do
        for _, ev in ipairs(evs) do
            send({ type = "note_off", channel = ev.channel, note = ev.note, velocity = 0 })
        end
    end
    note_off_at = {}
end

local function set_running(state)
    if running == state then return end
    running = state
    if not running then
        flush_notes()
        beat = 0
    end
    send_osc("/" .. ROUTE_NAME .. "/running", running and 1 or 0)
    log(running and "Started" or "Stopped")
end

local function transport_start()
    flush_notes()
    beat = 0
    if not running then
        running = true
        log("Started")
    else
        log("Restarted from beat 1")
    end
    send_osc("/" .. ROUTE_NAME .. "/running", 1)
end

function init()
    return {
        inputs  = {"midi"},
        outputs = {"midi"},
        osc = {
            params = {
                bpm = {
                    set = function(v) set_bpm(v); log(string.format("BPM: %.1f", v)) end,
                    get = get_bpm,
                    -- CC payload 0–127 scaled linearly to 20–200 BPM
                    midi = {
                        { type = "cc", channel = CC_CHANNEL,
                          controller = CC_CONTROLLER, scale = {20, 200} },
                    },
                },
                running = {
                    set = function(v) set_running(v ~= 0) end,
                    get = function() return running and 1 or 0 end,
                    -- CC value ≥ 64 → start (1), < 64 → stop (0)
                    midi = {
                        { type = "cc", channel = START_STOP_CHANNEL,
                          controller = START_STOP_CONTROLLER, threshold = 64 },
                    },
                },
                start = {
                    set = transport_start,
                    midi = { { type = "start" } },
                },
                stop = {
                    set = function() set_running(false) end,
                    midi = { { type = "stop" } },
                },
                continue = {
                    set = function() set_running(true) end,
                    midi = { { type = "continue" } },
                },
            },
        },
    }
end

function on_tick(tick, bpm, ppqn)
    if not running then return end

    -- Handle pending note-offs
    if note_off_at[tick] then
        for _, ev in ipairs(note_off_at[tick]) do
            send({ type = "note_off", channel = ev.channel, note = ev.note, velocity = 0 })
        end
        note_off_at[tick] = nil
    end

    -- Fire on quarter-note boundaries
    if tick % ppqn == 0 then
        beat = (beat % BEATS_PER_BAR) + 1
        local note = (beat == 1) and BEAT_1_NOTE or BEAT_N_NOTE

        send({ type = "note_on", channel = CHANNEL, note = note, velocity = VELOCITY })
        send_osc("/" .. ROUTE_NAME .. "/beat", beat, BEATS_PER_BAR, bpm)

        -- Schedule note-off after a fixed wall-clock duration regardless of BPM.
        -- Tick duration = 60000 / (bpm * ppqn) ms, so ticks needed for NOTE_LEN_MS:
        local off_ticks = math.max(1, math.floor(NOTE_LEN_MS * bpm * ppqn / 60000.0 + 0.5))
        local off_tick  = tick + off_ticks
        note_off_at[off_tick] = note_off_at[off_tick] or {}
        table.insert(note_off_at[off_tick], { note = note, channel = CHANNEL })

        log(string.format("Beat %d/%d  BPM: %.1f", beat, BEATS_PER_BAR, bpm))
    end
end

