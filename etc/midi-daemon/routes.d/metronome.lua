-- metronome.lua
-- A simple configurable metronome.
-- Plays a high click on beat 1, low click on other beats.
-- Virtual ports created: midi-daemon:metronome (in + out)

local BEAT_1_NOTE  = config.beat_1_note  or 37   -- Side Stick (GM)
local BEAT_N_NOTE  = config.beat_n_note  or 56   -- Cowbell (GM)
local CHANNEL      = config.channel      or 10   -- GM percussion channel
local VELOCITY     = config.velocity     or 100
local BEATS_PER_BAR = config.beats_per_bar or 4
local NOTE_LEN_MS   = config.note_len_ms   or 20   -- fixed note duration in ms

-- CC that controls BPM (maps 0–127 → 20–200 BPM)
local CC_TYPE       = config.cc_type       or "cc"
local CC_CHANNEL    = config.cc_channel    or 1
local CC_CONTROLLER = config.cc_controller or 21

-- CC that starts/stops the metronome (value >= 64 = start, value < 64 = stop)
-- MIDI Transport Start (0xFA), Stop (0xFC), and Continue (0xFB) are also honoured.
local START_STOP_CHANNEL    = config.start_stop_channel    or 1
local START_STOP_CONTROLLER = config.start_stop_controller or 22

set_bpm(config.bpm   or 120)
set_ppqn(config.ppqn or 24)

local beat = 0
local note_off_at = {}  -- tick -> {note, channel}
local running = (config.start_running ~= false)  -- default true

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

        -- Schedule note-off after a fixed wall-clock duration regardless of BPM.
        -- Tick duration = 60000 / (bpm * ppqn) ms, so ticks needed for NOTE_LEN_MS:
        local off_ticks = math.max(1, math.floor(NOTE_LEN_MS * bpm * ppqn / 60000.0 + 0.5))
        local off_tick = tick + off_ticks
        note_off_at[off_tick] = note_off_at[off_tick] or {}
        table.insert(note_off_at[off_tick], { note = note, channel = CHANNEL })

        log(string.format("Beat %d/%d  BPM: %.1f", beat, BEATS_PER_BAR, bpm))
    end
end

function on_midi(msg)
    -- BPM control CC
    if msg.type == CC_TYPE and msg.channel == CC_CHANNEL and msg.controller == CC_CONTROLLER then
        local new_bpm = 20 + (msg.value / 127.0) * 180
        set_bpm(new_bpm)
        log(string.format("BPM changed to %.1f", new_bpm))
    -- Start/stop CC (value >= 64 starts, value < 64 stops)
    elseif msg.type == "cc" and msg.channel == START_STOP_CHANNEL
            and msg.controller == START_STOP_CONTROLLER then
        set_running(msg.value >= 64)
    -- MIDI Transport messages
    elseif msg.type == "start" then
        transport_start()
    elseif msg.type == "continue" then
        set_running(true)
    elseif msg.type == "stop" then
        set_running(false)
    end
end
