-- VolumePanMuteControl.lua
-- Bidirectional OSC ↔ MIDI bridge for channel volume, pan, and mute.
--
-- OSC parameters (address prefix = /VolumePanMuteControl):
--   /VolumePanMuteControl/volume  <float 0–1>    — channel volume (0=silent, 1=full)
--   /VolumePanMuteControl/pan     <float -1–1>   — stereo pan (−1=left, 0=center, 1=right)
--   /VolumePanMuteControl/mute    <bool|int 0|1> — mute (1=muted, 0=unmuted)
--
-- Subscription (TouchOSC / Lemur):
--   /VolumePanMuteControl/subscribe [port [timeout_secs]]
--
-- Default MIDI CCs (override in config.toml [VolumePanMuteControl]):
--   volume → CC  7  (MIDI Channel Volume)
--   pan    → CC 10  (MIDI Pan; 0=left, 64=center, 127=right)
--   mute   → CC 118 (no universal MIDI mute standard; configure to suit your DAW)

local CHANNEL = config.channel or 1

local VOLUME_CHANNEL    = config.volume_channel    or CHANNEL
local VOLUME_CONTROLLER = config.volume_controller or 7    -- CC 7 = MIDI Channel Volume

local PAN_CHANNEL       = config.pan_channel       or CHANNEL
local PAN_CONTROLLER    = config.pan_controller    or 10   -- CC 10 = MIDI Pan

local MUTE_CHANNEL      = config.mute_channel      or CHANNEL
local MUTE_CONTROLLER   = config.mute_controller   or 118  -- no universal MIDI mute CC

local volume = 1.0   -- 0–1, default full
local pan    = 0.0   -- −1–1, default center
local muted  = false

local function send_volume_cc()
    send({ type = "cc", channel = VOLUME_CHANNEL, controller = VOLUME_CONTROLLER,
           value = math.floor(volume * 127.0 + 0.5) })
end

local function send_pan_cc()
    -- OSC −1..1 → MIDI 0..127 (pan=0 → CC 64, the MIDI center)
    send({ type = "cc", channel = PAN_CHANNEL, controller = PAN_CONTROLLER,
           value = math.floor((pan + 1.0) / 2.0 * 127.0 + 0.5) })
end

local function send_mute_cc()
    send({ type = "cc", channel = MUTE_CHANNEL, controller = MUTE_CONTROLLER,
           value = muted and 127 or 0 })
end

function init()
    return {
        inputs  = {"midi"},
        outputs = {"midi"},
        osc = {
            params = {
                Volume = {
                    set = function(v)
                        volume = math.max(0.0, math.min(1.0, v))
                        send_volume_cc()
                        log(string.format("Volume: %.3f", volume))
                    end,
                    get = function() return volume end,
                    -- Incoming CC 7 (0–127) scaled linearly to OSC range 0.0–1.0
                    midi = {
                        { type = "cc", channel = VOLUME_CHANNEL,
                          controller = VOLUME_CONTROLLER, scale = {0.0, 1.0} },
                    },
                },
                Pan = {
                    set = function(v)
                        pan = math.max(-1.0, math.min(1.0, v))
                        send_pan_cc()
                        log(string.format("Pan: %.3f", pan))
                    end,
                    get = function() return pan end,
                    -- Incoming CC 10 (0–127) scaled linearly to OSC range −1.0–1.0
                    midi = {
                        { type = "cc", channel = PAN_CHANNEL,
                          controller = PAN_CONTROLLER, scale = {-1.0, 1.0} },
                    },
                },
                Mute = {
                    set = function(v)
                        muted = (v ~= 0 and v ~= false)
                        send_mute_cc()
                        log(muted and "Muted" or "Unmuted")
                    end,
                    get = function() return muted and 1 or 0 end,
                    -- Incoming CC value ≥ 64 → muted (1), < 64 → unmuted (0)
                    midi = {
                        { type = "cc", channel = MUTE_CHANNEL,
                          controller = MUTE_CONTROLLER, threshold = 64 },
                    },
                },
            },
        },
    }
end
