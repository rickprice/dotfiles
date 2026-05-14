-- invert_controllers.lua
-- Forwards all incoming MIDI messages, inverting the value (0–127 → 127–0)
-- for a configured set of controllers on a specific message type and channel.
-- Virtual ports: midi-daemon:invert_controllers (in + out)

local TYPE        = config.type        or "cc"
local CHANNEL     = config.channel     or 1
local controllers = config.controllers or {}

-- Build a set for O(1) lookup
local invert_set = {}
for _, controller in ipairs(controllers) do
    invert_set[controller] = true
end

function on_midi(msg)
    if msg.type == TYPE and msg.channel == CHANNEL and invert_set[msg.controller] then
        msg.value = 127 - msg.value
    end
    send(msg)
end
