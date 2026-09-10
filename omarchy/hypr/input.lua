-- Keep only your personal input overrides here. Uncommented settings below
-- replace Omarchy's defaults.

-- Keyboard layout and options.
-- See https://wiki.hypr.land/Configuring/Basics/Variables/#input
hl.config({
	input = {
		-- UK layout with Caps Lock as the compose key (was in input.conf).
		kb_layout = "gb",
		kb_options = "compose:caps",
		-- Change speed of keyboard repeat.
		repeat_rate = 40,
		repeat_delay = 600,

		-- Start with numlock on by default.
		numlock_by_default = true,

		touchpad = {
			-- One-finger tap = left click (normal click follows).
			tap_to_click = true,
			-- Skip the two-finger "clickfinger" gesture: use button/tap zones
			-- so a press/tap on the right zone of the pad is a real right click.
			clickfinger_behavior = false,
			-- Split tap zones as left / right / middle (bottom-right = right click).
			tap_button_map = "lrm",
		},
	},
})

-- Per-device layouts: split keyboards keep a US layout (was in input.conf).
-- https://wiki.hypr.land/Configuring/Advanced-and-Cool/Devices/
hl.device({
	name = "foostan-corne-v4-keyboard",
	kb_layout = "us",
})

hl.device({
	name = "thomas-haukland-cheapino2-system-control",
	kb_layout = "us",
})

-- App-specific touchpad scroll speeds.
-- o.window("(Alacritty|kitty|foot)", { scroll_touchpad = 1.5 })
-- o.window("com.mitchellh.ghostty", { scroll_touchpad = 0.2 })

-- Enable touchpad gestures for changing workspaces.
-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Gestures/
-- hl.gesture({ fingers = 3, direction = "horizontal", action = "workspace" })

-- Enable touchpad gestures for moving focus (helpful on scrolling layout).
-- hl.gesture({ fingers = 3, direction = "left", action = function() hl.dispatch(hl.dsp.focus({ direction = "l" })) end })
-- hl.gesture({ fingers = 3, direction = "right", action = function() hl.dispatch(hl.dsp.focus({ direction = "r" })) end })
