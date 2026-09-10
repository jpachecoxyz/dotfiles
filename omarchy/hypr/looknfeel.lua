-- Change the default Omarchy look'n'feel.

-- https://wiki.hypr.land/Configuring/Basics/Variables/#general
-- hl.config({
--   general = {
--     -- No gaps between windows or borders.
--     gaps_in = 0,
--     gaps_out = 0,
--     border_size = 0,
--
--     -- Change to niri-like side-scrolling layout.
--     layout = "scrolling",
--   },
-- })

-- Use the master stack layout instead of the default dwindle layout.
-- https://wiki.hypr.land/Configuring/Layouts/Master-Layout/
hl.config({
	general = {
		layout = "master",
	},

	-- Keep the first window as master; every new window goes into the stack.
	-- https://wiki.hypr.land/Configuring/Layouts/Master-Layout/#master-new_status
	master = {
		new_status = "stack",
	},
})

-- Floating windows use a distinct border color (was in windowrules.conf).
-- https://wiki.hypr.land/Configuring/Basics/Window-Rules/
o.window({ float = true }, { border_color = "rgb(458588)" })

-- https://wiki.hypr.land/Configuring/Basics/Variables/#decoration
hl.config({
	decoration = {
		-- Use round window corners.
		rounding = 8,

		-- Dim unfocused windows (0.0 = no dim, 1.0 = fully dimmed).
		-- dim_inactive = true,
		-- dim_strength = 0.15,
	},
})

-- https://wiki.hypr.land/Configuring/Basics/Variables/#animations
-- hl.config({
--   animations = {
--     -- Disable all animations.
--     enabled = false,
--   },
-- })

-- Scratchpad: simulated drop-down from the quickshell bar. Omarchy's default
-- slidevert makes special workspaces rise from the bottom; a negative distance
-- inverts show (descends from the top) and hide (rises upward).
hl.animation({
	leaf = "specialWorkspaceIn",
	enabled = true,
	speed = 10,
	bezier = "easeOutQuint",
	style = "slidevert -100%",
})
hl.animation({
	leaf = "specialWorkspaceOut",
	enabled = true,
	speed = 10,
	bezier = "easeOutQuint",
	style = "slidevert -100%",
})

-- https://wiki.hypr.land/Configuring/Basics/Variables/#layout
-- hl.config({
--   layout = {
--     -- Avoid overly wide single-window layouts on wide screens.
--     single_window_aspect_ratio = { 1, 1 },
--   },
-- })

-- https://wiki.hypr.land/Configuring/Layouts/Scrolling-Layout/
-- hl.config({
--   scrolling = {
--     -- See only one column per screen instead of two.
--     column_width = 0.97,
--   },
-- })
