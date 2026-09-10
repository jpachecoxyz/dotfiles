-- Personal keybinding overrides, ported from the old bindings.conf.
-- Current bindings: omarchy menu keybindings --print

-- ====================== Unbind defaults we replace or disable ======================

-- Application rebinds
hl.unbind("SUPER + RETURN") -- was: Terminal
hl.unbind("SUPER + SHIFT + RETURN") -- was: Browser
hl.unbind("SUPER + W") -- was: Close window
hl.unbind("SUPER + SHIFT + E") -- was: Email webapp
hl.unbind("SUPER + SHIFT + P") -- was: Google Photos
hl.unbind("SUPER + SLASH") -- was: Monitor scaling up
hl.unbind("SUPER + SHIFT + G") -- was: Signal
hl.unbind("SUPER + SHIFT + Y") -- was: YouTube webapp
hl.unbind("SUPER + O") -- was: Pop window out
hl.unbind("PRINT") -- was: Screenshot
hl.unbind("SUPER + SHIFT + S") -- was: Google Maps
hl.unbind("SUPER + V") -- was: Universal paste
hl.unbind("SUPER + J") -- was: Toggle window split
hl.unbind("SUPER + K") -- was: Keybindings
hl.unbind("SUPER + L") -- was: Toggle workspace layout
-- hl.unbind("SUPER + SPACE") -- was: Omarchy menu
hl.unbind("SUPER + SHIFT + A") -- was: ChatGPT
hl.unbind("SUPER + SHIFT + W") -- was: Omawrite
hl.unbind("SUPER + LEFT") -- was: Focus left
hl.unbind("SUPER + RIGHT") -- was: Focus right
hl.unbind("SUPER + UP") -- was: Focus up
hl.unbind("SUPER + DOWN") -- was: Focus down

-- Disabled without a replacement
-- SUPER + S is NOT unbound here: it's rebound to the scratchpad below.
hl.unbind("SUPER + G") -- was: Toggle window grouping
hl.unbind("SUPER + P") -- was: Pseudo window
hl.unbind("SUPER + SHIFT + BACKSPACE") -- was: Toggle window gaps (old conf typo: "BACKSPACESHIFT")
hl.unbind("ALT + PRINT") -- was: Screenrecording (moved to SUPER + SHIFT + R)

-- ====================== Applications ======================

o.bind("SUPER + RETURN", "Terminal", 'uwsm-app -- xdg-terminal-exec --dir="$(omarchy-cmd-terminal-cwd)"')
o.bind("ALT + SHIFT + RETURN", "Tmux", 'uwsm-app -- xdg-terminal-exec --dir="$(omarchy-cmd-terminal-cwd)" tmux new')
o.bind("SUPER + SHIFT + E", "File manager", "uwsm-app -- nautilus --new-window")
o.bind(
	"SUPER + E",
	"Emacs dired",
	'emacsclient -nc -a "" -F \'((name . "emacs-dired") (client-class . "emacs-dired"))\' -e \'(dired "~")\''
)
o.bind("SUPER + W", "Browser", "omarchy-launch-browser")
o.bind("SUPER + Q", "Close window", hl.dsp.window.close())
o.bind("SUPER + SLASH", "Editor", 'emacsclient -nc -a ""')

-- Web apps
o.bind("SUPER + SHIFT + G", "Github", { webapp = "https://github.com/jpachecoxyz" })
o.bind("SUPER + SHIFT + Y", "YouTube", { webapp = "https://youtube.com/", focus = true })

-- Menus & captures
-- o.bind("SUPER + O", "Omarchy menu", "omarchy-menu toggle")
o.bind("SUPER + O", "Omarchy menu", "omarchy-menu toggle")
o.bind("PRINT", "Screenshot full", "omarchy capture screenshot fullscreen")
o.bind("SUPER + SHIFT + S", "Screenshot region", "omarchy capture screenshot region")
o.bind("SUPER + SHIFT + PRINT", "Capture menu", "omarchy menu toggle capture")
o.bind("SUPER + SHIFT + R", "Screenrecording", "omarchy-menu toggle trigger.capture.screenrecord")

-- Terminal flotante del scratchpad.
o.bind("SUPER + SHIFT + RETURN", "Scratchpad terminal", hl.dsp.workspace.toggle_special("scratchpad-terminal"))

-- Dropdown terminal (disabled, replaced by the scratchpad)
-- o.bind(
-- 	"SUPER + SHIFT + RETURN",
-- 	"Dropdown terminal",
-- 	"bash "
-- 		.. os.getenv("HOME")
-- 		.. "/.config/omarchy/plugins/io.github.tuthan.dropdown-terminal/bin/omarchy-dropdown-terminal"
-- )

-- Clipboard
o.bind("SUPER + V", "Clipboard manager", "omarchy menu clipboard")

-- Sticky notes (Omasticky)
o.bind("SUPER + N", "Toggle sticky notes", "omarchy-shell omasticky toggleHidden")

-- ====================== Window management ======================

o.bind("SUPER + J", "Focus next window", hl.dsp.window.cycle_next())
o.bind("SUPER + K", "Focus previous window", hl.dsp.window.cycle_next({ next = false }))
o.bind("SUPER + SHIFT + J", "Swap with next window", hl.dsp.window.swap({ next = true }))
o.bind("SUPER + SHIFT + K", "Swap with previous window", hl.dsp.window.swap({ prev = true }))

-- Resize / move (repeat while held)
o.bind(
	"SUPER + L",
	"Resize window right",
	hl.dsp.window.resize({ x = 40, y = 0, relative = true }),
	{ repeating = true }
)
o.bind(
	"SUPER + H",
	"Resize window left",
	hl.dsp.window.resize({ x = -40, y = 0, relative = true }),
	{ repeating = true }
)
o.bind(
	"SUPER + RIGHT",
	"Move window right",
	hl.dsp.window.move({ x = 50, y = 0, relative = true }),
	{ repeating = true }
)
o.bind(
	"SUPER + LEFT",
	"Move window left",
	hl.dsp.window.move({ x = -50, y = 0, relative = true }),
	{ repeating = true }
)
o.bind("SUPER + DOWN", "Move window down", hl.dsp.window.move({ x = 0, y = 50, relative = true }), { repeating = true })
o.bind("SUPER + UP", "Move window up", hl.dsp.window.move({ x = 0, y = -50, relative = true }), { repeating = true })

o.bind("SUPER + SHIFT + L", "Hyprlock", "hyprlock")

-- Floating windows (float, size exactly 1260x600, then center)
-- o.bind("SUPER + SPACE", "Toggle floating (centered 1260x600)", function()
-- 	hl.dispatch(hl.dsp.window.float({ action = "toggle" }))
-- 	hl.dispatch(hl.dsp.window.resize({ x = 1260, y = 600 }))
-- 	hl.dispatch(hl.dsp.window.center())
-- end)

o.bind("ALT + TAB", "Workspaces Overview", "omarchy-shell shell toggle omarchy-overview")

-- Special workspaces
o.bind("SUPER + SHIFT + A", "Gemini", hl.dsp.workspace.toggle_special("gemini"))
o.bind("SUPER + SHIFT + W", "WhatsApp", hl.dsp.workspace.toggle_special("whatsapp"))
o.bind("SUPER + SHIFT + T", "Telegram", hl.dsp.workspace.toggle_special("telegram"))

-- Dropdown Terminal
hl.bind("CTRL + GRAVE", hl.dsp.global("io.github.tuthan.dropdown-terminal:toggle"))
