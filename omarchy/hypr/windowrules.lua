-- Personal window and workspace rules, ported from the old windowrules.conf.
-- Same rules, rewritten for Hyprland's Lua config API (hl.window_rule /
-- hl.workspace_rule). Loaded from hyprland.lua.

-- ============================ Window rules ============================

hl.window_rule({
	name = "WebBrowser",
	match = { class = "[Cc]hromium" },
	workspace = "2 silent",
})

hl.window_rule({
	name = "WebBrowserPrivateChromium",
	match = { title = "New Incognito Tab - Chromium" },
	workspace = "10 silent",
})

hl.window_rule({
	name = "Emacs",
	match = { class = "[Ee]macs" },
	workspace = "3",
})

hl.window_rule({
	name = "EmacsScripts",
	match = { class = "emacs-agenda" },
	float = true,
	size = { 500, 500 },
	animation = "popin",
})

hl.window_rule({
	name = "Qemu",
	match = { class = "qemu" },
	workspace = "10 silent",
	fullscreen_state = "3",
})

-- Scratchpad terminal (toggled with SUPER + SHIFT + RETURN).
hl.window_rule({
	name = "Scratchpad",
	match = { class = "scratchpad" },
	float = true,
	size = { 1500, 500 },
	move = { "(monitor_w * 0.5 - 750)", "50" },
	animation = "slide top",
})

hl.window_rule({
	name = "FreeCAD",
	match = { class = "org.freecad.FreeCAD" },
	workspace = "8",
})

hl.window_rule({
	name = "Doppler",
	match = { title = "doppler.gif - mpv" },
	float = true,
	size = { 500, 500 },
	animation = "popin",
})

hl.window_rule({
	name = "Mpv",
	match = { class = "[Mm]pv" },
	float = true,
})

-- Web apps (opened in their special workspaces).
hl.window_rule({
	name = "ChatGPTWebapp",
	match = { class = ".*chatgpt\\.com__-Default" },
	float = true,
	size = { 1500, 800 },
	center = true,
})

hl.window_rule({
	name = "GeminiWebapp",
	match = { class = ".*gemini\\.google\\.com__-Default" },
	float = true,
	size = { 1500, 800 },
	center = true,
})

hl.window_rule({
	name = "WhatsApp",
	match = { class = "com.rtosta.zapzap" },
	float = true,
	size = { 1500, 800 },
	center = true,
})

hl.window_rule({
	name = "Telegram",
	match = { class = "org.telegram.desktop" },
	float = true,
	size = { 1500, 800 },
	center = true,
})

hl.window_rule({
	name = "FloatBorder",
	match = { float = true },
	border_color = "rgb(458588)",
})

-- =========================== Workspace rules ============================

hl.workspace_rule({
	workspace = "special:chatgpt",
	on_created_empty = 'omarchy-launch-webapp "https://chatgpt.com"',
})

hl.workspace_rule({
	workspace = "special:gemini",
	on_created_empty = 'omarchy-launch-webapp "https://gemini.google.com"',
})

hl.workspace_rule({
	workspace = "special:whatsapp",
	on_created_empty = "com.rtosta.zapzap",
})

hl.workspace_rule({
	workspace = "special:telegram",
	on_created_empty = "org.telegram.desktop",
})

-- Terminal flotante del scratchpad: lo abre SUPER + SHIFT + RETURN.
hl.workspace_rule({
	workspace = "special:scratchpad-terminal",
	on_created_empty = "uwsm-app -- alacritty --class scratchpad",
})

-- Scratchpad genérico: aquí llegan las ventanas movidas con SUPER + ALT + S.
-- SUPER + S lo muestra/oculta. Vacío no abre nada a propósito.
hl.workspace_rule({
	workspace = "special:scratchpad",
})
