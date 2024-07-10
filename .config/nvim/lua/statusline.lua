-- INI STATUSLINE
-- local util = require('creativenull.utils')
vim.opt.showmode = false
vim.opt.laststatus = 2
local CTRL_v = vim.api.nvim_replace_termcodes('<C-v>', true, true, true)
local CTRL_s = vim.api.nvim_replace_termcodes('<C-s>', true, true, true)
local modes = setmetatable({
    ["n"]    = "[NORMAL]",
    ["i"]    = "[INSERT]",
    ["R"]    = "[REPLACE]",
    ["v"]    = "[VISUAL]",
    ["V"]    = "[V-LINE]",
    [CTRL_v] = "[V-BLOCK]",
    ["c"]    = "[COMMAND]",
    ["s"]    = "[SELECT]",
    ["S"]    = "[S-LINE]",
    [CTRL_s] = "[S-BLOCK]",
    ["t"]    = "[TERMINAL]",
}, {
    __index = function()
        return "UNKNOWN "
    end
})

function GitBranch()
    local cmd = 'git branch --show-current'

    local is_dir = util.is_dir(vim.fn.getcwd() .. '/.git')
    if not is_dir then
        return ''
    end

    local fp = io.popen(cmd)
    local branch = fp:read('*a')

    -- TODO:
    -- Will need to check if the '^@' chars are at the end
    -- instead of implicitly removing the last 2 chars
    branch = string.sub(branch, 0, -2)
    return branch
end

local get_current_mode = function()
    local current_mode = vim.api.nvim_get_mode().mode
    return string.format("%s", modes[current_mode])
end

local file_type = function()
    local v_ft = vim.bo.filetype
    if v_ft == nil or v_ft == "" then
        return string.format("%s", "no ft")
    else
        return string.format("%s", v_ft)
    end
end

local file_format = function()
    return string.format("%s", vim.bo.fileformat)
end

local file_enc = function()
    local v_fenc = vim.bo.fileencoding
    if v_fenc == nil or v_fenc == "" then
        return string.format("%s", vim.go.encoding)
    else
        return string.format("%s", v_fenc)
    end
end

Statusline = function()
    return table.concat {
        get_current_mode(),
        "%r %t %m",
        "%=",
        file_format(),
        "|",
        file_enc(),
        "|",
        file_type(),
        "|",
        "%3p%%",
        "|",
        "%l,%c",
        ""
    }
end

vim.opt.statusline = "%!v:lua.Statusline()"

-- FIN STATUSLINE
