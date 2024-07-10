-- Personal Configuration keymaps
-- Variables declarations

-- [[ Setting options ]]
-- See `:help vim.o`

-- Set highlight on search
vim.o.hlsearch = false
vim.o.cmdheight = 1

-- Make line numbers default
vim.wo.number = true
vim.wo.relativenumber = true

-- Enable mouse mode
vim.o.mouse = 'a'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true
vim.cmd([[highlight clear LineNr]])
vim.cmd([[highlight clear SignColumn]])
vim.cmd([[highlight GitSignsAdd guibg = NONE]])
vim.cmd([[highlight GitSignsChange guibg=NONE]])
vim.cmd([[highlight GitSignsDelete guibg=NONE]])

-- Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'
vim.opt.showmode = false

local map = vim.api.nvim_set_keymap
local opts = { noremap = true, silent = true }
local os = vim.loop.os_uname().sysname

vim.keymap.set("n", "<bs>", ":edit #<cr>", { silent = true })
-- Blink cursor:
vim.cmd([[set guicursor+=a:-blinkwait175-blinkoff150-blinkon175]])
vim.cmd([[hi Normal guibg=NONE ctermbg=NONE]])
vim.opt.scrolloff = 5
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.colorcolumn = "81"
vim.opt.clipboard = "unnamedplus"
vim.opt.autochdir = true
vim.opt.showtabline = 1
vim.opt.conceallevel = 2
vim.opt.cursorline = false
-- Do not load tohtml.vim

-- Tabs keys
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.g.rst_syntax_code_list = { "python", "lua" }
vim.g.loaded_2html_plugin = 1

-- Do not load zipPlugin.vim, gzip.vim and tarPlugin.vim (all these plugins are
-- related to checking files inside compressed files)
-- Do not load the tutor plugin
-- Do not use builtin matchit.vim and matchparen.vim since we use vim-matchup
-- Disable sql omni completion, it is broken.
vim.g.loaded_zipPlugin = 1
vim.g.loaded_gzip = 1
vim.g.loaded_tarPlugin = 1
vim.g.loaded_tutor_mode_plugin = 1
vim.g.loaded_matchit = 1
vim.g.loaded_matchparen = 1
vim.g.loaded_sql_completion = 1

-- Keymaps
-- git
map("n", "<leader>gs", ":Telescope git_status<cr>", opts)
map("n", "<leader>gc", ":Telescope git_commits<cr>", opts)
map("n", "<leader>gb", ":Telescope git_branches<cr>", opts)

-- Move windows:
map("n", "<C-h>", "<C-W>h", opts)
map("n", "<C-j>", "<C-W>j", opts)
map("n", "<C-k>", "<C-W>k", opts)
map("n", "<C-l>", "<C-W>l", opts)

-- Move in insert mode like in nirmal mode hjkl:
map("i", "<C-h>", "<Left>", opts)
map("i", "<C-j>", "<Down>", opts)
map("i", "<C-k>", "<Up>", opts)
map("i", "<C-l>", "<Right>", opts)

-- barbar mappings --
-- Move to previous/next
map("n", "<A-,>", ":bprevious<CR>", opts)
map("n", "<A-.>", ":bnext<CR>", opts)
map("n", "<A-q>", ":bd<CR>", opts)

-- documents markdown, pdf & norg files.
map("n", "<leader>pdf", ":silent ! latexpdf % <CR>", opts)
map("n", "<leader>md", ":MarkdownPreview<CR>", opts)
map("n", "<leader>doc",
    ":silent ! pandoc ./% --pdf-engine=xelatex --template eisvogel -o ./format/pdf/%.pdf<CR>", opts)
map("n", "<leader>ne", ":Neorg export to-file<CR>", opts)
map("n", "<leader>zz", ":ZenMode<CR>", opts)

--
-- [[ nvim Config files ]]
if os == "Windows_NT" then
    map("n", "<leader>ecf", ":cd ~/Appdata/Local/nvim/ | Telescope find_files<CR>", opts)
elseif os == "Linux" then
    map("n", "<leader>ecf", ":cd ~/.config/nvim/ | Telescope find_files<CR>", opts)
end

map("n", "<C-S>", ":%s/\\<<C-r><C-w>\\>//gI<Left><Left><Left>", opts)
map("v", "<", "<gv", opts)
map("v", ">", ">gv", opts)
map("n", "<M-x>", ":!", opts) -- Meta X like emacs. This is blasphemy....
map("n", "<leder>hrr", ":so%<CR>", opts)

map("i", "jk", "<Esc>l", opts)
map("i", "jj", "<Esc>l", opts)
map("i", "qw", "<Esc>$", opts)
map("n", "cw", "ciw", opts)
map("n", "vw", "viw", opts)
map("n", "<esc>", ":noh<return><esc>", opts)
map("n", "<f12>", ":!python %<CR>", opts)
map("n", "<f9>", ":setlocal spell! spelllang=en<CR>", opts)
map("n", "<f10>", ":setlocal spell! spelllang=es<CR>", opts)
map("i", "<M->>", "<C-x>s", opts)
map("n", "<leader><leader>e", ":Ex<CR>", opts)
map("n", "<leader>ni", ":Neorg index<CR>", opts)
map("n", "<leader>?", ":Telescope oldfiles theme=get_ivy<CR>", opts)
map("n", "<leader>,", ":Telescope buffers theme=get_ivy<CR>", opts)
vim.keymap.set('n', '<localleader>x', ':Neorg exec cursor<CR>', { silent = true })

vim.cmd([[hi tkLink ctermfg=Cyan cterm=bold,underline guifg=blue gui=bold,underline]])
vim.cmd([[hi tkBrackets ctermfg=gray guifg=gray]])

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
-- vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
    callback = function()
        vim.highlight.on_yank()
    end,
    group = highlight_group,
    pattern = '*',
})

-- [[ LSP diagnositcs ]]

vim.api.nvim_set_keymap('n', '<leader>do', '<cmd>lua vim.diagnostic.open_float()<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>d[', '<cmd>lua vim.diagnostic.goto_prev()<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>d]', '<cmd>lua vim.diagnostic.goto_next()<CR>', { noremap = true, silent = true })
-- The following command requires plug-ins "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim", and optionally "kyazdani42/nvim-web-devicons" for icon support
vim.api.nvim_set_keymap('n', '<leader>dd', '<cmd>Telescope diagnostics<CR>', { noremap = true, silent = true })
-- If you don't want to use the telescope plug-in but still want to see all the errors/warnings, comment out the telescope line and uncomment this:
-- vim.api.nvim_set_keymap('n', '<leader>dd', '<cmd>lua vim.diagnostic.setloclist()<CR>', { noremap = true, silent = true })

vim.keymap.set(
    "",
    "<Leader>l",
    require("lsp_lines").toggle,
    { desc = "Toggle lsp_lines" }
)

-- [[ nvim-tree ]]

map("n", "<C-e>", ":Vexplore<CR>", opts)
