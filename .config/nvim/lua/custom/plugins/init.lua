-- [[ insert your plugins below ]]

return {
  -- [[ Eyecandy & fancy prgramming stuff ]]

  {
    "mcchrish/zenbones.nvim",
    dependencies = "rktjmp/lush.nvim",
    priority = 1000,
    config = function()
      vim.g.jpbones = {
        solid_line_nr = true,
        darken_comments = 45,
        transparent_background = true,
      }
      vim.cmd [[colorscheme jpbones]]
    end
  },
  --
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
  },

  { 'tpope/vim-sleuth' }, -- Detect tabstop and shiftwidth automatically

  { 'matze/vim-move' },
  { 'itchyny/calendar.vim' },

  {
    'windwp/nvim-autopairs',
    config = function()
      require('nvim-autopairs').setup()
    end
  },

  { 'tpope/vim-surround' },

  { 'lilydjwg/colorizer' },

  {
    'stevearc/dressing.nvim',
    config = function()
      require('dressing').setup()
    end
  },

  {
    "yamatsum/nvim-cursorline",
    config = function()
      require('nvim-cursorline').setup {
        cursorline = {
          enable = false,
          timeout = 1000,
          number = false,
        },
        cursorword = {
          enable = true,
          min_length = 2,
          hl = { underline = true },
        }
      }
    end
  },

  -- [[ Markdown Preview ]]

  -- Preview in browser
  {
    "iamcco/markdown-preview.nvim",
    ft = { "markdown" },
    build = "cd app && npm install",
    config = function()
      vim.g.mkdp_filetypes = { "markdown" }
      vim.g.mkdp_page_title = "${name}"
      vim.g.mkdp_markdown_css = vim.fn.stdpath("config") .. "/assets/markdown.css"
    end,
  },

  -- Table mode
  -- https://github.com/dhruvasagar/vim-table-mode
  {
    "dhruvasagar/vim-table-mode",
    ft = { "markdown", "norg", "org" },
    config = function()
      -- Bug: changing prefix doesn't work https://github.com/dhruvasagar/vim-table-mode/issues/222
      -- :h table-mode-mappings
      -- For Markdown-compatible tables use
      vim.g.table_mode_map_prefix = "<localleader>t"
      vim.g.table_mode_corner = "+"
      vim.g.table_mode_header_fillchar = '='
      -- vim.cmd("let g:table_mode_map_prefix = '<localleader>t'")
      -- vim.g.toggle_mode_options_toggle_map = "<localleader>tm"
      -- vim.g.table_mode_commands_tableize = "<localleader>tt"
    end,
  },

  -- -- [[ notes ]]
  {
    "nvim-neorg/neorg",
    build = ":Neorg sync-parsers",
    cmd = "Neorg",
    dependencies = {
      "nvim-lua/plenary.nvim",
      'jubnzv/mdeval.nvim',
      'nvim-neorg/neorg-telescope',
      "laher/neorg-exec",
      { "pysan3/neorg-templates", dependencies = { "L3MON4D3/LuaSnip" } },
    },
    config = function()
      require("telescope").load_extension("neorg")
      require("neorg").setup {
        load = {
          ["external.templates"] = {
            config = {
              templates_dir = vim.fn.stdpath("config") .. "/templates/norg",
            },
          },
          ["core.defaults"] = {}, -- Loads default behaviour
          ["core.neorgcmd"] = {},
          ["core.completion"] = { config = { engine = "nvim-cmp", name = "[Norg]" } },
          ["core.looking-glass"] = {},
          ["core.integrations.telescope"] = {},
          ["core.keybinds"] = {
            config = {
              -- neorg_leader = "<leader><leader>",
              hook = function(keybinds)
                keybinds.map("norg", "n", "ngj", "<Cmd>Neorg journal today<CR>")
                keybinds.map("norg", "n", "njo", "<Cmd>Neorg journal toc open<CR>")
                keybinds.map("norg", "n", "njO", "<Cmd>Neorg journal toc update<CR>")
                keybinds.map("norg", "n", "np", "<Cmd>Neorg presenter start<CR>")
                keybinds.map("norg", "n", "nt", "<Cmd>Neorg tangle current-file<CR>")
                keybinds.map("norg", "n", "nwn", "<Cmd>Neorg workspace notes<CR>")
                keybinds.map("norg", "n", "nww", "<Cmd>Neorg workspace work<CR>")
                keybinds.remap_event("norg", "i", "<C-f>", "core.integrations.telescope.insert_link")
                keybinds.remap_event("norg", "n", "ge", "core.looking-glass.magnify-code-block")
                keybinds.remap_event("norg", "n", "<C-s>", "core.integrations.telescope.find_linkable")
              end,
            },
          },
          ["core.concealer"] = {
            config = {
              folds = true,
              icon_present = "varided",
              icons = {
                heading = {
                  icons = { "⁖", "⁖", "⁖", "⁖", "⁖", "⁖", },
                },
                todo = {
                  cancelled = {
                    icon = "_",
                  },
                  done = {
                    icon = "x",
                  },
                  on_hold = {
                    icon = "-",
                  },
                  recurring = {
                    icon = "+",
                  },
                  uncertain = {
                    icon = "?",
                  },
                  pending = {
                    icon = "-",
                  },
                  undone = {
                    icon = " ",
                  },
                  urgent = {
                    icon = "!",
                  },
                },
              },
            },
          }, -- Adds pretty icons to your documents
          ["core.highlights"] = {
            config = {
            } -- highlights here
          },
          ["core.tempus"] = {},
          ["core.ui.calendar"] = {},
          ["core.export.markdown"] = {},
          ["core.export"] = {},
          ["core.manoeuvre"] = {},
          ["core.summary"] = {},
          ["core.syntax"] = {},
          ["core.promo"] = {},
          ["core.journal"] = {
            config = {
              strategy = "nested",
            },
          },
          ["core.ui"] = {},
          ["core.tangle"] = {},
          ["core.presenter"] = { config = { zen_mode = "zen-mode" } },
          ["core.queries.native"] = {},
          ["core.qol.todo_items"] = {
            config = {
              create_todo_parents = true,
            },
          },
          ["core.qol.toc"] = {
            config = {
              close_after_use = true,
            },
          },
          ["core.esupports.metagen"] = { config = { type = "empty", update_date = true } },
          ["core.esupports.hop"] = {},
          ["core.dirman"] = { -- Manages Neorg workspaces
            config = {
              workspaces = {
                notes = "~/notes",
                work = "~/notes/work",
              },
              default_workspace = "work",
            },
          },
          ["external.exec"] = {},
        },
      }
      vim.api.nvim_set_hl(0, "@neorg.tags.ranged_verbatim.code_block", { bg = "#333333" })
    end,
  },

  {
    'jakewvincent/mkdnflow.nvim',
    rocks = 'luautf8', -- Ensures optional luautf8 dependency is installed
    dependencies = { 'ekickx/clipboard-image.nvim',
      "mzlogin/vim-markdown-toc" },
    config = function()
      require('mkdnflow').setup({
        links = {
          style = 'markdown',
          name_is_source = false,
          conceal = false,
          context = 0,
          implicit_extension = nil,
          transform_implicit = false,
          transform_explicit = function(text)
            text = text:gsub(" ", "-")
            text = text:lower()
            text = os.date('%Y-%m-%d_') .. text
            return (text)
          end
        },
        tables = {
          trim_whitespace = true,
          format_on_move = true,
          auto_extend_rows = false,
          auto_extend_cols = false,
        },
        mappings = {
          -- MkdnFollowLink = { { 'n', 'v' }, '<leader>fl' }, -- see MkdnEnter
          MkdnToggleToDo = { { 'n', 'v' }, '<Space>to' },
          MkdnNewListItem = { { 'n', 'v' }, '<leader>li' },
        },
      })
    end,
  },

  -- [[ Zen mode ]]
  {
    'folke/zen-mode.nvim',
    dependencies = { 'folke/twilight.nvim' },
    config = function()
      require("zen-mode").setup({
        window = {
          width = .75, -- width will be 85% of the editor width
          options = {
            number = false,
            relativenumber = false,
          },
        }
      })
    end,
  },

  {
    'akinsho/toggleterm.nvim',
    version = "*",
    config = function()
      local status_ok, toggleterm = pcall(require, "toggleterm")
      if not status_ok then
        return
      end

      toggleterm.setup({
        -- size = 25,
        size = function(term)
          if term.direction == "horizontal" then
            return 17
          elseif term.direction == "vertical" then
            return vim.o.columns * 0.4
          end
        end,
        open_mapping = [[<C-\>]],
        hide_numbers = true,
        shade_filetypes = {},
        shade_terminals = true,
        shading_factor = 2,
        start_in_insert = true,
        insert_mappings = false,
        persist_size = true,
        direction = "float",
        -- direction = "horizontal",
        close_on_exit = true,
        shell = vim.o.shell,
        float_opts = {
          width = math.min(math.ceil(vim.fn.winwidth(0) * 0.8), 120),
          height = math.min(math.ceil(vim.fn.winheight(0) * 0.8), 28),
          border = "curved",
          winblend = 0,
          highlights = {
            border = "Normal",
            background = "Normal",
          },
        },
      })

      local opts = { height = math.floor(vim.fn.winheight(0) * 0.85) }

      local Terminal = require("toggleterm.terminal").Terminal
      local lazygit = Terminal:new({ cmd = "lazygit", hidden = true, float_opts = opts })

      -- :Lazygit
      vim.api.nvim_create_user_command("LazyGit", function()
        lazygit:toggle()
      end, {})


      vim.api.nvim_set_keymap("n", "<leader>lg", ":LazyGit<cr>", { noremap = true, silent = true })

      function _G.set_terminal_keymaps()
        local opts = { buffer = 0 }
        vim.keymap.set("t", "<esc>", [[<C-\><C-n>]], opts)
        vim.keymap.set("t", "jk", [[<C-\><C-n>]], opts)
        vim.keymap.set("t", "<C-h>", [[<Cmd>wincmd h<CR>]], opts)
        vim.keymap.set("t", "<C-j>", [[<Cmd>wincmd j<CR>]], opts)
        vim.keymap.set("t", "<C-k>", [[<Cmd>wincmd k<CR>]], opts)
        vim.keymap.set("t", "<C-;>", [[<Cmd>wincmd l<CR>]], opts)
      end

      -- if you only want these mappings for toggle term use term://*toggleterm#* instead
      -- vim.cmd("autocmd! TermOpen term://* lua set_terminal_keymaps()")
      vim.cmd("autocmd! TermOpen term://*toggleterm#* lua set_terminal_keymaps()")
    end,
  },

  -- {
  --   "nvim-tree/nvim-tree.lua",
  --   lazy = true,
  --   version = "*",
  --   dependencies = {
  --     "nvim-tree/nvim-web-devicons",
  --   },
  --   config = function()
  --     require("nvim-tree").setup {}
  --   end,
  -- },

  {
    "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
    config = function()
      require("lsp_lines").setup()
    end,
  },

  -- {
  --   'Exafunction/codeium.vim',
  --   ft = { "python", "lua", "rust" },
  --   config = function()
  --     -- Change '<C-g>' here to any keycode you like.
  --     vim.keymap.set('i', '<c-a>', function() return vim.fn['codeium#Accept']() end, { expr = true })
  --     vim.keymap.set('i', '<c-.>', function() return vim.fn['codeium#CycleCompletions'](1) end, { expr = true })
  --     vim.keymap.set('i', '<c-,>', function() return vim.fn['codeium#CycleCompletions'](-1) end, { expr = true })
  --     vim.keymap.set('i', '<c-x>', function() return vim.fn['codeium#Clear']() end, { expr = true })
  --   end
  -- },

  {
    "michaelb/sniprun",
    build = "sh ./install.sh"
  },

  -- [[ Rust Baby!! ]]
  {
    'simrat39/rust-tools.nvim',
    ft = { "rust" },
    config = function()
      local rt = require("rust-tools")
      local mason_registry = require("mason-registry")

      local codelldb = mason_registry.get_package("codelldb")
      local extension_path = codelldb:get_install_path() .. "/extension/"
      local codelldb_path = extension_path .. "adapter/codelldb"
      local liblldb_path = extension_path .. "lldb/lib/liblldb.dylib"

      rt.setup({
        dap = {
          adapter = require("rust-tools.dap").get_codelldb_adapter(codelldb_path, liblldb_path),
        },
        server = {
          capabilities = require("cmp_nvim_lsp").default_capabilities(),
          on_attach = function(_, bufnr)
            vim.keymap.set("n", "<leader>k", rt.hover_actions.hover_actions, { buffer = bufnr })
            vim.keymap.set("n", "<leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
          end,
        },
        tools = {
          hover_actions = {
            auto_focus = true,
          },
        },
      })
    end
  },

  {
    'rust-lang/rust.vim',
    ft = "rust",
    init = function()
      vim.g.rustfmt_autosave = 1
    end
  },

  --
}
