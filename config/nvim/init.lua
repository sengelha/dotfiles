-- init.lua: My NeoVIM config
--
-- Based on https://github.com/nvim-lua/kickstart.nvim

-- Set <space> as the leaer key
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- We always use Nerd Fonts
vim.g.have_nerd_font = true

-- Enable line numbers
vim.o.number = true

-- Dont' show the mode, since it's already in the status line
vim.o.showmode = false

-- Sync clipboard between OS and Neovim
vim.schedule(function() vim.o.clipboard = 'unnamedplus' end)

-- Make wrapped text visually indented to match the indentation of the original line
vim.o.breakindent = true

-- Enable undo/redo changes even after closing and reopening a file
vim.o.undofile = true

-- Case-insensitive searching unless \C or one or more capital letters in search term
vim.o.ignorecase = true
vim.o.smartcase = true

-- Add gutter on far left for displaying signs likeke LSP diagnostics or Git changes
vim.o.signcolumn = 'yes'

-- Reduce times to make nvim more responsive
vim.o.updatetime = 250
vim.o.timeoutlen = 300

-- Show some whitespace chars
vim.o.list = true
vim.opt.listchars = { tab = '» ', trail = '·', nbsp = '␣' }

-- Preview substitutions live, as you type
vim.o.inccommand = 'split'

-- Show which line the cursor is on
vim.o.cursorline = true

-- Minimal number of screen lines to keep above and below the cursor
vim.o.scrolloff = 10

-- Require confirmation for operations that would fail due to unsaved changes in
-- the buffer
vim.o.confirm = true

-- Clear highlights on search when pressing <Esc> in normal mode
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

-- Diagnostic config & keymaps
vim.diagnostic.config {
  update_in_insert = false,
  severity_sort = true,
  float = { border = 'rounded', source = 'if_many' },
  underline = { severity = { min = vim.diagnostic.severity.WARN } },
  virtual_text = true,
  virtual_lines = false,
  jump = { float = true },
}
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostic [Q]uickfix list' })

-- Disable arrow keys in normal mode
vim.keymap.set('n', '<left>', '<cmd>echo "Use h to move!!"<CR>')
vim.keymap.set('n', '<right>', '<cmd>echo "Use l to move!!"<CR>')
vim.keymap.set('n', '<up>', '<cmd>echo "Use k to move!!"<CR>')
vim.keymap.set('n', '<down>', '<cmd>echo "Use j to move!!"<CR>')

-- Highlight when yanking (copying) text
vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking (copying) text',
  group = vim.api.nvim_create_augroup('my-highlight-yank', { clear = true }),
  callback = function() vim.hl.on_yank() end,
})

-- Install 'lazy.nvim' plugin manager
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = 'https://github.com/folke/lazy.nvim.git'
  local out = vim.fn.system { 'git', 'clone', '--filter=blob:none', '--branch=stable', lazyrepo, lazypath }
  if vim.v.shell_error ~= 0 then error('Error cloning lazy:nvim:\n' .. out) end
end

vim.opt.rtp:prepend(lazypath)

-- Configure and install plugins
require('lazy').setup({
  -- TODO: add nvim-lspconfig
  -- TODO: add telescope
  -- TODO: Add oil for filesystem editing?
  -- TODO: Add nvim-treesitter
  -- TODO: Add nvim-cmp (completion engine)
  -- TODO: Add conform (code formatting)
  -- TODO: Add org-mode support

  -- Add git related signs to the gutter
  {
    'lewis6991/gitsigns.nvim',
    opts = {
      signs = {
        add = { text = '+' },
        change = { text = '~' },
        delete = { text = '_' },
        topdelete = { text = '‾' },
        changedelete = { text = '~' },
      },
    },
  },

  -- Show pending keybinds
  {
    'folke/which-key.nvim',
    event = 'VimEnter',
    opts = {
      delay = 0,
      icons = { mappings = vim.g.have_herd_font },
      spec = {
        { '<leader>s', group = '[S]earch', mode = { 'n', 'v' } },
        { '<leader>t', group = '[T]oggle' },
        { '<leader>h', group = 'Git [H]unk', mode = { 'n', 'v' } },
        { 'gr', group = 'LSP Actions', mode = { 'n' } },
      },
    },
  },

  -- Tokyo Night color scheme
  {
    'folke/tokyonight.nvim',
    priority = 1000, -- Load before all other
    config = function()
      require('tokyonight').setup {
        styles = {
          comments = { italic = false }, -- Disable italics in comments
        },
      }
      vim.cmd.colorscheme 'tokyonight-night'
    end,
  },

  -- Highlight todo, notes, etc. in comments
  {
    'folke/todo-comments.nvim',
    event = 'VimEnter',
    dependencies = { 'nvim-lua/plenary.nvim' },
    opts = { signs = false },
  },

  -- Collection of various mall independent plugins/modules
  {
    'nvim-mini/mini.nvim',
    config = function()
      -- Better around/inside textobjects
      require('mini.ai').setup { n_lines = 500 }
      -- Add/delete/replace surroundings
      require('mini.surround').setup()
    end,
  },

  -- Lualine status line
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      require('lualine').setup({
        options = {
          theme = 'auto',
        },
      })
    end,
  },

  -- org-mode support
  {
    'nvim-orgmode/orgmode',
    event = 'VeryLazy',
    ft = { 'org' },
    config = function()
      require('orgmode').setup({
      })
      vim.lsp.enable('org')
    end,
  },

}, { ---@diagnostic disable-line: missing-fields
  ui = {
    icons = vim.g.have_nerd_font and {} or {
      cmd = '⌘',
      config = '🛠',
      event = '📅',
      ft = '📂',
      init = '⚙',
      keys = '🗝',
      plugin = '🔌',
      runtime = '💻',
      require = '🌙',
      source = '📄',
      start = '🚀',
      task = '📌',
      lazy = '💤 ',
    },
  },
})

-- vim: ts=2 sts=2 sw=2 et
