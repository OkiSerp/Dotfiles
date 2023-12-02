vim.opt.clipboard:append("unnamedplus")

vim.opt.guicursor = ""
vim.opt.mouse:append("a")

vim.opt.signcolumn = "yes"
vim.opt.cursorline = true

vim.opt.scrollback = 8000
vim.opt.scrolljump = 1
vim.opt.scrolloff = 9

vim.wo.number = true
vim.wo.relativenumber = true

vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2

vim.opt.smartindent = true
vim.opt.autoindent = true
vim.opt.expandtab = true
vim.opt.smarttab = true

vim.opt.breakindent = true
vim.opt.wrap = false

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.autochdir = true

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.writebackup = false

vim.opt.undodir = vim.fn.expand("~/.cache/nvim/undodir")
vim.opt.undofile = true
