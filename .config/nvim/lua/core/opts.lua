vim.wo.number = true
vim.wo.relativenumber = true

vim.opt.scrolloff = 8
vim.opt.sidescrolloff = 8

vim.opt.guicursor = ""

vim.opt.signcolumn = "yes"
vim.opt.cursorline = true

vim.opt.clipboard:append("unnamedplus")

vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4

vim.opt.smartindent = true
vim.opt.autoindent = true
vim.opt.expandtab = true
vim.opt.smarttab = true

vim.opt.breakindent = true
vim.opt.wrap = false

vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.swapfile = false

vim.opt.undodir = vim.fn.expand("~/.cache/nvim/undodir")
vim.opt.undofile = true
