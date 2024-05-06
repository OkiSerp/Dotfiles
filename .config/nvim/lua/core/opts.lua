local srp = {
    undodir = "~/.cache/nvim/srp/undodir",
    tabwidth = 2,
}

vim.g.mapleader = " "
vim.g.maplocalleader = " m"

vim.opt.undodir = vim.fn.expand(srp.undodir)
vim.opt.undofile = true

vim.opt.clipboard:append("unnamedplus")

vim.opt.hidden = true

vim.opt.cursorline = true
vim.opt.guicursor = ""

vim.wo.number = true
vim.wo.relativenumber = true

-- vim.opt.scroll = 15
-- vim.opt.scrolloff = 8
-- vim.opt.sidescrolloff = 23

vim.opt.softtabstop = srp.tabwidth
vim.opt.shiftwidth = srp.tabwidth
vim.opt.tabstop = srp.tabwidth

vim.opt.smartindent = true
vim.opt.autoindent = true
vim.opt.expandtab = true
vim.opt.smarttab = true

vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.breakindent = true
vim.opt.wrap = false

vim.opt.spell = false

vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.swapfile = false
