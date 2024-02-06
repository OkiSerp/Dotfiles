-- To launch neovim without plugins: 
-- nvim -u path/to/noplug.lua

vim.g.mapleader = " "
vim.g.maplocalleader = " m"

vim.opt.hidden = true

vim.opt.clipboard:append("unnamedplus")

vim.wo.number = true
vim.wo.relativenumber = true

vim.opt.scroll = 15
vim.opt.scrolloff = 8
vim.opt.sidescrolloff = 12

vim.opt.guicursor = ""

vim.opt.signcolumn = "no"
vim.opt.cursorline = true

vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4

vim.opt.smartindent = true
vim.opt.autoindent = true
vim.opt.expandtab = true
vim.opt.smarttab = true

vim.opt.breakindent = true
vim.opt.wrap = false

vim.opt.spell = true

vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.swapfile = false

vim.opt.undodir = vim.fn.expand("~/.cache/nvim/undodir")
vim.opt.undofile = true

local n, v, i = "n", "v", "i"
local nv, nvi = { n, v }, { n, v, i }

local sil = { silent = true }

local bind = vim.keymap.set

bind(n, "<leader>oe", vim.cmd.Explore)

bind(nvi, "<M-q>", vim.cmd.xall)
bind(nvi, "<M-Q>", function()
    vim.cmd(":qall!")
end)

bind(nvi, "<M-s>", vim.cmd.write)

bind(nvi, "<M-k>", vim.cmd.bprevious)
bind(nvi, "<M-j>", vim.cmd.bnext)

bind(nvi, "<M-B>", vim.cmd.bdelete)

bind(v, "J", ":move '>+1<Cr>gv=gv", sil)
bind(v, "K", ":move '<-2<Cr>gv=gv", sil)

bind(v, "<", "<gv")
bind(v, ">", ">gv")

bind(n, "n", "nzzzv")
bind(n, "N", "Nzzzv")

bind(n, "G", "Gzz")

bind(n, "<C-u>", "<C-u>zz")
bind(n, "<C-d>", "<C-d>zz")

bind(n, "J", "mzJ`z")

bind(nv, "'", "%")

bind(n, "x", "\"_x")
bind(n, "X", "\"_X")

bind(n, "Q", "<Nop>")

local utils = {}

utils.togglewrap = function()
    if vim.opt.wrap:get() then
        vim.cmd("set nowrap")
        return
    end
    vim.cmd("set wrap")
end

utils.togglenumber = function()
    if vim.opt.rnu:get() and vim.opt.nu:get() then
        vim.opt.nu = false
        vim.opt.rnu = false
    elseif not vim.opt.rnu:get() and vim.opt.nu:get() then
        vim.opt.nu = true
        vim.opt.rnu = true
    else
        vim.opt.nu = true
        vim.opt.rnu = false
    end
end

utils.togglespell = function()
    if vim.opt.spell:get() then
        vim.cmd("set nospell")
        return
    end
    vim.cmd("set spell")
end

vim.keymap.set("n", "<leader>tw", utils.togglewrap)
vim.keymap.set("n", "<leader>tn", utils.togglenumber)
vim.keymap.set("n", "<leader>ts", utils.togglespell)
