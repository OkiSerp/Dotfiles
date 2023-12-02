vim.g.mapleader = " "
vim.g.maplocalleader = " m"

local n, v = "n", "v"
local c, i = "c", "i"

local nv = { "n", "v" }
local nvi = { "n", "v", "i" }

local silent = { silent = true }
local opts = { noremap = true, silent = true }

local bind = vim.keymap.set

bind(nv, "<leader>qq", vim.cmd.xall, opts)
bind(nv, "<leader>qQ", ":qall!<Cr>", opts)
