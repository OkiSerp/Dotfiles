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

bind(nv, "<leader>oe", vim.cmd.Ex, opts)

vim.opt.hidden = true

bind(nvi, "<M-q>", vim.cmd.bdelete, opts)
bind(nvi, "<M-s>", vim.cmd.write, { noremap = true })

bind(nvi, "<M-j>", vim.cmd.bnext, opts)
bind(nvi, "<M-k>", vim.cmd.bprevious, opts)

local utils = require("utils")

bind(nv, "<leader>tw", utils.ToggleWrap, opts)
bind(nv, "<leader>tl", utils.ToggleNumbers, opts)
