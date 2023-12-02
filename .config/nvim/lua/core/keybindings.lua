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

bind(v, "<", "<gv")
bind(v, ">", ">gv")

bind(v, "J", ":move '>+1<CR>gv=gv", silent)
bind(v, "K", ":move '<-2<CR>gv=gv", silent)

bind(n, "n", "nzzzv")
bind(n, "N", "Nzzzv")

bind(n, "J", "mzJ`z")

bind(nv, "'", "%")

bind(n, "x", [["_x]])
bind(n, "X", [["_X]])

bind(nv, "Q", "<Nop>")
bind(nv, "<Space>", "<Nop>")

bind(i, "<M-Cr>", "<C-o>O")

bind(i, "<M-v>", "<C-o>P")

bind(n, "<M-x>", ":")
