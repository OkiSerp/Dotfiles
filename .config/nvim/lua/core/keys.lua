vim.g.mapleader = " "
vim.g.maplocalleader = " m"

local n, nv = "n", { "n", "v" }
local nvi = { "n", "v", "i" }
local sil = { silent = true }

local bind = vim.keymap.set

bind(nvi, "<M-q>", ":xa<Cr>")
bind(nvi, "<M-Q>", ":qa!<Cr>")

bind(nvi, "<M-s>", ":w<Cr>")

bind(nv, "<leader>oe", ":Ex<Cr>")

vim.opt.hidden = true

bind(nvi, "<M-k>", ":bp<Cr>")
bind(nvi, "<M-j>", ":bn<Cr>")

local util = require("util")

bind(n, "<leader>tw", util.togglewrap)
bind(n, "<leader>tl", util.togglenumber)

vim.keymap.set(
    "n",
    "<leader>cw",
    [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<left><left><left>]]
)

bind("v", "J", ":move '>+1<Cr>gv=gv", sil)
bind("v", "K", ":move '<-2<Cr>gv=gv", sil)

bind("v", "<", "<gv")
bind("v", ">", ">gv")

bind("n", "n", "nzzzv")
bind("n", "N", "Nzzzv")

bind("n", "G", "Gzz")

bind("n", "J", "mzJ`z")

bind({ "n", "v" }, "'", "%")

bind("n", "x", "\"_x")
bind("n", "X", "\"_X")

bind("n", "Q", "<Nop>")
