vim.g.mapleader = " "
vim.g.maplocalleader = " m"

vim.keymap.set("n", "<leader>qq", ":xall<Cr>")
vim.keymap.set("n", "<leader>qQ", ":qall!<Cr>")

vim.keymap.set("n", "<leader>fs", ":write<Cr>")

vim.keymap.set("n", "<leader>oe", ":Explore<Cr>")

vim.opt.hidden = true

vim.keymap.set("n", "<leader>bp", ":bprevious<Cr>")
vim.keymap.set("n", "<leader>bn", ":bnext<Cr>")

vim.keymap.set("n", "<leader>bk", ":bdelete<Cr>")

local util = require("util")

vim.keymap.set("n", "<leader>tw", util.togglewrap)

vim.keymap.set("n", "<leader>tn", util.togglenumber)

vim.keymap.set(
    "n",
    "<leader>cw",
    [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<left><left><left>]]
)

vim.keymap.set("v", "J", ":move '>+1<Cr>gv=gv", { silent = true })
vim.keymap.set("v", "K", ":move '<-2<Cr>gv=gv", { silent = true })

vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")

vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

vim.keymap.set("n", "G", "Gzz")

vim.keymap.set("n", "J", "mzJ`z")

vim.keymap.set({ "n", "v" }, "'", "%")

vim.keymap.set("n", "x", "\"_x")
vim.keymap.set("n", "X", "\"_X")

vim.keymap.set("n", "Q", "<Nop>")
