vim.g.mapleader = " "
vim.g.maplocalleader = " m"

local n, v = "n", "v"
local c, i = "c", "i"

local nv = { "n", "v" }
local nvi = { "n", "v", "i" }

local silent = { silent = true }
local opts = { noremap = true, silent = true }

local bind = vim.keymap.set

bind(nv, "<leader>hs", vim.cmd.source)

bind(nv, "<leader>qq", vim.cmd.xall, opts)
bind(nv, "<leader>qQ", ":qall!<Cr>", opts)

bind(nv, "<leader>oe", vim.cmd.Ex, opts)

bind(nv, "<leader>;", ":", { noremap = true })

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

bind(c, "<M-j>", "<Down>")
bind(c, "<M-k>", "<Up>")

bind(n, "<leader>/", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

bind(nv, "<leader>wh", "<C-w>h", opts)
bind(nv, "<leader>wj", "<C-w>j", opts)
bind(nv, "<leader>wk", "<C-w>k", opts)
bind(nv, "<leader>wl", "<C-w>l", opts)

bind(nv, "<leader>ws", "<C-w>s", opts)
bind(nv, "<leader>wv", "<C-w>v", opts)

bind(nv, "<leader>w<M-o>", "<C-w>o", opts)
bind(nv, "<leader>wd", "<C-w>q", opts)

bind(nv, "<leader>w=", "<C-w>=", opts)
bind(nv, "<leader>w_", "<C-w>_", opts)
bind(nv, "<leader>w|", "<C-w>|", opts)

bind(nv, "<leader>w<", "<C-w><<C-w><", opts)
bind(nv, "<leader>w>", "<C-w>><C-w>>", opts)

bind(nv, "<leader>w+", "<C-w>+", opts)
bind(nv, "<leader>w-", "<C-w>-", opts)
