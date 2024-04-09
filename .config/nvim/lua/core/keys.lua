local utils = require("utils")

local n, v, i = "n", "v", "i"
local nv, nvi = { n, v }, { n, v, i }

local sil = { silent = true }

local bind = vim.keymap.set

bind(n, "<leader>tw", utils.togglewrap)
bind(n, "<leader>tl", utils.togglenumber)
bind(n, "<leader>ts", utils.togglespell)

bind(n, "<leader>oe", vim.cmd.Explore)

bind(nvi, "<M-q>", vim.cmd.xall)
bind(nvi, "<M-Q>", function()
    vim.cmd(":qall!")
end)

bind(nvi, "<M-e>", vim.cmd.bdelete)

bind(nvi, "<M-s>", vim.cmd.write)

bind(nvi, "<M-k>", vim.cmd.bprevious)
bind(nvi, "<M-j>", vim.cmd.bnext)

bind(i, "<M-Cr>", "<C-o>O")

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
