local utils = require("utils")

local bind = utils.bind

local n, v, i = "n", "v", "i"
local nv, nvi = { n, v }, { n, v, i }

local sil = { silent = true }

bind(n, "<leader>tw", utils.togglewrap)
bind(n, "<leader>tl", utils.togglenumber)
bind(n, "<leader>ts", utils.togglespell)

bind(nvi, "<M-q>", ":xa<Cr>")
bind(nvi, "<M-Q>", ":qa!<Cr>")

bind(nvi, "<M-s>", ":w<Cr>")

bind(nv, "<leader>oe", ":Ex<Cr>")

bind(nvi, "<M-k>", ":bp<Cr>")
bind(nvi, "<M-j>", ":bn<Cr>")

bind(v, "J", ":move '>+1<Cr>gv=gv", sil)
bind(v, "K", ":move '<-2<Cr>gv=gv", sil)

bind(v, "<", "<gv")
bind(v, ">", ">gv")

bind(n, "n", "nzzzv")
bind(n, "N", "Nzzzv")

bind(n, "G", "Gzz")

bind(n, "J", "mzJ`z")

bind(nv, "'", "%")

bind(n, "x", "\"_x")
bind(n, "X", "\"_X")

bind(n, "Q", "<Nop>")
