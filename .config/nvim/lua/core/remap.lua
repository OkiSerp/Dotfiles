vim.keymap.set({ "n", "v", "i" }, "<M-q>", vim.cmd.xall)
vim.keymap.set({ "n", "v", "i" }, "<M-Q>", function()
    vim.cmd(":qall!")
end)

vim.keymap.set({ "n", "v", "i" }, "<M-s>", vim.cmd.write)

vim.keymap.set({ "n", "v", "i" }, "<M-e>", vim.cmd.bdelete)

vim.keymap.set({ "n", "v", "i" }, "<M-p>", vim.cmd.bprevious)
vim.keymap.set({ "n", "v", "i" }, "<M-n>", vim.cmd.bnext)

vim.keymap.set("c", "<M-p>", "<Up>")
vim.keymap.set("c", "<M-n>", "<Down>")

vim.keymap.set("i", "<M-Cr>", "<C-o>O")
vim.keymap.set("i", "<C-c>", "<Esc>")

vim.keymap.set({ "n", "v" }, "'", function()
    vim.cmd.normal("%")
end, { noremap = true })

vim.keymap.set("x", "<M-i>", [["_dP]])

vim.keymap.set("v", "J", ":move '>+1<Cr>gv=gv", { silent = true })
vim.keymap.set("v", "K", ":move '<-2<Cr>gv=gv", { silent = true })

vim.keymap.set("n", "<M-r>", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

vim.keymap.set("n", "J", "mzJ`z")

vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

vim.keymap.set("n", "G", "Gzz")

vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "<C-d>", "<C-d>zz")

vim.keymap.set("n", "x", "\"_x")
vim.keymap.set("n", "X", "\"_X")
