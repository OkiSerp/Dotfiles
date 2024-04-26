if vim.fn.executable("fish") then
  vim.cmd("set shell=fish")
else
  vim.cmd("set shell=bash")
end

vim.g.mapleader = " "
vim.g.maplocalleader = " m"

vim.opt.clipboard:append("unnamedplus")

vim.opt.hidden = true

vim.wo.number = true
vim.wo.relativenumber = true

vim.opt.scroll = 15
vim.opt.scrolloff = 8
vim.opt.sidescrolloff = 21

vim.opt.guicursor = ""

vim.opt.signcolumn = "yes"
vim.opt.cursorline = true

vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2

vim.opt.smartindent = true
vim.opt.autoindent = true
vim.opt.expandtab = true
vim.opt.smarttab = true

vim.opt.breakindent = true
vim.opt.wrap = false

vim.opt.spell = false

vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.swapfile = false

vim.opt.undodir = vim.fn.expand("~/.cache/nvim/undodir")
vim.opt.undofile = true

vim.api.nvim_create_autocmd({ "BufWritePre" }, {
  pattern = { "*" },
  callback = function()
    local save_cursor = vim.fn.getpos(".")
    pcall(function() vim.cmd [[%s/\s\+$//e]] end)
    vim.fn.setpos(".", save_cursor)
  end,
})

vim.keymap.set("n", "-", vim.cmd.Explore)

vim.keymap.set({ "n", "v", "i" }, "<M-q>", vim.cmd.xall)
vim.keymap.set({ "n", "v", "i" }, "<M-Q>", function()
    vim.cmd(":qall!")
end)

vim.keymap.set({ "n", "v", "i" }, "<M-e>", vim.cmd.bdelete)

vim.keymap.set({ "n", "v", "i" }, "<M-s>", vim.cmd.write)

vim.keymap.set({ "n", "v", "i" }, "<M-k>", vim.cmd.bprevious)
vim.keymap.set({ "n", "v", "i" }, "<M-j>", vim.cmd.bnext)

vim.keymap.set("i", "<M-Cr>", "<C-o>O")

vim.keymap.set("v", "J", ":move '>+1<Cr>gv=gv", { silent = true })
vim.keymap.set("v", "K", ":move '<-2<Cr>gv=gv", { silent = true })

vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")

vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

vim.keymap.set("n", "G", "Gzz")

vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "<C-d>", "<C-d>zz")

vim.keymap.set("n", "J", "mzJ`z")

vim.keymap.set({ "n", "v" }, "'", "%")

vim.keymap.set("n", "x", [["_x"]])
vim.keymap.set("n", "X", [["_X"]])

vim.keymap.set("n", "Q", "<Nop>")

vim.keymap.set("c", "<M-k>", "<Up>")
vim.keymap.set("c", "<M-j>", "<Down>")

vim.keymap.set("n", "<leader>tw", function()
  if vim.opt.wrap:get() then
    vim.cmd("set nowrap")
    return
  end
  vim.cmd("set wrap")
end)

vim.keymap.set("n", "<leader>tl", function()
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
end)

vim.keymap.set("n", "<leader>ts", function()
  if vim.opt.spell:get() then
    vim.cmd("set nospell")
    return
  end
  vim.cmd("set spell")
end)
