local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable",
        lazypath,
    })
end

vim.opt.rtp:prepend(lazypath)

require("core.opts")
require("core.keys")

require("autocmd")

local opts = {
    defaults = {
        lazy = true,
    },
    install = {
        colorscheme = { "catppuccin" },
    },
}

require("lazy").setup("plug", opts)
