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

require("core")

local opts = {
  defaults = {
    lazy = true,
  },
  install = {
    missing = true,
    colorscheme = { "catppuccin" },
  },
  change_detection = {
    enabled = true,
    notify = false,
  },
  checker = {
    enabled = true,
    notify = false,
    frequency = 3600 * 24 * 7,
  },
  disabled_plugins = {
    "gzip",
    "matchit",
    "matchparen",
    "netrwPlugin",
    "tarPlugin",
    "tohtml",
    "tutor",
    "zipPlugin",
  },
}

require("lazy").setup({{ import = "plugins" }}, opts)
