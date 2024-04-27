local plug = {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 999,
    lazy = false,
}

local integ = {
    treesitter = true,
    vim_sneak = true,
    telescope = {
        enabled = true,
    },
}

local opts = {
    flavour = "mocha",
    transparent_background = true,
    show_end_of_buffer = true,
    term_colors = true,
    styles = {
        conditionals = {},
    },
    integrations = integ,
}

plug.init = function()
    vim.opt.background = "dark"
    vim.opt.termguicolors = true
end

plug.config = function()
    require("catppuccin").setup(opts)
    vim.cmd.colorscheme("catppuccin")
end

return plug
