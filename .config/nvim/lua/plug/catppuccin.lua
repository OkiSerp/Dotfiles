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
    mason = true,
    neogit = true,
    gitsigns = true,
}

local opts = {
    flavour = "mocha",
    background = {
        light = "latte",
        dark = "mocha",
    },
    transparent_background = true,
    show_end_of_buffer = true,
    term_colors = true,
    dim_inactive = {
        enabled = false,
        shade = "dark",
        percentage = 0.15,
    },
    no_italic = false,
    no_bold = false,
    no_underline = false,
    styles = {
        comments = {},
        conditionals = {},
        loops = {},
        functions = {},
        keywords = {},
        strings = {},
        variables = {},
        numbers = {},
        booleans = {},
        properties = {},
        types = {},
        operators = {},
    },
    color_overrides = {},
    custom_highlights = {},
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
