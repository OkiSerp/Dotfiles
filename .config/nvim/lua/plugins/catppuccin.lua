local plugin = {
  "catppuccin/nvim",
  name = "catppuccin",
  lazy = false,
  priority = 999,
  init = function()
    vim.opt.background = "dark"
    vim.opt.termguicolors = true
  end,
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
  integrations = {
    which_key = true,
    telescope = {
      enabled = true,
    },
    treesitter = true,
    indent_blankline = {
      enabled = true,
      scope_color = "lavender",
      colored_indent_levels = false,
    },
    noice = true,
    mini = {
      enabled = true,
    },
  },
}

plugin.config = function()
  require("catppuccin").setup(opts)
  vim.cmd.colorscheme "catppuccin"
end

return plugin
