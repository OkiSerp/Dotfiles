local plugin = {
  "folke/noice.nvim",
  event = "VeryLazy",
  dependencies = {
    "MunifTanjim/nui.nvim",
    "rcarriga/nvim-notify",
  },
}

plugin.opts = {
  cmdline = {
    enabled = true,
    view = "cmdline",
    opts = {},
    format = {
      cmdline = { pattern = "^:", icon = ">_", lang = "vim" },
      search_down = {
        kind = "search", pattern = "^/", icon = "?", lang = "regex"
      },
      search_up = {
        kind = "search", pattern = "^%?", icon = "?", lang = "regex"
      },
      filter = { pattern = "^:%s*!", icon = "$", lang = "bash" },
      lua = {
        pattern = {
          "^:%s*lua%s+", "^:%s*lua%s*=%s*", "^:%s*=%s*"
        },
        icon = "_",
        lang = "lua",
      },
      help = { pattern = "^:%s*he?l?p?%s+", icon = "_" },
      input = {},
    },
  },
  messages = {
    enabled = true,
    view = "mini",
    view_error = "mini",
    view_warn = "mini",
    view_history = "messages",
    view_search = "virtualtext",
  },
  lsp = {
    progress = {
      enabled = true,
      format = "lsp_progress",
      format_done = "lsp_progress_done",
      throttle = 1000 / 30,
      view = "mini",
    },
    message = {
      enabled = true,
      view = "mini",
      opts = {},
    },
  },
}

return plugin
