local maps = {
  { "<leader>sf", "<Cmd>Telescope current_buffer_fuzzy_find<Cr>", mode = { "n", "v" } },
  { "<leader>so", "<Cmd>Telescope find_files<Cr>", mode = { "n", "v" } },
  { "<leader>sr", "<Cmd>Telescope oldfiles<Cr>", mode = { "n", "v" } },
  { "<leader>sl", "<Cmd>Telescope live_grep<Cr>", mode = { "n", "v" } },
  { "<leader>sb", "<Cmd>Telescope buffers<Cr>", mode = { "n", "v" } },
  { "<leader>sm", "<Cmd>Telescope marks<Cr>", mode = { "n", "v" } },
  { "<leader>sh", "<Cmd>Telescope help_tags<Cr>", mode = { "n", "v" } },
  { "<leader>sF", "<Cmd>Telescope filetypes<Cr>", mode = { "n", "v" } },
  { "<leader>sk", "<Cmd>Telescope keymaps<Cr>", mode = { "n", "v" } },
  { "<leader>sc", "<Cmd>Telescope commands<Cr>", mode = { "n", "v" } },
  { "<leader>sgf", "<Cmd>Telescope git_files<Cr>", mode = { "n", "v" } },
  { "<leader>sgs", "<Cmd>Telescope git_status<Cr>", mode = { "n", "v" } },
  { "<leader>sgc", "<Cmd>Telescope git_commits<Cr>", mode = { "n", "v" } },
  { "<leader>sgb", "<Cmd>Telescope git_branches<Cr>", mode = { "n", "v" } },
}

local opts = {
  defaults = {
    file_ignore_patterns = {
      ".git/",
      "node_modules/",
    },
    mappings = {
      i = {
        ["<M-j>"] = "move_selection_next",
        ["<M-k>"] = "move_selection_previous",
        ["<M-q>"] = "close",
      },
      n = {
        ["<M-j>"] = "move_selection_next",
        ["<M-k>"] = "move_selection_previous",
        ["<M-q>"] = "close",
      },
    },
  },
  pickers = {
    find_files = {
      hidden = true,
    },
  },
}

return {
  "nvim-telescope/telescope.nvim",
  tag = "0.1.4",
  dependencies = { "nvim-lua/plenary.nvim" },
  config = function ()
    require("telescope").setup(opts)
  end,
  keys = maps,
}
