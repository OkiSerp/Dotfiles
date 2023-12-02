return {
  "windwp/nvim-ts-autotag",
  lazy = false,
  config = {},
  event = { "BufReadPre", "BufNewFile" },
  dependencies = {
    "nvim-treesitter/nvim-treesitter"
  },
}
