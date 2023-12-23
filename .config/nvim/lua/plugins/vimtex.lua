return {
  "lervag/vimtex",
  event = { "BufReadPre", "BufNewFile" },
  keys = {
    { "<leader>lc", "<Cmd>VimtexCompile<Cr>", mode = "n" },
    { "<leader>lv", "<Cmd>VimtexView<Cr>", mode = "n" },
  },
}
