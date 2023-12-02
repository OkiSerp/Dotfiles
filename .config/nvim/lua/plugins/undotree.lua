return {
  "mbbill/undotree",
  event = { "BufReadPre", "BufNewFile" },
  keys = {
    { "<leader>tu", "<Cmd>UndotreeToggle<Cr>", mode = { "n", "v" } },
  },
}
