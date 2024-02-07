local plug = {
    "NeogitOrg/neogit",
    dependencies = {
        "nvim-lua/plenary.nvim",
        "sindrets/diffview.nvim",
        "nvim-telescope/telescope.nvim",
    },
    config = true,
    lazy = false,
}

plug.keys = {
    { "<leader>gg", vim.cmd.Neogit, mode = { "n", "v" } },
}

return plug
