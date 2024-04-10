local plug =  {
    "nvim-telescope/telescope.nvim",
    tag = "0.1.5",
}

plug.keys = {
    { "<leader>sf", ":Telescope current_buffer_fuzzy_find<Cr>", mode = { "n", "v" } },
    { "<leader>so", ":Telescope find_files<Cr>", mode = { "n", "v" } },
    { "<leader>sr", ":Telescope oldfiles<Cr>", mode = { "n", "v" } },
    { "<leader>sl", ":Telescope live_grep<Cr>", mode = { "n", "v" } },
    { "<leader>sb", ":Telescope buffers<Cr>", mode = { "n", "v" } },
    { "<leader>sh", ":Telescope help_tags<Cr>", mode = { "n", "v" } },
}

plug.opts = {
    defaults = {
        file_ignore_patterns = {
            ".git/",
            "node_modules/",
        },
        mappings = {
            i = {
                ["<M-j>"] = "move_selection_next",
                ["<M-k>"] = "move_selection_previous",
                ["<M-e>"] = "close",
            },
            n = {
                ["<M-j>"] = "move_selection_next",
                ["<M-k>"] = "move_selection_previous",
                ["<M-e>"] = "close",
            },
        },
    },
    pickers = {
        find_files = {
            hidden = true,
        },
    },
}

plug.dependencies = {
    "nvim-lua/plenary.nvim"
}

return plug
