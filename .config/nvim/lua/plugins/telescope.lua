local plug =  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    tag = "0.1.6",
}

plug.keys = {
    { "<leader>sf", ":Telescope current_buffer_fuzzy_find<Cr>", mode = "n", desc = "Search current buffer" },
    { "<leader>so", ":Telescope find_files<Cr>", mode = "n", desc = "Search working directory" },
    { "<leader>sr", ":Telescope oldfiles<Cr>", mode = "n", desc = "Search for recent files" },
    { "<leader>sb", ":Telescope buffers<Cr>", mode = "n", desc = "Search for opened buffers" },
    { "<leader>sh", ":Telescope help_tags<Cr>", mode = "n", desc = "Search help tags" },
}

plug.opts = {
    defaults = {
        file_ignore_patterns = {
            ".git/",
            "node_modules/",
        },
        mappings = {
            i = {
                ["<M-p>"] = "move_selection_previous",
                ["<M-n>"] = "move_selection_next",
                ["<C-c>"] = "close",
            },
            n = {
                ["<M-p>"] = "move_selection_previous",
                ["<M-n>"] = "move_selection_next",
                ["<C-c>"] = "close",
            },
        },
    },
    pickers = {
        find_files = {
            hidden = true,
        },
    },
}

return plug
