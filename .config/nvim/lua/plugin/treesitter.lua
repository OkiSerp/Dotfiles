return {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    lazy = false,
    config = function()
        require("nvim-treesitter.configs").setup({
            ensure_installed = {
                "vim",
                "lua",
                "vimdoc",
                "luadoc",
                "bash",
                "fish",
                "diff",
                "git_config",
                "gitcommit",
                "gitignore",
                "json",
                "toml",
                "yaml",
            },
        })
    end,
}
