local plug = {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    lazy = false,
}

local ensure = {
    "lua",
    "luadoc",
    "vim",
    "vimdoc",
    "bash",
    "fish",
    "gitignore",
    "markdown",
    "javascript",
    "typescript",
    "html",
    "scss",
    "css",
    "cpp",
    "c",
}

plug.config = function()
    local conf = require("nvim-treesitter.configs")
    conf.setup({
        ensure_installed = ensure,
        sync_install = false,
        auto_install = true,
        highlight = { enable = true },
        autotag = { enable = true },
        indent = { enable = true },
    })
end

return plug
