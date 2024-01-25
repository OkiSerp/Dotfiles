local plug = {
    "VonHeikemen/lsp-zero.nvim",
    branch = "v3.x",
    lazy = false,
}

plug.dependencies = {
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
    "neovim/nvim-lspconfig",
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/nvim-cmp",
    "L3MON4D3/LuaSnip",
}

plug.keys = {
    { "<leader>hm", ":Mason<Cr>", mode = { "n", "v" } },
}

plug.config = function()
    require("mason").setup({
        ui = {
            icons = {
                package_installed = "✓",
                package_pending = "➜",
                package_uninstalled = "✗",
            },
        }
    })

    require("mason-lspconfig").setup({
        ensure_installed = {
            "lua_ls",
        },
        automatic_installation = true,
    })

    local cmp = require("cmp")
    cmp.setup({
        mapping = cmp.mapping.preset.insert({
            ["<M-k>"] = cmp.mapping.select_prev_item(),
            ["<M-j>"] = cmp.mapping.select_next_item(),
            ["<M-d>"] = cmp.mapping.scroll_docs(-4),
            ["<M-f>"] = cmp.mapping.scroll_docs(4),
            ["<M-Space>"] = cmp.mapping.complete(),
            ["<M-e>"] = cmp.mapping.abort(),
            ["<Cr>"] = cmp.mapping.confirm({ select = true }),
        })
    })

    local lspzero = require("lsp-zero")
    lspzero.on_attach(function(_, bufnr)
        lspzero.default_keymaps({ buffer = bufnr })
    end)

    local lspconfig = require("lspconfig")
    lspconfig.lua_ls.setup({
        settings = {
            Lua = {
                diagnostics = {
                    globals = { "vim" },
                },
                workspace = {
                    library = {
                        [vim.fn.expand("$VIMRUNTIME/lua")] = true,
                        [vim.fn.stdpath("config") .. "/lua"] = true,
                    },
                },
            },
        },
    })
end

return plug
