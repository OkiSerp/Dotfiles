local setup = function()
  require("mason").setup({
    ui = {
      icons = {
        package_installed = "✓",
        package_pending = "➜",
        package_uninstalled = "✗",
      },
    }
  })

  vim.keymap.set({ "n", "v" }, "<leader>hm", vim.cmd.Mason)

  require("mason-lspconfig").setup({
    ensure_installed = {
      "lua_ls",
      "tsserver",
      "emmet_ls",
      "jsonls",
      "texlab"
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
    lspzero.default_keymaps({buffer = bufnr})
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

  lspconfig.jsonls.setup({
    filetypes = { "json", "jsonc" },
  })

  lspconfig.tsserver.setup({
    filetypes = { "typescript" },
    root_dir = lspconfig.util.root_pattern(
      "package.json", "tsconfig.json", ".git"
    ),
  })

  lspconfig.emmet_ls.setup({
		filetypes = {
			"typescriptreact",
			"javascriptreact",
			"javascript",
			"css",
			"sass",
			"scss",
			"less",
			"vue",
			"html",
		},
	})

  lspconfig.texlab.setup({})
end

return {
  "VonHeikemen/lsp-zero.nvim",
  branch = "v3.x",
  lazy = false,
  dependencies = {
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
    "neovim/nvim-lspconfig",
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/nvim-cmp",
    "L3MON4D3/LuaSnip",
  },
  config = setup,
}
