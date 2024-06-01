local plugin = {
    "lewis6991/gitsigns.nvim",
    lazy = false,
}

local function map(mode, lhs, rhs, opts)
    opts = opts or {}
    lhs = "<M-g>" .. lhs
    local tmode = {}
    mode:gsub(".", function(c) table.insert(tmode, c) end)
    vim.keymap.set(tmode, lhs, rhs, opts)
end

plugin.config = function()
    local gitsigns = require("gitsigns")
    gitsigns.setup({
        current_line_blame_opts = {
            delay = 500,
        },
        on_attach = function(_)
            map("n", "[", gitsigns.prev_hunk)
            map("n", "]", gitsigns.next_hunk)
            map("n", "p", gitsigns.preview_hunk)
            map("n", "b", gitsigns.toggle_current_line_blame)
        end,
    })
end

return plugin
