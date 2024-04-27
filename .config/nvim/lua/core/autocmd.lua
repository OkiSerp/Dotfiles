vim.cmd [[
if has("autocmd")
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | execute "normal! g`\"" | endif
endif
]]

vim.api.nvim_create_autocmd({ "BufWritePre" }, {
    pattern = { "*" },
    callback = function()
        local save_cursor = vim.fn.getpos(".")
        pcall(function() vim.cmd [[%s/\s\+$//e]] end)
        vim.fn.setpos(".", save_cursor)
    end,
})
