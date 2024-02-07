vim.keymap.set("n", "<localleader>su", function()
    vim.cmd("setlocal spell spelllang=uk_UA")
end)

vim.keymap.set("n", "<localleader>sr", function()
    vim.cmd("setlocal spell spelllang=ru_RU")
end)

vim.keymap.set("n", "<localleader>se", function()
    vim.cmd("setlocal spell spelllang=en_US")
end)
