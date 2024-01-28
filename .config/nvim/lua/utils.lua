local utils = {}

utils.bind = function(mode, map, cmd, opts)
    local opts = opts or {}
    vim.keymap.set(mode, map, cmd, opts)
end

utils.togglewrap = function()
    if vim.opt.wrap:get() then
        vim.cmd("set nowrap")
        return
    end
    vim.cmd("set wrap")
end

utils.togglenumber = function()
    if vim.opt.rnu:get() and vim.opt.nu:get() then
        vim.opt.nu = false
        vim.opt.rnu = false
    elseif not vim.opt.rnu:get() and vim.opt.nu:get() then
        vim.opt.nu = true
        vim.opt.rnu = true
    else
        vim.opt.nu = true
        vim.opt.rnu = false
    end
end

return utils
