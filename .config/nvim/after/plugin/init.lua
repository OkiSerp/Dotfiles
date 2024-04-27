local status, wk = pcall(require, "which-key")
if not status then
    return
end

local srp = { toggle = {} }

srp.toggle.wrap = function()
    if vim.opt.wrap:get() then
        vim.cmd("set nowrap")
        return
    end
    vim.cmd("set wrap")
end

srp.toggle.number = function()
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

srp.toggle.spell = function()
    if vim.opt.spell:get() then
        vim.cmd("set nospell")
        return
    end
    vim.cmd("set spell")
end

srp.toggle.im = function()
    local opt = vim.opt.keymap:get()
    if opt == nil or opt == "" then
        vim.cmd("set iminsert=1 imsearch=1")
        vim.cmd("set keymap=ukrainian-jcuken")
        return
    end
    vim.cmd("set iminsert=0 imsearch=-1")
    vim.cmd("set keymap=")
end

wk.register({
    t = {
        name = "toggle",
        w = { srp.toggle.wrap, "Soft line wrapping" },
        l = { srp.toggle.number, "Line numbers" },
        s = { srp.toggle.spell, "Spell checker" },
    }
}, { prefix = "<leader>", mode = "n" })

wk.register({
    [ "<C-\\>" ] = { srp.toggle.im, "Input method" },
}, { mode = { "n", "v", "i" } })
