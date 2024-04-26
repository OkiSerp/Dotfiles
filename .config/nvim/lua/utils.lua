local utils = {}

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

utils.togglespell = function()
  if vim.opt.spell:get() then
    vim.cmd("set nospell")
    return
  end
  vim.cmd("set spell")
end

utils.toggleinputmethod = function ()
  local opt = vim.opt.keymap:get()
  if opt == nil or opt == "" then
    vim.cmd("set iminsert=1 imsearch=1")
    vim.cmd("set keymap=ukrainian-jcuken")
    return
  end
  vim.cmd("set iminsert=0 imsearch=-1")
  vim.cmd("set keymap=")
end

return utils
