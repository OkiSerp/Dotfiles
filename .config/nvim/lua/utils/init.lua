local M = {}

function M.ToggleWrap()
  if vim.opt.wrap:get() then
    vim.cmd("set nowrap")
    return
  end
  vim.cmd("set wrap")
end

function M.ToggleNumbers()
  if vim.opt.rnu:get() and vim.opt.nu:get() then
    vim.opt.nu = false
    vim.opt.rnu = false
  elseif vim.opt.nu:get() and not vim.opt.rnu:get() then
    vim.opt.nu = true
    vim.opt.rnu = true
  else
    vim.opt.nu = true
    vim.opt.rnu = false
  end
end

return M
