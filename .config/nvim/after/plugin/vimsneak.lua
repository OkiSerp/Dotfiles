local _, ok = pcall(require, "vim-sneak")

if not ok then
  return
end

vim.cmd("let g:sneak#use_ic_scs = 1")
