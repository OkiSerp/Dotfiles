;;; completion/corfu/config.el -*- lexical-binding: t; -*-

(use-package! corfu
  :custom
  (corfu-auto t)
  (corfu-quit-at-boundary t)
  :init
  (setq completion-cycle-threshold 3
        tab-always-indent 'complete)
  :config
  (corfu-echo-mode 1)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)
  (global-corfu-mode 1))

(map! :after corfu
      :map corfu-map
      "M-k" 'corfu-previous
      "M-j" 'corfu-next
      "ESC" 'corfu-quit)

(map! :after corfu
      "C-SPC" 'completion-at-point)

(use-package! nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters
               'nerd-icons-corfu-formatter))
