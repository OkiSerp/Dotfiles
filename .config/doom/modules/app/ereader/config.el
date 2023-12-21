;;; app/ereader/config.el -*- lexical-binding: t; -*-

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook
  ((nov-mode . mixed-pitch-mode)
   (nov-mode . visual-line-mode))
  :custom
  (nov-text-width t)
  (nov-variable-pitch nil)
  :config
  (add-hook! 'nov-mode-hook
    (face-remap-add-relative 'default :height 1.1)))

(map! :after nov
      :map nov-mode-map
      :nv "k" 'evil-previous-visual-line
      :nv "j" 'evil-next-visual-line
      :nv "0" 'evil-beginning-of-visual-line
      :nv "$" 'evil-end-of-visual-line)

(use-package! olivetti
  :after nov
  :config
  (add-hook! 'nov-mode-hook
    (setq-local olivetti-body-width 80)
    (olivetti-mode 1)))
