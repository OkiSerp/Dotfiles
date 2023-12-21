;;; input/reverse/config.el -*- lexical-binding: t; -*-

(use-package! char-fold
  :custom
  (char-fold-symmetric t)
  (search-default-mode 'char-fold-to-regexp))

(use-package! reverse-im
  :demand t
  :after char-fold
  :custom
  (reverse-im-char-fold t)
  (reverse-im-read-char-advice-function
   'reverse-im-read-char-include)
  (reverse-im-input-methods
   '("ukrainian-computer" "russian-computer"))
  (reverse-im-modifiers '(control meta))
  :config
  (reverse-im-mode 1))
