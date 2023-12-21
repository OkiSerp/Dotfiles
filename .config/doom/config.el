;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 40))

(setq confirm-kill-emacs nil)

(defun serp/delete-frame (&rest _)
  "Delete frame without prompt."
  (interactive)
  (if (cdr (visible-frame-list))
      (delete-frame)
    (save-buffers-kill-emacs)))

(bind-key [remap delete-frame] 'serp/delete-frame)

(let ((size 16)
      (font "Mononoki"))
  (when (doom-font-exists-p font)
    (setq doom-font (font-spec :name font :size size)
          doom-big-font (font-spec :name font :size (+ 6 size)))))

(let ((font "Noto Color Emoji"))
  (when (doom-font-exists-p font)
    (setq doom-emoji-font (font-spec :name font))))

(let ((font "Noto Sans Symbols 2"))
  (when (doom-font-exists-p font)
    (setq doom-symbol-font (font-spec :name font))))

(let ((font "Noto Sans"))
  (when (doom-font-exists-p font)
    (setq doom-variable-pitch-font (font-spec :name font))))

(let ((font "Noto Serif"))
  (when (doom-font-exists-p font)
    (setq doom-serif-font (font-spec :name font))))

(use-package! catppuccin-theme
  :custom
  (catppuccin-flavor 'mocha)
  :config
  (load-theme 'catppuccin t))

(after! catppuccin-theme
  (custom-set-faces!
    `(show-paren-match
      :background ,(catppuccin-get-color 'surface2))))

(setq +doom-dashboard-functions
      '(doom-dashboard-widget-loaded))

(add-hook! '+doom-dashboard-mode-hook
  (setq-local evil-normal-state-cursor '(hbar . 0)))

(setq doom-modeline-icon nil)

(setq display-line-numbers-type nil
      scroll-margin 5)

(setq-default tab-width 2
              indent-tabs-mode nil)

(after! fish-mode
  (setq fish-indent-offset 2))

(use-package! evil
  :init
  (setq evil-disable-insert-state-bindings t
        evil-kill-on-visual-paste nil
        evil-want-fine-undo t))

(after! evil
  (map! :nv "'" 'evil-jump-item))

(after! evil-snipe
  (setq evil-snipe-scope 'visible))

(map! :map vertico-map
      "M-k" 'vertico-previous
      "M-j" 'vertico-next)

(map! :map read-expression-map
      "M-k" 'previous-line-or-history-element
      "M-j" 'next-line-or-history-element)

(map! :map global-map
      "M-k" 'previous-buffer
      "M-j" 'next-buffer
      "M-i" 'ibuffer)

(map! :leader
      (:prefix ("d" . "dired")
       :n "j" 'dired-jump
       :n "o" 'dired-jump-other-window))

(map! :map dired-mode-map
      :n "h" 'dired-up-directory
      :n "l" 'dired-find-alternate-file)

(setq trash-directory "~/.local/share/Trash/files/"
      delete-by-moving-to-trash t
      magit-delete-by-moving-to-trash t)

(after! (org catppuccin-theme)
  (custom-set-faces!
    `(org-todo :foreground ,(catppuccin-get-color 'teal))
    `(org-verbatim :foreground ,(catppuccin-get-color 'yellow))))

(setq org-directory "~/OrgFiles/")

(after! org
  (setq org-log-done t
        org-ellipsis " …"
        org-startup-folded 'fold
        org-image-actual-width nil
        org-hide-emphasis-markers t
        org-appear-autoemphasis nil))

(after! org
  (setq org-superstar-headline-bullets-list '(10033)
        org-superstar-item-bullet-alist '((43 . 8226) (45 . 9702))))

(defun serp/browse-org-directory (&rest _)
  "Browse your `org-directory'."
  (interactive)
  (unless (file-directory-p org-directory)
    (make-directory org-directory t))
  (doom-project-browse org-directory))

(map! :leader :desc "Browse org directory" "fo"
      'serp/browse-org-directory)

(setq org-emphasis-regexp-components
      '("-[:space:]('\"{" "-[:space:].,:!?;'\")}\\[‼…" "[:space:]" "." 1))

(defun serp/org-enlarge-headlines (&rest _)
  "Make org headings larger and thicker."
  (dolist (face '((org-level-1 . 1.5) (org-level-2 . 1.4)
                  (org-level-3 . 1.3) (org-level-4 . 1.3)
                  (org-level-5 . 1.3) (org-level-6 . 1.3)
                  (org-level-7 . 1.3) (org-level-8 . 1.3)))
    (set-face-attribute (car face) nil :weight 'heavy :height (cdr face))))

(add-hook 'org-mode-hook 'serp/org-enlarge-headlines)

(defun serp/org-meta-return (&optional arg)
  "Make the same logic as `org-meta-return', but better."
  (interactive "P")
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (call-interactively
       (cond (arg 'org-insert-heading)
             ((org-at-table-p) 'org-table-wrap-region)
             ((org-in-item-p) '+org/insert-item-below)
             ('org-insert-heading)))))

(map! :after org
      :map org-mode-map
      "M-RET" 'serp/org-meta-return
      :n "RET" '+org/dwim-at-point)

(after! org
  (evil-set-register ?w [?A ?* escape ?^ ?w ?i ?* escape ?j])
  (evil-set-register ?e [?A ?/ escape ?^ ?w ?i ?/ escape ?j])
  (evil-set-register ?r [?v ?i ?w escape ?a ?_ escape ?b ?i ?_ escape ?w])
  (evil-set-register ?f [?v ?i ?w escape ?a ?+ escape ?b ?i ?+ escape ?w]))

(use-package! elfeed
  :hook (elfeed-search-mode . elfeed-update)
  :config
  (setq elfeed-search-filter "@4-month-ago +unread"))

(map! :leader :desc "RSS" "oe" 'elfeed)

(use-package! olivetti
  :custom
  (olivetti-body-width 80)
  :config
  (add-hook! '(text-mode-hook org-mode-hook)
    (olivetti-mode 1)))
