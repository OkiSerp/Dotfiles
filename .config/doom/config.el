;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq confirm-kill-emacs nil)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.

(setq user-full-name "Oleksii Kapula"
      user-mail-address "")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(let ((size 20)
      (font "Mononoki Nerd Font"))
  (when (doom-font-exists-p font)
    (setq doom-font (font-spec :family font :size size)
          doom-big-font (font-spec :family font :size (+ 6 size)))))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.

(use-package! catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin :no-confirm)
  (custom-set-faces!
    `(show-paren-match
      :background ,(catppuccin-get-color 'crust))))

(defface @/visual-bell
  `((t :foreground ,(catppuccin-get-color 'text)
     :background ,(catppuccin-get-color 'red)))
  "Face to use for `@/visual-bell-fn'.")

(defun @/visual-bell-fn (&rest _)
  "Function to use for `ring-bell-function'."
  (let* ((face (if (facep 'mode-line-active)
                   'mode-line-active
                 'mode-line))
         (buf (current-buffer))
         (cookie (face-remap-add-relative
                  face '@/visual-bell)))
    (force-mode-line-update)
    (run-with-timer
     0.15 nil
     (lambda ()
       (with-current-buffer buf
         (face-remap-remove-relative cookie)
         (force-mode-line-update))))))

(after! catppuccin-theme
  (setq ring-bell-function '@/visual-bell-fn
        visible-bell t))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

(setq display-line-numbers-type nil)

(add-hook! 'prog-mode-hook
  (face-remap-add-relative 'font-lock-comment-face :slant 'italic))

(setq-default tab-width 2
              standard-indent 2)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(let ((dir "~/OrgFiles/"))
  (setq org-directory dir
        org-archive-location (expand-file-name ".archive/%s::" dir)
        org-agenda-files (list (expand-file-name "Agenda.org" dir))))

(setq org-emphasis-regexp-components
      '("-[:space:]('\"{" "-[:space:].,:!?;'\")}\\[‼…" "[:space:]" "." 1))

(after! org
  (setq
   org-log-done t
   org-ellipsis " …"
   org-special-ctrl-k t
   org-log-into-drawer t
   org-startup-folded 'fold
   org-image-actual-width nil
   org-hide-block-startup nil
   org-hide-drawer-startup t
   org-hide-emphasis-markers t
   org-appear-autoemphasis nil
   org-startup-with-inline-images nil))

(after! org
  (setq org-superstar-headline-bullets-list '(9675))
  (setq org-superstar-item-bullet-alist '((43 . 10022) (45 . 10148))))

(after! (org catppuccin-theme)
  (custom-set-faces!
    `(org-todo :foreground ,(catppuccin-get-color 'teal))
    `(org-verbatim :foreground ,(catppuccin-get-color 'yellow))))

(defun @/browse-org-directory (&rest _)
  "Browse your `org-directory'."
  (interactive)
  (unless (file-directory-p org-directory)
    (make-directory org-directory 'parents))
  (doom-project-browse org-directory))

(map! :leader :desc "Browse org directory" "fo"
      '@/browse-org-directory)

(defun @/org-enlarge-headlines (&rest _)
  (dolist (face '((org-level-1 . 1.5) (org-level-2 . 1.4)
                  (org-level-3 . 1.3) (org-level-4 . 1.3)
                  (org-level-5 . 1.3) (org-level-6 . 1.3)
                  (org-level-7 . 1.3) (org-level-8 . 1.3)))
    (set-face-attribute (car face) nil :weight 'heavy :height (cdr face))))

(add-hook 'org-mode-hook '@/org-enlarge-headlines)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
