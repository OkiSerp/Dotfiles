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

(defun @/org-center-buffer (&rest _)
  (setq-local
   visual-fill-column-width 90
   visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(add-hook 'org-mode-hook '@/org-center-buffer)

(defun @/org-insert-heading (&rest _)
  (evil-open-above 1)
  (evil-normal-state)
  (evil-next-line)
  (evil-append-line 1))

(add-hook 'org-insert-heading-hook '@/org-insert-heading)

(defun @/org-meta-return (&optional arg &rest _)
  (interactive "P")
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (call-interactively
       (cond (arg #'org-insert-heading)
             ((org-at-table-p) #'org-table-wrap-region)
             ((org-at-heading-or-item-p) #'+org/insert-item-below)))))

(map! :after evil-org
      :map (evil-org-mode-map org-mode-map)
      "M-RET" '@/org-meta-return)

(map! :map (evil-org-mode-map org-mode-map)
      "M-[" 'org-previous-visible-heading
      "M-]" 'org-next-visible-heading)

(evil-set-register ?w [?A ?* escape ?^ ?w ?i ?* escape ?j])
(evil-set-register ?e [?A ?/ escape ?^ ?w ?i ?/ escape ?j])

(evil-set-register ?r [?v ?i ?w escape ?a ?_ escape ?b ?i ?_ escape ?w])

(setq default-input-method "ukrainian-computer")

(map! :leader (:prefix ("l" . "translate")))

(map! :leader :desc "English input" "le"
      (cmd! (set-input-method nil)
            (setq default-input-method "ukrainian-computer"
                  evil-input-method nil)))

(map! :leader :desc "Ukrainian input" "lu"
      (cmd! (set-input-method "ukrainian-computer")))

(map! :leader :desc "Russian input" "lr"
      (cmd! (set-input-method "russian-computer")))

(dolist
    (provider
     '(("Cambridge dictionary"
        "https://dictionary.cambridge.org/dictionary/english/%s")
       ("Google translate"
        "https://translate.google.com/?sl=en&tl=uk&text=%s&op=translate")
       ("Deepl ru"
        "https://www.deepl.com/translator#en/ru/%s")))
  (add-to-list '+lookup-provider-url-alist provider))

(map! :leader :desc "Cambridge dictionary" "lc"
      (cmd! (+lookup/online
             (doom-thing-at-point-or-region) "Cambridge dictionary")))

(map! :leader :desc "Google translate" "lg"
      (cmd! (+lookup/online
             (doom-thing-at-point-or-region) "Google translate")))

(map! :leader :desc "Deepl ru" "ld"
      (cmd! (+lookup/online
             (doom-thing-at-point-or-region) "Deepl ru")))

(use-package! google-translate
  :config
  (setq google-translate-default-source-language "en"
        google-translate-default-target-language "uk"
        google-translate-pop-up-buffer-set-focus t
        google-translate-output-destination 'help
        google-translate-backend-method 'curl
        google-translate-show-phonetic t
        google-translate-display-translation-phonetic nil))

(after! google-translate
  (map! :leader :desc "Translate buffer" "lb"
        'google-translate-buffer)
  (map! :leader :desc "Translate at point" "lw"
        'google-translate-at-point)
  (map! :leader :desc "Translate at point reverse" "lW"
        'google-translate-at-point-reverse)
  (map! :leader :desc "Translate smooth" "ls"
        'google-translate-smooth-translate)
  (map! :leader :desc "Translate query" "lq"
        'google-translate-query-translate)
  (map! :leader :desc "Translate query reverse" "lQ"
        'google-translate-query-translate-reverse))

(use-package! vertico
  :config
  (setq vertico-count 10)
  :bind (:map vertico-map
              ("M-k" . vertico-previous)
              ("M-j" . vertico-next)))

(use-package! company
  :bind (:map company-active-map
              ("M-k" . company-select-previous)
              ("M-j" . company-select-next)))

(setq +doom-dashboard-menu-sections nil
      +doom-dashboard-ascii-banner-fn nil)

(remove-hook '+doom-dashboard-functions 'doom-dashboard-widget-footer)

(add-hook! 'doom-after-init-hook
  (add-hook! '+doom-dashboard-functions :append
    (insert "\n" (+doom-dashboard--center +doom-dashboard--width "Pure Evil‼"))))

(add-hook! '+doom-dashboard-mode-hook
  (setq-local evil-normal-state-cursor '(hbar . 0)))

(defvar @/frame-geometry-file
  (expand-file-name "frame-geometry" user-emacs-directory)
  "File that stores previous session's last frame' geometry.")

(defun @/frame-geometry-write-file (&rest _)
  "Write frame geometry to `@/frame-geometry-file'."
  (let ((top (frame-parameter (selected-frame) 'top))
        (left (frame-parameter (selected-frame) 'left))
        (width (frame-parameter (selected-frame) 'width))
        (height (frame-parameter (selected-frame) 'height))
        (fullscreen (frame-parameter (selected-frame) 'fullscreen)))
    (if fullscreen
        (with-temp-file @/frame-geometry-file
          (insert
           (format
            "(add-to-list 'default-frame-alist '(fullscreen . %S))\n"
            fullscreen)))
      (with-temp-file @/frame-geometry-file
        (insert
         "(dolist (param '("
         (format "(top . %d)\n" top)
         (format "\t(left . %d)\n" left)
         (format "\t(width . %d)\n" width)
         (format "\t(height . %d)))\n" height)
         "(add-to-list 'default-frame-alist param))\n")))))

(defun @/frame-geometry-load-file (&rest _)
  "Load frame geometry from `@/frame-geometry-file'."
  (when (file-readable-p @/frame-geometry-file)
    (load @/frame-geometry-file :noerror :nomessage)))

(add-hook 'emacs-startup-hook '@/frame-geometry-load-file)
(add-hook 'kill-emacs-hook '@/frame-geometry-write-file)

(defun @/set-blur-behind-x-frame (&rest _)
  "Set blur behind `x' frame."
  (interactive)
  (let* ((frame-id (frame-parameter (selected-frame) 'outer-window-id))
         (command (concat "xprop -f _KDE_NET_WM_BLUR_BEHIND_REGION 32c "
                          "-set _KDE_NET_WM_BLUR_BEHIND_REGION 0 -id "
                          frame-id)))
    (call-process-shell-command command nil nil)
    (set-frame-parameter (selected-frame) 'blur 1)))

(defun @/remove-blur-behind-x-frame (&rest _)
  "Set blur behind `x' frame."
  (interactive)
  (let* ((frame-id (frame-parameter (selected-frame) 'outer-window-id))
         (command (format "xprop -remove _KDE_NET_WM_BLUR_BEHIND_REGION -id %s"
                          frame-id)))
    (call-process-shell-command command nil nil)
    (set-frame-parameter (selected-frame) 'blur 0)))

(defun @/toggle-blur-behind-x-frame ()
  "Toggle blur behind `x' frame."
  (interactive)
  (let ((blur (frame-parameter (selected-frame) 'blur)))
    (cl-block nil
      (when (eql blur nil)
        (@/set-blur-behind-x-frame)
        (cl-return))
      (if (> blur 0)
          (@/remove-blur-behind-x-frame)
        (@/set-blur-behind-x-frame)))))

(defun @/set-blur-behind-new-x-frame ()
  "Set frame behind newly created `x' frame."
  (let ((blur (frame-parameter (selected-frame) 'blur)))
    (when (eql blur nil)
      (@/set-blur-behind-x-frame))))

(when (eql (window-system) 'x)
  (add-to-list 'default-frame-alist '(alpha-background . 90))
  (add-hook 'emacs-startup-hook '@/set-blur-behind-x-frame)
  (add-hook 'doom-switch-frame-hook '@/set-blur-behind-new-x-frame)
  (map! :leader :desc "Blur behind frame" "tu"
        '@/toggle-blur-behind-x-frame))

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
