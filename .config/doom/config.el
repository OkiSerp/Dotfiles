;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq confirm-kill-emacs nil)

(setq user-full-name "Oleksii Kapula"
      user-mail-address "")

(let ((size 20)
      (font "Mononoki Nerd Font"))
  (when (doom-font-exists-p font)
    (setq doom-font (font-spec :family font :size size)
          doom-big-font (font-spec :family font :size (+ 6 size)))))

(use-package! catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin :no-confirm))

(after! catppuccin-theme
  (custom-set-faces!
    `(show-paren-match
      :background ,(catppuccin-get-color 'surface2))))

(setq display-line-numbers-type nil)

(add-hook! '(prog-mode-hook conf-mode-hook)
  (face-remap-add-relative 'font-lock-comment-face :slant 'italic))

(setq-default tab-width 2
              standard-indent 2)

(after! fish-mode
  (setq fish-indent-offset 2))

(setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

(setq scroll-margin 9)

(setq fill-column 80
      display-fill-column-indicator t
      display-fill-column-indicator-character ?│)

(add-hook! '(prog-mode-hook conf-mode-hook)
  (display-fill-column-indicator-mode +1))

(let ((dir (expand-file-name "OrgFiles/" (getenv "HOME"))))
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
  (setq org-superstar-headline-bullets-list '(10033))
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
  "Make org headings larger and thicker."
  (dolist (face '((org-level-1 . 1.5) (org-level-2 . 1.4)
                  (org-level-3 . 1.3) (org-level-4 . 1.3)
                  (org-level-5 . 1.3) (org-level-6 . 1.3)
                  (org-level-7 . 1.3) (org-level-8 . 1.3)))
    (set-face-attribute (car face) nil :weight 'heavy :height (cdr face))))

(add-hook 'org-mode-hook '@/org-enlarge-headlines)

(defun @/org-center-buffer (&rest _)
  "Center org buffer."
  (setq-local
   visual-fill-column-width 90
   visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(add-hook 'org-mode-hook '@/org-center-buffer)

(defun @/org-insert-heading (&rest _)
  "Add one line above newly created org heading."
  (evil-open-above 1)
  (evil-normal-state)
  (evil-next-line)
  (evil-append-line 1))

(add-hook 'org-insert-heading-hook '@/org-insert-heading)

(defun @/org-meta-return (&optional arg &rest _)
  "Modified version of `org-meta-return'."
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

(map! :leader :desc "English input method" "le"
      (cmd! (set-input-method nil)
            (setq default-input-method "ukrainian-computer"
                  evil-input-method nil)))

(map! :leader :desc "Ukrainian input method" "lu"
      (cmd! (set-input-method "ukrainian-computer")))

(map! :leader :desc "Russian input method" "lr"
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

(after! google-translate
  (map! :leader :desc "Change source lang to ru" "lR"
        (cmd! (setq google-translate-default-target-language "ru")))
  (map! :leader :desc "Change source lang to uk" "lU"
        (cmd! (setq google-translate-default-target-language "uk"))))

(defun @/google-translate-from-clipboard (&rest _)
  "Translate text from clipboard using `google-translate' package."
  (interactive)
  (let ((source google-translate-default-source-language)
        (target google-translate-default-target-language)
        (output google-translate-output-destination)
        (text (gui-get-selection 'CLIPBOARD)))
    (google-translate-translate source target text output)))

(after! google-translate
  (map! :leader :desc "Translate from clipboard" "lf"
        '@/google-translate-from-clipboard))

(map! "M-v" 'yank
      :after evil-org
      :map (evil-org-mode-map org-mode-map)
      "M-v" 'org-yank)

(map! :after evil-org
      :map (evil-org-mode-map org-mode-map)
      :nvi "M-k" 'previous-buffer
      :nvi "M-j" 'next-buffer)

(map! :nvi "M-k" 'previous-buffer
      :nvi "M-j" 'next-buffer)

(map! "M-q" 'kill-current-buffer)
(map! "M-s" 'save-buffer)

(map! "M-i" '+vertico/switch-workspace-buffer
      "M-I" 'consult-buffer)

(map! :leader "dj" 'dired-jump)
(map! :leader "do" 'dired-jump-other-window)

(map! :map dired-mode-map
      :n "h" #'dired-up-directory
      :n "l" #'dired-find-alternate-file)

(use-package! vertico
  :config
  (setq vertico-count 10)
  :bind (:map vertico-map
              ("M-k" . vertico-previous)
              ("M-j" . vertico-next)))

(use-package! company
  :config
  (setq company-box-scrollbar nil)
  :bind (:map company-active-map
              ("M-k" . company-select-previous)
              ("M-j" . company-select-next)))

(map! :map read-expression-map
      "M-k" 'previous-line-or-history-element
      "M-j" 'next-line-or-history-element)

(keymap-global-unset "C-s")

(after! evil
  (map! :nvm "'" 'evil-jump-item))

(after! evil-snipe
  (setq evil-snipe-scope 'visible
        evil-snipe-repeat-scope 'whole-visible))

(customize-set-variable
 'evil-disable-insert-state-bindings t)

(after! evil
  (setq evil-kill-on-visual-paste nil
        evil-want-fine-undo t))

(after! evil
  (setq evil-split-window-below t
        evil-vsplit-window-right nil))

(customize-set-variable 'vterm-always-compile-module t)

(setq trash-directory "~/.local/share/Trash/files/"
      magit-delete-by-moving-to-trash t
      delete-by-moving-to-trash t)

(after! recentf
  (add-to-list
   'recentf-exclude (expand-file-name ".local/" doom-emacs-dir)))

(after! doom-modeline
  (add-to-list 'doom-modeline-continuous-word-count-modes 'text-mode)
  (setq doom-modeline-enable-word-count t))

(customize-set-variable
 'doom-modeline-buffer-file-name-style 'relative-to-project)

(setq +doom-dashboard-menu-sections nil
      +doom-dashboard-ascii-banner-fn nil)

(remove-hook '+doom-dashboard-functions 'doom-dashboard-widget-footer)

(add-hook! 'doom-after-init-hook
  (add-hook! '+doom-dashboard-functions :append
    (insert "\n" (+doom-dashboard--center
                  +doom-dashboard--width "Pure Evil‼"))))

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
            "(add-to-list 'initial-frame-alist '(fullscreen . %S))\n"
            fullscreen)))
      (with-temp-file @/frame-geometry-file
        (insert
         "(dolist (param '("
         (format "(top . %d)\n" top)
         (format "\t(left . %d)\n" left)
         (format "\t(width . %d)\n" width)
         (format "\t(height . %d)))\n" height)
         "(add-to-list 'initial-frame-alist param))\n")))))

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
    (when (eql (window-system) 'x)
      (call-process-shell-command command nil nil)
      (set-frame-parameter (selected-frame) 'blur 1))))

(defun @/remove-blur-behind-x-frame (&rest _)
  "Set blur behind `x' frame."
  (interactive)
  (let* ((frame-id (frame-parameter (selected-frame) 'outer-window-id))
         (command (format "xprop -remove _KDE_NET_WM_BLUR_BEHIND_REGION -id %s"
                          frame-id)))
    (when (eql (window-system) 'x)
      (call-process-shell-command command nil nil)
      (set-frame-parameter (selected-frame) 'blur 0))))

(defun @/toggle-blur-behind-x-frame (&rest _)
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

(defun @/set-blur-behind-new-x-frame (&rest _)
  "Set frame behind newly created `x' frame."
  (let ((blur (frame-parameter (selected-frame) 'blur)))
    (when (eql blur nil)
      (@/set-blur-behind-x-frame))))

(add-to-list 'default-frame-alist '(alpha-background . 90))

(add-hook 'window-setup-hook '@/set-blur-behind-x-frame)
(add-hook 'doom-switch-frame-hook '@/set-blur-behind-new-x-frame)

(map! :leader :desc "Blur behind frame" "tu"
      '@/toggle-blur-behind-x-frame)
