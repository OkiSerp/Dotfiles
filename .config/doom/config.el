;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq confirm-kill-emacs nil)

(defun my/delete-frame (&rest _)
  "Delete frame without prompt."
  (interactive)
  (if (cdr (visible-frame-list))
      (delete-frame)
    (save-buffers-kill-terminal)))

(bind-key [remap delete-frame] 'my/delete-frame)

(defvar my/font-family "Iosevka"
  "Default font family for `my/font-load' function.")

(defvar my/font-size 18
  "Default font size for `my/font-load' function.")

(defun my/font-load
    (&optional _ family size big-size &rest _)
  "Configure font family and its size both explicitly or interactively."
  (interactive
   (list current-prefix-arg
         (read-string "Font family: " my/font-family)
         (read-number "Font size: " my/font-size)))
  (let* ((family (cond (family) (my/font-family)))
         (size (cond (size) (my/font-size)))
         (big-size (cond (big-size) (t (+ 6 size)))))
    (if (doom-font-exists-p family)
        (progn
          (setq doom-font (font-spec :family family :size size)
                doom-big-font (font-spec :family family :size big-size))
          (when (called-interactively-p 'any)
            (doom/reload-font)))
      (message "%s" (propertize (format "%s doesn't exists!" family)
                                'face 'warning)))))

(map! :leader "hrF" 'my/font-load)

(my/font-load 0 nil 20 32)

(let ((font-family "JoyPixels"))
  (when (doom-font-exists-p font-family)
    (setq doom-emoji-font (font-spec :family font-family))))

(setq doom-theme 'doom-one)

(add-hook! '(prog-mode-hook conf-mode-hook)
  (face-remap-add-relative 'font-lock-comment-face :slant 'italic))

(defun my/doom-dashboard-widget-quote ()
  (when doom-init-time
    (insert
     (propertize
      (+doom-dashboard--center
       +doom-dashboard--width
       "Don't let the evil dwell in your system.")
      'face 'doom-dashboard-banner))))

(setq +doom-dashboard-functions
      '(my/doom-dashboard-widget-quote))

(add-hook! '+doom-dashboard-mode-hook
  (setq-local evil-normal-state-cursor '(hbar . 0)))

(setq doom-modeline-icon nil)

(setq display-line-numbers-type nil)

(setq mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

(setq hscroll-margin 13
      scroll-margin 7)

(custom-set-variables
 '(display-fill-column-indicator-character 124)
 '(display-fill-column-indicator-column 80))

(map! :leader :desc "Vertical ruler"
      "tc" 'display-fill-column-indicator-mode)

(setq-default tab-width 2
              standard-indent 2
              indent-tabs-mode nil)

(after! fish-mode
  (setq fish-indent-offset 2))

(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(add-hook! 'web-mode-hook
  (setq web-mode-part-padding 0
        web-mode-style-padding 0
        web-mode-script-padding 0))

(setq org-directory (expand-file-name ".orgnotes/" (getenv "HOME")))

(after! org
  (setq org-ellipsis " "
        org-log-done 'time
        org-startup-folded 'fold
        org-image-actual-width nil
        org-hide-emphasis-markers t
        org-appear-autoemphasis nil))

(after! org
  (setq org-superstar-headline-bullets-list '(10033)
        org-superstar-item-bullet-alist '((43 . 187) (45 . 8250))))

(defun my/browse-org-directory (&rest _)
  "Browse your `org-directory'."
  (interactive)
  (unless (file-directory-p org-directory)
    (make-directory org-directory t))
  (doom-project-browse org-directory))

(map! :leader :desc "Browse org directory"
      "fo" 'my/browse-org-directory)

(setq org-emphasis-regexp-components
      '("-[:space:]('\"{" "-[:space:].,:!?;'\")}\\[‼…" "[:space:]" "." 1))

(defun my/org-enlarge-headings (&rest _)
  "Make org headings larger and thicker."
  (let* ((level-1 1.7)
         (level-2 (- level-1 0.1))
         (level-3 (- level-2 0.1))
         (level-0 (- level-3 0.1)))
    (dolist (face `((org-level-1 . ,level-1) (org-level-2 . ,level-2)
                    (org-level-3 . ,level-3) (org-level-4 . ,level-0)
                    (org-level-5 . ,level-0) (org-level-6 . ,level-0)
                    (org-level-7 . ,level-0) (org-level-8 . ,level-0)))
      (set-face-attribute (car face) nil :weight 'heavy :height (cdr face)))))

(add-hook 'org-mode-hook 'my/org-enlarge-headings)

(defun my/org-insert-heading-fn (&rest _)
  "Add one line above newly created org heading."
  (evil-open-above 1)
  (evil-normal-state)
  (evil-next-line)
  (evil-append-line 1))

(add-hook 'org-insert-heading-hook 'my/org-insert-heading-fn)

(defun my/org-meta-return (&optional arg)
  "Make the same logic as `org-meta-return', but more suitable."
  (interactive "P")
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (call-interactively
       (cond (arg 'org-insert-heading)
             ((org-at-table-p) 'org-table-wrap-region)
             ((org-at-heading-or-item-p) '+org/insert-item-below)))))

(map! :after org
      :map org-mode-map
      "M-u" 'org-metaup
      "M-d" 'org-metadown
      "M-h" 'org-metaleft
      "M-l" 'org-metaright
      "M-RET" 'my/org-meta-return
      :n "RET" '+org/dwim-at-point
      :localleader "s." 'org-fold-show-all)

(after! org
  (evil-set-register ?w [?A ?* escape ?^ ?w ?i ?* escape ?j])
  (evil-set-register ?e [?A ?/ escape ?^ ?w ?i ?/ escape ?j])
  (evil-set-register ?r [?v ?i ?w escape ?a ?_ escape ?b ?i ?_ escape]))

(use-package! evil
  :init
  (setq evil-disable-insert-state-bindings t
        evil-kill-on-visual-paste nil
        evil-want-fine-undo t))

(after! evil
  (map! :nv (char-to-string ?') 'evil-jump-item))

(after! evil-snipe
  (setq evil-snipe-scope 'visible))

(map! :leader
      :desc "Browse private config"
      "fp" 'doom/open-private-config
      "fP" 'ignore)

(map! "M-s" 'save-buffer
      "M-e" 'kill-current-buffer)

(map! :nvi "M-k" 'previous-buffer
      :nvi "M-j" 'next-buffer
      :nvi "M-i" 'ibuffer)

(map! :map vertico-map
      "M-k" 'vertico-previous
      "M-j" 'vertico-next)

(use-package! company
  :custom (company-box-scrollbar nil)
  :bind (:map company-active-map
              ("M-k" . company-select-previous)
              ("M-j" . company-select-next)))

(map! :map read-expression-map
      "M-k" 'previous-line-or-history-element
      "M-j" 'next-line-or-history-element)

(map! :map (minibuffer-mode-map evil-command-line-map)
      "M-k" 'previous-history-element
      "M-j" 'next-history-element)

(map! :leader
      (:prefix ("d" . "dired")
       :n "j" 'dired-jump
       :n "o" 'dired-jump-other-window))

(map! :map dired-mode-map
      :n "h" 'dired-up-directory
      :n "l" 'dired-find-alternate-file)

(map! :leader "w M-o" 'delete-other-windows)

(setq trash-directory "~/.local/share/Trash/files/"
      delete-by-moving-to-trash t
      magit-delete-by-moving-to-trash t)

(when (modulep! :checkers spell +aspell)
  (remove-hook 'text-mode-hook 'spell-fu-mode))

(customize-set-variable
 'default-input-method "ukrainian-computer")

(use-package! google-translate
  :init
  (setq google-translate-preferable-input-methods-alist
        '((nil) ("ukrainian-computer" . ("uk" "ru"))))
  :config
  (set-face-attribute
   'google-translate-listen-button-face nil :height 1.0)
  (setq google-translate-listen-button-label "[Listen]"
        google-translate-listen-program (executable-find "mplayer")
        google-translate-default-source-language "en"
        google-translate-default-target-language "uk"
        google-translate-pop-up-buffer-set-focus t
        google-translate-output-destination 'help
        google-translate-backend-method 'curl
        google-translate-show-phonetic t
        google-translate-display-translation-phonetic nil
        google-translate-input-method-auto-toggling t))

(defun my/google-translate-listen-translation (language text)
  "Improved version of the original `google-translate-listen-translation'
function, but without debuging."
  (message "Retrieving audio message…")
  (apply 'call-process google-translate-listen-program nil nil nil
         (google-translate-format-listen-urls text language))
  (message "Done playing audio!"))

(defalias 'google-translate-listen-translation
  (symbol-function 'my/google-translate-listen-translation))

(defun my/google-translate-clipboard (&rest _)
  "Translate text from clipboard by using `google-translate' package."
  (interactive)
  (let ((source google-translate-default-source-language)
        (target google-translate-default-target-language)
        (output google-translate-output-destination)
        (text (gui-get-selection 'CLIPBOARD 'TEXT)))
    (google-translate-translate source target text output)))

(map! :after google-translate
      :leader
      (:prefix ("l" . "translate")
       :desc "Translate buffer"
       "b" 'google-translate-buffer
       :desc "Translate at point"
       "w" 'google-translate-at-point
       :desc "Translate query"
       "q" 'google-translate-query-translate
       :desc "Translate smooth"
       "s" 'google-translate-smooth-translate
       :desc "Translate query reverse"
       "e" 'google-translate-query-translate-reverse
       :desc "Translate clipboard"
       "c" 'my/google-translate-clipboard))

(defvar my/additional-lookup-prividers
  '(("Cambridge dictionary" "https://dictionary.cambridge.org/dictionary/english/%s")
    ("Urban dictionary" "https://www.urbandictionary.com/define.php?term=%s")))

(dolist (provider my/additional-lookup-prividers)
  (add-to-list '+lookup-provider-url-alist provider))

(defun my/lookup-interpretation
    (&optional _ text &rest _)
  "Look up interpretation of text."
  (interactive
   (list current-prefix-arg
         (minibuffer-with-setup-hook
             (lambda (&rest _)
               (set-input-method "ukrainian-computer"))
           (read-string
            (propertize "Look up interpretation of: " 'face 'success)))))
  (let ((url "https://slovnyk.ua/index.php?swrd=%s"))
    (browse-url-with-browser-kind 'external (format url text))))

(map! :leader :desc "Look up interpretation"
      "lv" 'my/lookup-interpretation)
