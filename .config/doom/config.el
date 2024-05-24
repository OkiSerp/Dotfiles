;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq confirm-kill-emacs nil)

(defun srp/delete-frame (&rest _)
  "Delete frame without prompt."
  (interactive)
  (if (cdr (visible-frame-list))
      (delete-frame)
    (save-buffers-kill-terminal)))

(bind-key [remap delete-frame] 'srp/delete-frame)

(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 40))

(defvar srp/font-family "Iosevka"
  "Default font family for `srp/font-load' function.")

(defvar srp/font-size 18
  "Default font size for `srp/font-load' function.")

(defun srp/font-load
    (&optional _ family size big-size &rest _)
  "Configure font family and its size both explicitly or interactively."
  (interactive
   (list current-prefix-arg
         (read-string "Font family: " srp/font-family)
         (read-number "Font size: " srp/font-size)))
  (let* ((family (cond (family) (srp/font-family)))
         (size (cond (size) (srp/font-size)))
         (big-size (cond (big-size) (t (+ 6 size)))))
    (if (doom-font-exists-p family)
        (progn
          (setq doom-font (font-spec :family family :size size)
                doom-big-font (font-spec :family family :size big-size))
          (when (called-interactively-p 'any)
            (doom/reload-font)))
      (message "%s" (propertize (format "%s doesn't exists!" family)
                                'face 'warning)))))

(map! :leader "hrF" 'srp/font-load)

(srp/font-load)

(let ((font-family "Noto Color Emoji"))
  (when (doom-font-exists-p font-family)
    (setq doom-emoji-font (font-spec :name font-family))))

(setq doom-theme 'doom-one)

(add-hook! '(prog-mode-hook conf-mode-hook)
  (face-remap-add-relative 'font-lock-comment-face :slant 'italic))

(defun srp/doom-dashboard-widget-quote ()
  (when doom-init-time
    (insert
     (propertize
      (+doom-dashboard--center
       +doom-dashboard--width
       "Don't let the evil dwell in your system.")
      'face 'doom-dashboard-banner))))

(setq +doom-dashboard-functions
      '(srp/doom-dashboard-widget-quote))

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

(setq org-directory "~/.orgnotes/")

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

(defun srp/browse-org-directory (&rest _)
  "Browse your `org-directory'."
  (interactive)
  (unless (file-directory-p org-directory)
    (make-directory org-directory t))
  (doom-project-browse org-directory))

(map! :leader :desc "Browse org directory"
      "fo" 'srp/browse-org-directory)

(setq org-emphasis-regexp-components
      '("-[:space:]('\"{" "-[:space:].,:!?;'\")}\\[‼…" "[:space:]" "." 1))

(defun srp/org-enlarge-headings (&rest _)
  "Make org headings larger and thicker."
  (dolist (face '((org-level-1 . 1.5) (org-level-2 . 1.4)
                  (org-level-3 . 1.3) (org-level-4 . 1.3)
                  (org-level-5 . 1.3) (org-level-6 . 1.3)
                  (org-level-7 . 1.3) (org-level-8 . 1.3)))
    (set-face-attribute (car face) nil :weight 'heavy :height (cdr face))))

(add-hook 'org-mode-hook 'srp/org-enlarge-headings)

(defun srp/org-insert-heading-fn (&rest _)
  "Add one line above newly created org heading."
  (evil-open-above 1)
  (evil-normal-state)
  (evil-next-line)
  (evil-append-line 1))

(add-hook 'org-insert-heading-hook 'srp/org-insert-heading-fn)

(defun srp/org-meta-return (&optional arg)
  "Make the same logic as `org-meta-return', but a bit better."
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
      "M-RET" 'srp/org-meta-return
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
  (map! :nv "'" 'evil-jump-item))

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

(map! :map minibuffer-mode-map
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

(when (modulep! :checkers spell)
  (spell-fu-global-mode 0)
  (remove-hook 'text-mode-hook 'spell-fu-mode)
  (add-hook! 'input-method-activate-hook
    (spell-fu-mode 0)))

(defvar srp/im "ukrainian-computer"
  "Define variable for my input method.")

(setq default-input-method srp/im)

(use-package! google-translate
  :init
  (setq google-translate-preferable-input-methods-alist
        `((nil) (,srp/im . ("uk" "ru"))))
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

(defun srp/google-translate-clipboard (&rest _)
  "Translate text from clipboard by using `google-translate' package."
  (interactive)
  (let ((source google-translate-default-source-language)
        (target google-translate-default-target-language)
        (output google-translate-output-destination)
        (text (gui-get-selection 'CLIPBOARD 'TEXT)))
    (google-translate-translate source target text output)))

(defun srp/google-translate-listen-at-point (&rest _)
  "Listen the word at point or the words in the active region by using
`google-translate' package."
  (interactive)
  (google-translate-listen-translation
   google-translate-default-source-language
   (doom-thing-at-point-or-region)))

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
       "c" 'srp/google-translate-clipboard
       :desc "Listen at point"
       "r" 'srp/google-translate-listen-at-point))

(defvar srp/additional-lookup-prividers
  '(("Cambridge dictionary" "https://dictionary.cambridge.org/dictionary/english/%s")
    ("Urban dictionary" "https://www.urbandictionary.com/define.php?term=%s")))

(dolist (provider srp/additional-lookup-prividers)
  (add-to-list '+lookup-provider-url-alist provider))

(defun srp/lookup-interpretation
    (&optional _ word &rest _)
  "Look up interpretation of a word."
  (interactive
   (list current-prefix-arg
         (minibuffer-with-setup-hook
             (lambda (&rest _)
               (set-input-method "ukrainian-computer"))
           (read-string
            (propertize "Look up interpretation of: " 'face 'warning)))))
  (let ((url "https://slovnyk.ua/index.php?swrd=%s"))
    (browse-url-default-browser (format url word))))

(map! :leader :desc "Look up interpretation"
      "lv" 'srp/lookup-interpretation)
