;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq frame-title-format nil)

(setq confirm-kill-emacs nil)

(defun serp/delete-frame (&rest _)
  "Delete frame without prompt."
  (interactive)
  (if (cdr (visible-frame-list))
      (delete-frame)
    (save-buffers-kill-terminal)))

(bind-key [remap delete-frame] 'serp/delete-frame)

(defun serp/add-blur-behind-x-frame (&optional frame &rest _)
  "Set blur behind `x' frame.\n
If FRAME in nil, use current frame."
  (interactive)
  (let* ((frame (cond (frame) (t (selected-frame))))
         (frame-id (frame-parameter frame 'outer-window-id))
         (command (format
                   "xprop %s %s %s"
                   "-f _KDE_NET_WM_BLUR_BEHIND_REGION 32c"
                   "-set _KDE_NET_WM_BLUR_BEHIND_REGION 0 -id"
                   frame-id)))
    (when (eql (window-system) 'x)
      (call-process-shell-command command)
      (set-frame-parameter frame 'blur 1))))

(defun serp/remove-blur-behind-x-frame (&optional frame &rest _)
  "Remove blur behind `x' frame.\n
If FRAME is nil, use current frame."
  (interactive)
  (let* ((frame (cond (frame) (t (selected-frame))))
         (frame-id (frame-parameter frame 'outer-window-id))
         (command (format
                   "xprop -remove _KDE_NET_WM_BLUR_BEHIND_REGION -id %s"
                   frame-id)))
    (when (eql (window-system) 'x)
      (call-process-shell-command command)
      (set-frame-parameter frame 'blur 0))))

(defun serp/toggle-blur-behind-x-frame (&optional frame &rest _)
  "Toggle blur behind `x' frame.\n
If FRAME is nil, use current frame."
  (interactive)
  (let* ((frame (cond (frame) (t (selected-frame))))
         (blur (frame-parameter frame 'blur)))
    (if (or (eql blur nil) (<= blur 0))
        (serp/add-blur-behind-x-frame frame)
      (serp/remove-blur-behind-x-frame frame))))

(defun serp/add-blur-behind-new-x-frame-on-switch (&rest _)
  "Set blur behind newly created `x' frame.\n
NOTE: the function works perfectly on frame switch."
  (let ((blur (frame-parameter (selected-frame) 'blur)))
    (when (eql blur nil)
      (serp/add-blur-behind-x-frame))))

(add-to-list 'default-frame-alist '(alpha-background . 90))

(add-hook 'window-setup-hook 'serp/add-blur-behind-x-frame)

(add-hook! 'window-selection-change-functions
  (serp/add-blur-behind-new-x-frame-on-switch))

(map! :leader :desc "Blur behind frame"
      "tu" 'serp/toggle-blur-behind-x-frame)

(setq user-full-name "Oleksii Kapula"
      user-mail-address "")

(add-to-list 'default-frame-alist '(width . 110))
(add-to-list 'default-frame-alist '(height . 36))

(let ((size 18)
      (font "JetBrains Mono"))
  (when (doom-font-exists-p font)
    (setq doom-font (font-spec :name font :size size)
          doom-big-font (font-spec :name font :size (+ 6 size)))))

(let ((font "Noto Color Emoji"))
  (when (doom-font-exists-p font)
    (setq doom-emoji-font (font-spec :name font))))

(setq doom-theme 'doom-one)

(add-hook! '(prog-mode-hook conf-mode-hook)
  (face-remap-add-relative 'font-lock-comment-face :slant 'italic))

(defun serp/doom-dashboard-widget-quote ()
  (when doom-init-time
    (insert
     (propertize
      (+doom-dashboard--center
       +doom-dashboard--width
       "Don't let the evil dwell in your system.")
      'face 'doom-dashboard-banner))))

(setq +doom-dashboard-functions
      '(serp/doom-dashboard-widget-quote))

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
              indent-tabs-mode nil)

(after! fish-mode
  (setq fish-indent-offset 2))

(setq org-directory "~/.orgfiles/")

(after! org
  (setq org-ellipsis " …"
        org-log-done 'time
        org-startup-folded 'fold
        org-image-actual-width nil
        org-hide-emphasis-markers t
        org-appear-autoemphasis nil))

(after! org
  (setq org-superstar-headline-bullets-list '(42)
        org-superstar-item-bullet-alist '((43 . 187) (45 . 8250))))

(defun serp/browse-org-directory (&rest _)
  "Browse your `org-directory'."
  (interactive)
  (unless (file-directory-p org-directory)
    (make-directory org-directory t))
  (doom-project-browse org-directory))

(map! :leader :desc "Browse org directory"
      "fo" 'serp/browse-org-directory)

(setq org-emphasis-regexp-components
      '("-[:space:]('\"{" "-[:space:].,:!?;'\")}\\[‼…" "[:space:]" "." 1))

(defun serp/org-enlarge-headings (&rest _)
  "Make org headings larger and thicker."
  (dolist (face '((org-level-1 . 1.5) (org-level-2 . 1.4)
                  (org-level-3 . 1.3) (org-level-4 . 1.3)
                  (org-level-5 . 1.3) (org-level-6 . 1.3)
                  (org-level-7 . 1.3) (org-level-8 . 1.3)))
    (set-face-attribute (car face) nil :weight 'heavy :height (cdr face))))

(add-hook 'org-mode-hook 'serp/org-enlarge-headings)

(defun serp/org-insert-heading-fn (&rest _)
  "Add one line above newly created org heading."
  (evil-open-above 1)
  (evil-normal-state)
  (evil-next-line)
  (evil-append-line 1))

(add-hook 'org-insert-heading-hook 'serp/org-insert-heading-fn)

(defun serp/org-meta-return (&optional arg)
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
      "M-RET" 'serp/org-meta-return
      :n "RET" '+org/dwim-at-point)

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
      :desc "Find file in private config"
      "fP" 'doom/find-file-in-private-config)

(map! "M-s" 'save-buffer
      "M-q" 'kill-current-buffer)

(map! :nvi "M-k" 'previous-buffer
      :nvi "M-j" 'next-buffer
      :nvi "M-i" 'ibuffer)

(map! :map vertico-map
      "M-k" 'vertico-previous
      "M-j" 'vertico-next)

(customize-set-variable 'company-box-scrollbar nil)

(map! :map company-active-map
      "M-k" 'company-select-previous
      "M-j" 'company-select-next)

(map! :map read-expression-map
      "M-k" 'previous-line-or-history-element
      "M-j" 'next-line-or-history-element)

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

(setq default-input-method "ukrainian-computer")

(when (modulep! :checkers spell)
  (add-hook! 'input-method-activate-hook
    (spell-fu-mode 0)))

(use-package! google-translate
  :init
  (setq google-translate-preferable-input-methods-alist
        `((nil) (,default-input-method . ("uk" "ru"))))
  :config
  (set-face-attribute
   'google-translate-listen-button-face nil :height 1.0)
  (setq google-translate-listen-button-label "[Listen]"
        google-translate-default-source-language "en"
        google-translate-default-target-language "uk"
        google-translate-pop-up-buffer-set-focus t
        google-translate-output-destination 'help
        google-translate-listen-program "mplayer"
        google-translate-backend-method 'curl
        google-translate-show-phonetic t
        google-translate-display-translation-phonetic nil
        google-translate-input-method-auto-toggling t))

(defun serp/google-translate-clipboard (&rest _)
  "Translate text from clipboard by using `google-translate' package."
  (interactive)
  (let ((source google-translate-default-source-language)
        (target google-translate-default-target-language)
        (output google-translate-output-destination)
        (text (gui-get-selection 'CLIPBOARD 'TEXT)))
    (google-translate-translate source target text output)))

(defun serp/google-translate-query-reverse (&rest _)
  "Like `google-translate-query-translate-reverse', but do not show
the phonetic spell."
  (interactive)
  (setq google-translate-show-phonetic nil)
  (google-translate-query-translate-reverse)
  (setq google-translate-show-phonetic t))

(map! :after google-translate
      :leader
      (:prefix ("l" . "translate")
       :desc "Translate at point"
       "w" 'google-translate-at-point
       :desc "Translate query"
       "q" 'google-translate-query-translate
       :desc "Translate smooth"
       "s" 'google-translate-smooth-translate
       :desc "Translate clipboard"
       "f" 'serp/google-translate-clipboard
       :desc "Translate query reverse"
       "e" 'serp/google-translate-query-reverse))

(dolist (provider
         '(("Cambridge dictionary"
            "https://dictionary.cambridge.org/dictionary/english/%s")))
  (add-to-list '+lookup-provider-url-alist provider))

(defun serp/lookup (url &optional query prompt im &rest _)
  "TODO: Look up query via prompt using minibuffer, …"
  (let ((prompt (cond (prompt) ("Search for: ")))
        (im (cond ((stringp im) im) (im default-input-method))))
    (minibuffer-with-setup-hook
        (lambda (&rest _)
          (unless (null im)
            (set-input-method im)))
      (browse-url-default-browser
       (format url (cond (query (read-string prompt))
                         (t (doom-thing-at-point-or-region))))))))

(map! :leader :desc "Slovnyk"
      "lv" (cmd! (serp/lookup "https://slovnyk.ua/index.php?swrd=%s"
                              'query nil 'input-method)))
