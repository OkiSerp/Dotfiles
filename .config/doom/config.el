;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(add-to-list 'default-frame-alist '(width . 110))
(add-to-list 'default-frame-alist '(height . 40))

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
(let ((size 18)
      (font "JetBrains Mono"))
  (when (doom-font-exists-p font)
    (setq doom-font (font-spec :name font :size size)
          doom-big-font (font-spec :name font :size (+ 6 size)))))

(let ((font "Noto Color Emoji"))
  (when (doom-font-exists-p font)
    (setq doom-emoji-font (font-spec :name font))))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.
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

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; (pixel-scroll-precision-mode 1)

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

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
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

(defun serp/org-enlarge-headlines (&rest _)
  "Make org headings larger and thicker."
  (dolist (face '((org-level-1 . 1.5) (org-level-2 . 1.4)
                  (org-level-3 . 1.3) (org-level-4 . 1.3)
                  (org-level-5 . 1.3) (org-level-6 . 1.3)
                  (org-level-7 . 1.3) (org-level-8 . 1.3)))
    (set-face-attribute (car face) nil :weight 'heavy :height (cdr face))))

(add-hook 'org-mode-hook 'serp/org-enlarge-headlines)

(defun serp/org-insert-heading-fn (&rest _)
  "Add one line above newly created org heading."
  (evil-open-above 1)
  (evil-normal-state)
  (evil-next-line)
  (evil-append-line 1))

(add-hook 'org-insert-heading-hook 'serp/org-insert-heading-fn)

(defun serp/org-meta-return (&optional arg)
  "Make the same logic as `org-meta-return', but better."
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

(map! :map vertico-map
      "M-k" 'vertico-previous
      "M-j" 'vertico-next)

(map! :map company-active-map
      "M-k" 'company-select-previous
      "M-j" 'company-select-next)

(map! :map read-expression-map
      "M-k" 'previous-line-or-history-element
      "M-j" 'next-line-or-history-element)

(map! :nvi "M-k" 'previous-buffer
      :nvi "M-j" 'next-buffer
      :nvi "M-i" 'ibuffer)

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

(map! :leader :desc "Toggle input method"
      "ti" 'toggle-input-method)

(use-package! google-translate
  :init
  (setq google-translate-preferable-input-methods-alist
        `((nil) (,default-input-method . ("uk" "ru"))))
  :config
  (set-face-attribute
   'google-translate-listen-button-face nil :height 1.0)
  (setq google-translate-listen-button-label "[Play]"
        google-translate-default-source-language "en"
        google-translate-default-target-language "uk"
        google-translate-pop-up-buffer-set-focus t
        google-translate-output-destination 'help
        google-translate-listen-program "mplayer"
        google-translate-backend-method 'curl
        google-translate-show-phonetic t
        google-translate-display-translation-phonetic nil
        google-translate-input-method-auto-toggling t))

(map! :after google-translate
      :when (eql google-translate-output-destination 'help)
      :map help-mode-map
      :n "p" (cmd! (forward-button 1) (push-button))
      :n "P" (cmd! (backward-button 1) (push-button)))

(map! :leader (:prefix ("l" . "translate")))

(after! google-translate
  (map! :leader :desc "Translate buffer"
        "lb" 'google-translate-buffer)
  (map! :leader :desc "Translate at point"
        "lw" 'google-translate-at-point)
  (map! :leader :desc "Translate smooth"
        "ls" 'google-translate-smooth-translate)
  (map! :leader :desc "Translate query"
        "lq" 'google-translate-query-translate))

(map! :after google-translate
      :leader :desc "Play translation"
      "le" (cmd! (google-translate-listen-translation
                  google-translate-default-source-language
                  (doom-thing-at-point-or-region))))

(after! google-translate
  (map! :leader :desc "Translate at point reverse"
        "lW" (cmd! (setq google-translate-show-phonetic nil)
                   (google-translate-at-point-reverse)
                   (setq google-translate-show-phonetic t))))

(after! google-translate
  (map! :leader :desc "Translate query reverse"
        "lQ" (cmd! (setq google-translate-show-phonetic nil)
                   (google-translate-query-translate-reverse)
                   (setq google-translate-show-phonetic t))))

(defun serp/google-translate-from-clipboard (&rest _)
  "Translate text from clipboard by using `google-translate' package."
  (interactive)
  (let ((source google-translate-default-source-language)
        (target google-translate-default-target-language)
        (output google-translate-output-destination)
        (text (gui-get-selection 'CLIPBOARD)))
    (google-translate-translate source target text output)))

(map! :after google-translate
      :leader :desc "Translate from clipboard"
      "lf" 'serp/google-translate-from-clipboard)

(dolist
    (provider
     '(("Cambridge dictionary"
        "https://dictionary.cambridge.org/dictionary/english/%s")
       ("Slovnyk" "https://slovnyk.ua/index.php?swrd=%s")))
  (add-to-list '+lookup-provider-url-alist provider))

(map! :leader :desc "Cambridge dictionary" "lc"
      (cmd! (+lookup/online
             (doom-thing-at-point-or-region) "Cambridge dictionary")))

(defun serp/lookup (url &optional prompt im &rest _)
  "Look up query via prompt using minibuffer.\n
A search URL (needs on '%s' to substitute with an url encoded query).
If PROMPT is passed, use it instead of default value.
Third option IM is input method to be used. If true than use
`default-input-method', but when it's nil, do nothing.\n
FIXME: Can't execute via `M-x'."
  (interactive)
  (let ((prompt (cond (prompt) ("Search for » ")))
        (im (cond ((stringp im) im) ((eql im t) default-input-method))))
    (minibuffer-with-setup-hook
        (lambda (&rest _)
          (unless (null im)
            (set-input-method im)))
      (browse-url-default-browser
       (format url (read-string prompt))))))

(map! :leader :desc "Look up Slovnyk"
      "lv" (cmd! (serp/lookup
                  "https://slovnyk.ua/index.php?swrd=%s"
                  "Search for ⇒ " t)))

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
