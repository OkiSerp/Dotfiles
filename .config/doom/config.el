;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(map! :leader :desc "Browse private config"
      "fp" 'doom/open-private-config
      :desc "Find file in private config"
      "fP" 'doom/find-file-in-private-config)

;; Frames seem to be a bit small. So we're going to fix it:
(add-to-list 'default-frame-alist '(width . 126))
(add-to-list 'default-frame-alist '(height . 34))

;; Hold `Alt/Meta' and press `RMB' to copy selection
(bind-key [M-mouse-3] 'clipboard-kill-ring-save)

;; Fed up with confirmation?
(setq confirm-kill-emacs nil)

(defun serp/delete-frame (&rest _)
  "Delete frame without prompt."
  (interactive)
  (if (cdr (visible-frame-list))
      (delete-frame)
    (save-buffers-kill-terminal)))

(bind-key [remap delete-frame] 'serp/delete-frame)

;; Can't leave without blurred frames. Not true…
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
(setq user-full-name "Alex Kapula"
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

;; Get rid of bloated dashboard.
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
(setq display-line-numbers-type nil
      scroll-margin 7)

;; You should care about indentations!
(setq-default tab-width 2
              indent-tabs-mode nil)

(after! fish-mode
  (setq fish-indent-offset 2))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/.orgfiles/")

(after! org
  (setq org-log-done t
        org-ellipsis " …"
        org-startup-folded 'fold
        org-image-actual-width nil
        org-hide-emphasis-markers t
        org-appear-autoemphasis nil))

(after! org
  (setq org-superstar-headline-bullets-list '(9679)
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

(after! org
  (evil-set-register ?w [?A ?* escape ?^ ?w ?i ?* escape ?j])
  (evil-set-register ?e [?A ?/ escape ?^ ?w ?i ?/ escape ?j])
  (evil-set-register ?r [?v ?i ?w escape ?a ?_ escape ?b ?i ?_ escape ?w])
  (evil-set-register ?f [?v ?i ?w escape ?a ?+ escape ?b ?i ?+ escape ?w]))

;; Make sure to turn off spell checker after opening my rhymes.
(add-hook! 'find-file-hook
  (let ((wbuf (expand-file-name "Org/rhymes.org" (getenv "HOME")))
        (cbuf (buffer-file-name (current-buffer))))
    (when (and (string-equal-ignore-case wbuf cbuf) spell-fu-mode)
      (spell-fu-mode 0))))

;; When you almost `evil' by default.
(use-package! evil
  :init
  (setq evil-disable-insert-state-bindings t
        evil-kill-on-visual-paste nil
        evil-want-fine-undo t))

(after! evil
  (map! :nv "'" 'evil-jump-item))

(after! evil-snipe
  (setq evil-snipe-scope 'visible))

;; Make `M-j/k' keys your friend.
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

;; Create `dired' prefix map.
(map! :leader
      (:prefix ("d" . "dired")
       :n "j" 'dired-jump
       :n "o" 'dired-jump-other-window))

(map! :map dired-mode-map
      :n "h" 'dired-up-directory
      :n "l" 'dired-find-alternate-file)

;; Be careful with your trash.
(setq trash-directory "~/.local/share/Trash/files/"
      delete-by-moving-to-trash t
      magit-delete-by-moving-to-trash t)

;; That's how neck-beards communicate.
(use-package! elfeed
  :hook (elfeed-search-mode . elfeed-update)
  :config
  (setq elfeed-search-filter "@4-month-ago +unread"))

(map! :leader :desc "RSS" "oe" 'elfeed)

(setq default-input-method "ukrainian-computer")

(defun serp/toggle-input-method (&rest _)
  "Toggle input method with message."
  (interactive)
  (toggle-input-method nil t)
  (if (null evil-input-method)
      (message "Back to normal!")
    (message "Ukrainian input method!")))

(bind-key [remap toggle-input-method] 'serp/toggle-input-method)

(map! :leader (:prefix ("l" . "translate")))

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
  (map! :leader :desc "Translate buffer"
        "lb" 'google-translate-buffer)
  (map! :leader :desc "Translate at point"
        "lw" 'google-translate-at-point)
  (map! :leader :desc "Translate at point reverse"
        "lW" 'google-translate-at-point-reverse)
  (map! :leader :desc "Translate smooth"
        "ls" 'google-translate-smooth-translate)
  (map! :leader :desc "Translate query"
        "lq" 'google-translate-query-translate)
  (map! :leader :desc "Translate query reverse"
        "lQ" 'google-translate-query-translate-reverse))

(after! google-translate
  (map! :leader :desc "Change source lang to ru" "lr"
        (cmd! (setq google-translate-default-target-language "ru")))
  (map! :leader :desc "Change source lang to uk" "lu"
        (cmd! (setq google-translate-default-target-language "uk"))))

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

(defun serp/google-translate-buffer-reverse (&rest _)
  "Translate current buffer, but switch target and source languages
by using `google-translate' package."
  (interactive)
  (let ((source google-translate-default-source-language)
        (target google-translate-default-target-language))
    (setq google-translate-default-source-language target
          google-translate-default-target-language source)
    (google-translate-buffer)
    (setq google-translate-default-source-language source
          google-translate-default-target-language target)))

(map! :after google-translate
      :leader :desc "Translate buffer reverse"
      "lB" 'serp/google-translate-buffer-reverse)

(dolist
    (provider
     '(("Cambridge dictionary"
        "https://dictionary.cambridge.org/dictionary/english/%s")))
  (add-to-list '+lookup-provider-url-alist provider))

(map! :leader :desc "Cambridge dictionary" "lc"
      (cmd! (+lookup/online
             (doom-thing-at-point-or-region) "Cambridge dictionary")))

(map! :leader :desc "Cambridge dictionary from CB" "lC"
      (cmd! (+lookup/online
             (gui-get-selection 'CLIPBOARD) "Cambridge dictionary")))

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
