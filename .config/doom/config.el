;;; -*- lexical-binding: t; -*-

(use-package! catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin :no-confirm))

(after! catppuccin-theme
  (custom-set-faces!
    `(show-paren-match
      :background ,(catppuccin-get-color 'surface2))))

(let ((font "Mononoki"))
  (when (doom-font-exists-p font)
    (setq doom-font (font-spec :name font :size 18)
          doom-big-font (font-spec :name font :size 24))))

(let ((font "Noto Sans"))
  (when (doom-font-exists-p font)
    (setq doom-variable-pitch-font (font-spec :name font))))

(let ((font "Noto Color Emoji"))
  (when (doom-font-exists-p font)
    (setq doom-emoji-font (font-spec :name font))))

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

(use-package! olivetti
  :custom
  (olivetti-body-width 110)
  :config
  (add-hook! 'mixed-pitch-mode-hook
    (setq-local olivetti-body-width 80)))

(use-package! auto-olivetti
  :after olivetti
  :custom
  (auto-olivetti-enabled-modes
   '(text-mode prog-mode conf-mode help-mode
     helpful-mode ibuffer-mode nov-mode))
  :config
  (auto-olivetti-mode))

(use-package! evil
  :init
  (setq evil-disable-insert-state-bindings t
        evil-kill-on-visual-paste nil
        evil-want-fine-undo t))

(after! evil
  (map! :nv "'" 'evil-jump-item))

(after! evil-snipe
  (setq evil-snipe-scope 'visible))

(use-package! corfu
  :custom
  (corfu-auto t)
  (corfu-quit-at-boundary t)
  :init
  (setq completion-cycle-threshold 3
        tab-always-indent 'complete)
  :config
  (corfu-history-mode 1)
  (global-corfu-mode 1))

(map! :map corfu-map
      "M-k" 'corfu-previous
      "M-j" 'corfu-next)

(map! :map vertico-map
      "M-k" 'vertico-previous
      "M-j" 'vertico-next)

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

(setq trash-directory "~/.local/share/Trash/files/"
      delete-by-moving-to-trash t
      magit-delete-by-moving-to-trash t)

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

(map! :leader :desc "Cambridge dictionary from CB" "lC"
      (cmd! (+lookup/online
             (gui-get-selection 'CLIPBOARD) "Cambridge dictionary")))

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

(defun serp/google-translate-from-clipboard (&rest _)
  "Translate text from clipboard by using `google-translate' package."
  (interactive)
  (let ((source google-translate-default-source-language)
        (target google-translate-default-target-language)
        (output google-translate-output-destination)
        (text (gui-get-selection 'CLIPBOARD)))
    (google-translate-translate source target text output)))

(map! :after google-translate
      :leader :desc "Translate from clipboard" "lf"
      'serp/google-translate-from-clipboard)

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

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook
  ((nov-mode . mixed-pitch-mode)
   (nov-mode . visual-line-mode))
  :custom
  (nov-text-width t)
  (nov-variable-pitch nil)
  :config
  (add-hook! 'nov-mode-hook
    (face-remap-add-relative 'default :height 1.1)))

(map! :after nov
      :map nov-mode-map
      :nv "k" 'evil-previous-visual-line
      :nv "j" 'evil-next-visual-line
      :nv "0" 'evil-beginning-of-visual-line
      :nv "$" 'evil-end-of-visual-line)

(after! (org catppuccin-theme)
  (custom-set-faces!
    `(org-todo :foreground ,(catppuccin-get-color 'teal))
    `(org-verbatim :foreground ,(catppuccin-get-color 'yellow))))

(setq org-directory "~/OrgFiles/")

(after! org
  (setq org-log-done t
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
  (setq org-superstar-headline-bullets-list '(10033)
        org-superstar-item-bullet-alist '((43 . 8226) (45 . 9702))))

(defun serp/browse-org-directory (&rest _)
  "Browse your `org-directory'."
  (interactive)
  (unless (file-directory-p org-directory)
    (make-directory org-directory 'parents))
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

(map! :after (:or org evil-org)
      :map (evil-org-mode-map org-mode-map)
      "M-RET" 'serp/org-meta-return
      :n "RET" '+org/dwim-at-point)

(evil-set-register ?w [?A ?* escape ?^ ?w ?i ?* escape ?j])
(evil-set-register ?e [?A ?/ escape ?^ ?w ?i ?/ escape ?j])

(evil-set-register ?r [?v ?i ?w escape ?a ?_ escape ?b ?i ?_ escape ?w])

(setq confirm-kill-emacs nil)

(defun serp/delete-frame (&rest _)
  "Delete frame without prompt."
  (interactive)
  (if (cdr (visible-frame-list))
      (delete-frame)
    (save-buffers-kill-emacs)))

(map! :leader :desc "Delete frame" "qf"
      'serp/delete-frame)

(add-to-list 'default-frame-alist '(width . 105))
(add-to-list 'default-frame-alist '(height . 35))

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
Note: the function works perfectly on frame switch."
  (let ((blur (frame-parameter (selected-frame) 'blur)))
    (when (eql blur nil)
      (serp/add-blur-behind-x-frame))))

(add-to-list 'default-frame-alist '(alpha-background . 90))

(add-hook 'window-setup-hook 'serp/add-blur-behind-x-frame)

(add-hook! 'window-selection-change-functions
  (serp/add-blur-behind-new-x-frame-on-switch))

(map! :leader :desc "Blur behind frame" "tu"
      'serp/toggle-blur-behind-x-frame)
