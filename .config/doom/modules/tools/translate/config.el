;;; tools/translate/config.el -*- lexical-binding: t; -*-

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

(when (modulep! :tools lookup)
    (dolist
        (provider
         '(("Cambridge dictionary"
            "https://dictionary.cambridge.org/dictionary/english/%s")
           ("Google translate"
            "https://translate.google.com/?sl=en&tl=uk&text=%s&op=translate")
           ("Deepl" "https://www.deepl.com/translator#en/ru/%s")))
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
    (map! :leader :desc "Google translate from CB" "lG"
          (cmd! (+lookup/online
                 (gui-get-selection 'CLIPBOARD) "Google translate")))
    (map! :leader :desc "Deepl translate" "ld"
          (cmd! (+lookup/online
                 (doom-thing-at-point-or-region) "Deepl")))
    (map! :leader :desc "Deepl translate from CB" "ld"
          (cmd! (+lookup/online
                 (gui-get-selection 'CLIPBOARD) "Deepl"))))
