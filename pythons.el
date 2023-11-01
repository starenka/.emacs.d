(use-package py-autopep8
  :ensure t
  :pin melpa
  :hook ((python-mode) . py-autopep8-mode))

;;(use-package reformatter
;;  :hook
;;  (python-mode . ruff-format-on-save-mode)
;;  (python-ts-mode . ruff-format-on-save-mode)
;;  :config
;;  (reformatter-define ruff-format
;;    :program "ruff"
;;    :args `("format" "--stdin-filename" ,buffer-file-name "-")))

(use-package reformatter
  :hook
  (python-mode . autopep8-on-save-mode)
  (python-ts-mode . autopep8-on-save-mode)
  :config
  (reformatter-define autopep8-format
    :program "autopep8"
    :args '("--in-place" "--hang-closing" "--aggresive 2" "--jobs 0" "--pep8-passes 10000" "-v" buffer-file-name)))

(add-hook 'python-mode-hook (lambda () ;; M-x devdocs-install
                              (setq-local devdocs-current-docs '(
                                                                 "django~3.2"
                                                                 ;;"elisp"
                                                                 "html"
                                                                 "http"
                                                                 ;;"javascript"
                                                                 "lua~5.4"
                                                                 "postgresql~15"
                                                                 "python~3.11"
                                                                 "redis"))))


(use-package pyvenv
  :ensure t
  :diminish
  :init
  (setenv "WORKON_HOME" "/data/.envs")
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode 1)
  ;;(pyvenv-tracking-mode 1) ;; this kills cpu bigtime
  )

(setq python-indent-guess-indent-offset nil
      python-indent-guess-indent-offset-verbose nil)

;; django templates
;;(use-package pony-mode :ensure t)
