(defun lsp-py-install-save-hooks ()
  ;;(add-hook 'before-save-hook #'lsp-organize-imports t t)
  (add-hook 'before-save-hook #'lsp-format-buffer t t))

(add-hook 'python-mode-hook 'lsp-py-install-save-hooks)
(add-hook 'python-mode-hook (lambda ()
                              (setq-local devdocs-current-docs '(
                                                                 "bash"
                                                                 "django~3.2"
                                                                 ;;"elisp"
                                                                 ;;"html"
                                                                 "http"
                                                                 ;;"javascript"
                                                                 ;;"lua~5.4"
                                                                 "postgresql~15"
                                                                 ;;"python~3.9"
                                                                 "python~3.10"
                                                                 ;;"python~3.11"
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
(use-package pony-mode :ensure t)
