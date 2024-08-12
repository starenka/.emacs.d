(use-package python
  :ensure t
  :bind (:map python-mode-map
              ("M-i" . python-add-import)))

(add-hook 'python-mode-hook (lambda () ;; M-x devdocs-install
                              (setq-local devdocs-current-docs '(
                                                                 "django~4.2"
                                                                 ;;"elisp"
                                                                 "html"
                                                                 "http"
                                                                 ;;"javascript"
                                                                 "lua~5.4"
                                                                 "postgresql~15"
                                                                 "python~3.11"
                                                                 "numpy~1.23"
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
