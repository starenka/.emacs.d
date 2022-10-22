(use-package py-autopep8
  :ensure t
  :config
  (setq py-autopep8-options '("--max-line-length=120"
                              "--ignore=E123,E133,E226,E241,E242,E251,E221"
                              "--pep8-passes=5000"))
  :hook ((python-mode-hook) . py-autopep8-mode))

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
