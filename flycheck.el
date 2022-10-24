(flycheck-define-checker ansible
  "https://ansible-lint.readthedocs.io/en/latest/usage.html"

  :command ("ansbile-lint" "-pq" source-inplace)
  :error-patterns
    ((error line-start (file-name) ":" line ": error: " (message) line-end))
  :modes yaml-mode ansible)

(use-package flycheck
  :ensure t
  :config
  (setq
   flycheck-flake8rc ".flake8"
   flycheck-python-flake8-executable "flake8"
   flycheck-python-pylint-executable "pylint"
   flycheck-ansible-executable "ansible-lint"
   flycheck-yamllintrc "yammlint"
   flycheck-dockerfile-hadolint-executable "/home/starenka/.local/bin/hadolint"
   flycheck-css-stylelint-executable "npx stylelint"
   flycheck-ansible-executable "ansible-lint")
  (setq-default flycheck-disabled-checkers
  '(emacs-lisp-checkdoc ;; dont yell about missing docs in el files
    python-pycompile
    python-mypy
    python-pyright))
  :hook
  ((css-mode . flycheck-mode)
   (emacs-lisp-mode . flycheck-mode)
   (sh-mode-hook . flycheck-mode)
   (lua-mode-hook . flycheck-mode)
   (dockerfile-mode . flycheck-mode)
   (yaml-mode . flycheck-mode)
   (python-mode-hook . flycheck-mode)
   (ansible-mode . flycheck-mode)))


(eval-after-load 'flymake '(require 'flymake-cursor))
