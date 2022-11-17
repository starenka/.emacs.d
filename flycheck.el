;; apt install ansible-lint
;; pip install yammlint --user
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
  ((emacs-lisp-mode . flycheck-mode)
   (sh-mode . flycheck-mode)
   (lua-mode-hook . flycheck-mode)
   (yaml-mode . flycheck-mode)
   (ansible-mode . flycheck-mode)
   (dockerfile-mode . flycheck-mode)
   (css-mode . flycheck-mode))
  :init
  (add-hook 'python-mode-hook 'flycheck-mode)
  )

(flycheck-define-checker ansible
  "https://ansible-lint.readthedocs.io/en/latest/usage.html"

  :command ("ansbile-lint" "-pq" source-inplace)
  :error-patterns
    ((error line-start (file-name) ":" line ": error: " (message) line-end))
  :modes yaml-mode ansible)

(eval-after-load 'flymake '(require 'flymake-cursor))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode t))))


