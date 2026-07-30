;; apt install ansible-lint
;; pip install yammlint --user
(use-package flycheck
  :ensure t
  :config
  (setq
   flycheck-python-ruff-executable "ruff"
   ;;flycheck-python-ruff-executable (expand-file-name "~/.local/bin/ruff")
   flycheck-ansible-executable "ansible-lint"
   flycheck-yamllintrc "yamllint"
   flycheck-dockerfile-hadolint-executable "/home/starenka/.local/bin/hadolint"
   flycheck-css-stylelint-executable "npx stylelint")
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp-checkdoc ;; dont yell about missing docs in el files
                  python-pylint
                  python-flake8
                  python-pycompile
                  python-mypy
                  python-pyright))
  :hook
  ((emacs-lisp-mode . flycheck-mode)
   (python-mode . flycheck-mode)
   (python-ts-mode . flycheck-mode)
   (sh-mode . flycheck-mode)
   (lua-mode . flycheck-mode)
   (yaml-mode . flycheck-mode)
   (ansible-mode . flycheck-mode)
   (dockerfile-mode . flycheck-mode)
   (css-mode . flycheck-mode)))

(defun vitek/python-flycheck-ruff ()
  (setq-local flycheck-checker 'python-ruff))

(dolist (hook '(python-mode-hook python-ts-mode-hook))
  (add-hook hook #'vitek/python-flycheck-ruff))

(flycheck-define-checker ansible
  "https://ansible-lint.readthedocs.io/en/latest/usage.html"

  :command ("ansible-lint" "-pq" source-inplace)
  :error-patterns
    ((error line-start (file-name) ":" line ": error: " (message) line-end))
  :modes yaml-mode ansible)

(use-package flymake-cursor :ensure t)
(use-package flymake
  :ensure t
  :config
  (require 'flymake-cursor))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode t))))
