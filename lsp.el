;; apt install python3-pylsp
(use-package lsp-mode
  :ensure t
  :init
  :hook ((python-mode . lsp-deferred)
         (lua-mode . lsp-deferred)
         (clojure-mode . lsp-deferred) ;; lsp-install-server ...
         (java-mode . lsp-deferred)
         (rust-mode . lsp-deferred) ;; $ rustup component add rust-analyzer
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq
   lsp-auto-guess-root t
   ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
   lsp-modeline-diagnostics-mode nil
   lsp-headerline-breadcrumb-enable nil
   lsp-modeline-code-actions-enable nil
   lsp-ui-sideline-show-code-actions nil
   lsp-ui-doc-enable nil
   lsp-diagnostics-disabled-modes '(python-mode) ;; let me do flycheck myself
   lsp-file-watch-threshold 2000
   ;; pylsp
   ;; mkvirtualenv pylsp && pip install 'python-lsp-server[all]' python-lsp-ruff python-lsp-isort pylsp-rope
   ;; workon pylsp && pip install -U 'python-lsp-server[all]' python-lsp-ruff python-lsp-isort pylsp-rope

   lsp-pylsp-server-command "/data/.envs/pylsp/bin/pylsp"
   lsp-pylsp-plugins-ruff.enabled t
   lsp-pylsp-configuration-sources ["flake8"]
   lsp-pylsp-plugins-pycodestyle-enabled nil
   lsp-pylsp-plugins-pydocstyle-enabled nil
   lsp-pylsp-plugins-mccabe-enabled nil
   lsp-pylsp-plugins-autopep8-enabled nil
   ;;lsp-pylsp-plugins-jedi-use-pyenv-environment t ;; just stick venv dir in your project

   lsp-completion-provider :none ; Disable default completion
   )
  :bind (
         ("C-c g" . lsp-find-definition)
         ("C-c f" . xref-find-definitions-other-window)
         ("C-c r" . lsp-ui-peek-find-references)
         ("C-c d" . lsp-describe-thing-at-point)
         ("C-c h" . lsp-ui-doc-glance))
  :commands lsp)

(use-package lsp-ui :ensure t :commands lsp-ui-mode)
