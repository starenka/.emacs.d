;; apt install python3-pylsp
(use-package lsp-mode
  :ensure t
  :init
  :hook ((python-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred)
         (lua-mode . lsp-deferred)
         (clojure-mode . lsp-deferred) ;; lsp-install-server ...
         (java-mode . lsp-deferred)
         (rust-mode . lsp-deferred) ;; $ rustup component add rust-analyzer
         ;; bun add -g typescript typescript-language-server (installs into ~/.bun/bin, needs to be on PATH)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (svelte-mode . lsp-deferred) ;; bun add -g svelte-language-server
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
   lsp-diagnostics-disabled-modes '(python-mode python-ts-mode) ;; let me do flycheck myself
   lsp-file-watch-threshold 2000
   ;; Keep pylsp focused on IDE features; lint/format belongs to ruff.
   ;; uv pip install -U 'python-lsp-server[all,websockets]' pylsp-rope
   lsp-pylsp-server-command "/data/.envs/emacs-pylsp/bin/pylsp"
   lsp-pylsp-plugins-autopep8-enabled nil
   lsp-pylsp-plugins-flake8-enabled nil
   lsp-pylsp-plugins-isort-enabled nil
   lsp-pylsp-plugins-mccabe-enabled nil
   lsp-pylsp-plugins-pycodestyle-enabled nil
   lsp-pylsp-plugins-pydocstyle-enabled nil
   lsp-pylsp-plugins-pyflakes-enabled nil
   lsp-pylsp-plugins-pylint-enabled nil
   lsp-pylsp-plugins-rope-autoimport-enabled nil
   lsp-pylsp-plugins-rope-completion-enabled nil
   lsp-pylsp-plugins-yapf-enabled nil
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

;; Guard against a race where a debounced pull-diagnostics request fires while
;; buffer-file-name is transiently nil (e.g. auto-revert with preserve-modes),
;; which otherwise crashes with (wrong-type-argument stringp nil) in
;; lsp-diagnostics--update-path.
(advice-add 'lsp-diagnostics--request-pull-diagnostics :around
            (lambda (orig-fn workspace)
              (when (buffer-file-name)
                (funcall orig-fn workspace))))

(use-package lsp-ui :ensure t :commands lsp-ui-mode)
