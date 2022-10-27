(defun projectile-after-switch-project-hook ()
  (direx:jump-to-directory)
  (sta:spawn-vterm-and-activate-venv-if-py-project))

(use-package helm-projectile :ensure t)

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system  'helm
        projectile-enable-caching      t
        helm-projectile-fuzzy-match    nil
        projectile-switch-project-action 'projectile-after-switch-project-hook
        projectile-git-submodule-command "" ;; don't recursce on submodules
        projectile-globally-ignored-files
        (append '(".pyc"
                  ".class"
                  "~")
                projectile-globally-ignored-files))
  (projectile-mode)
  (helm-projectile-on)
  (defconst projectile-mode-line-lighter " P"))

(use-package projectile-ripgrep :ensure t)
