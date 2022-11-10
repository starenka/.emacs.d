(defun projectile-after-switch-project-hook ()
  ;;(delete-other-windows)
  ;;(split-window-vertically (floor (* 0.68 (window-height))))
  ;;(split-window-right (floor (* 0.5 (window-width))))
  (sta:direx-project-or-direx)
  (magit-status)
  (sta:spawn-vterm-and-activate-venv-if-py-project)
  (ace-select-window))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-switch-project-action 'projectile-after-switch-project-hook
        projectile-git-submodule-command "" ;; don't recursce on submodules
        ;;projectile-enable-caching t ;; wont recognize new files sometimes
        projectile-globally-ignored-files
        (append '(".pyc"
                  ".class"
                  "~")
                projectile-globally-ignored-files))
  (defconst projectile-mode-line-lighter " P")
  (define-key projectile-command-map (kbd "s s") #'deadgrep))

(use-package projectile-ripgrep :ensure t)
