(defun projectile-after-switch-project-hook ()
  (magit-status)
  (delete-other-windows)
  ;;(sta:spawn-vterm-and-activate-venv-if-py-project)
  (sta:tree))
  ;;(ace-select-window))

;; Projectile calls this outside redisplay and caches the result. A live
;; (:eval ...) in the lighter ran projectile-project-name during redisplay and
;; segfaulted the daemon when a GUI emacsclient frame was created.
(defun sta:projectile-mode-line ()
  (format " [pr:%s]" (or (projectile-project-name) "-")))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line-function #'sta:projectile-mode-line
        projectile-switch-project-action 'projectile-after-switch-project-hook
        projectile-git-submodule-command "" ;; don't recursce on submodules
        projectile-git-command "git ls-files -zco" ;; workaround for projectile-find-file not showing ignored files
        ;;projectile-enable-caching t ;; wont recognize new files sometimes
        ;; globally-ignored-files matches full names; use -suffixes for extensions
        projectile-globally-ignored-file-suffixes
        (append '(".pyc" ".class" "~")
                projectile-globally-ignored-file-suffixes))
  (define-key projectile-command-map (kbd "s s") #'deadgrep))

(use-package projectile-ripgrep :ensure t)
