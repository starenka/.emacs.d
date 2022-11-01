(use-package monokai-theme :ensure t)
(use-package seoul256-theme :ensure t)

(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 80
                    :weight 'normal
                    :width 'normal)

(copy-face 'default 'fixed-pitch)

(set-face-attribute 'selectrum-current-candidate nil
                    :foreground "black"
                    :background "goldenrod1")

(set-face-attribute 'deadgrep-match-face nil
                    :foreground "black"
                    :background "goldenrod1")

;;(set-face-attribute 'helm-source-header nil
;;                    :background "#272822"
;;                    :foreground "goldenrod1"
;;                    :underline nil)

;;(set-face-background 'idle-highlight "#3E3D31")
;;(set-face-background 'idle-highlight "goldenrod")
;;(set-face-background 'idle-highlight "#c9acff")



