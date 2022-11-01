(use-package solarized-theme :ensure t)
(use-package seoul256-theme :ensure t)

;: @FIXME custom-face sets faces globally, not for just particular theme :/
(use-package monokai-theme
  :ensure t
  :pin melpa
  :custom-face
  (mode-line-buffer-id ((nil :foreground "goldenrod1")))
  (deadgrep-match-face ((nil :foreground "black" :background "goldenrod1")))
  (selectrum-current-candidate ((nil :foreground "black" :background "goldenrod1"))))

(use-package nano-theme
  :ensure t
  :custom-face
  (mode-line-buffer-id ((nil :foreground "black")))
  (deadgrep-match-face ((nil :foreground "black" :background "#b4eeb4")))
  (selectrum-current-candidate ((nil foreground "black" :background "#b4eeb4"))))


(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 80
                    :weight 'normal
                    :width 'normal)

(copy-face 'default 'fixed-pitch)


;;(set-face-attribute 'helm-source-header nil
;;                    :background "#272822"
;;                    :foreground "goldenrod1"
;;                    :underline nil)

;;(set-face-background 'idle-highlight "#3E3D31")
;;(set-face-background 'idle-highlight "goldenrod")
;;(set-face-background 'idle-highlight "#c9acff")



