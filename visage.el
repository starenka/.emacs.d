(use-package solarized-theme :ensure t)
(use-package seoul256-theme :ensure t)
(use-package nimbus-theme :ensure t)
(use-package leuven-theme :ensure t)

(use-package twilight-bright-theme
  :ensure t
  :config
  (custom-theme-set-faces
   'twilight-bright
   '(deadgrep-match-face ((nil :foreground "black" :background "#e3f4ff")))))

(use-package nano-theme
  :ensure t
  :config
  (custom-theme-set-faces
   'nano-light
   '(mode-line-buffer-id ((nil :foreground "black")))
   '(deadgrep-match-face ((nil :foreground "black" :background "#b4eeb4")))
   '(selectrum-current-candidate ((nil foreground "black" :background "#b4eeb4")))))

(use-package monokai-theme
  :ensure t
  :pin melpa
  :config
  (custom-theme-set-faces
   'monokai
   '(mode-line-buffer-id ((nil :foreground "goldenrod1")))
   '(deadgrep-match-face ((nil :foreground "black" :background "goldenrod1")))
   '(selectrum-current-candidate ((nil :foreground "black" :background "goldenrod1")))))

(load-theme 'twilight-bright t)
;;(load-theme 'monokai t)


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



