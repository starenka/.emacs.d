(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 80
                    :weight 'normal
                    :width 'normal)

(set-face-attribute 'variable-pitch nil
                    :family "Noto Sans"
                    :height 90
                    :weight 'normal
                    :width 'normal)

(copy-face 'default 'fixed-pitch)

;;(use-package solarized-theme :ensure t)
;;(use-package seoul256-theme :ensure t)
;;(use-package nimbus-theme :ensure t)
;;(use-package leuven-theme :ensure t)
;;((use-package nano-theme :ensure t)

(use-package twilight-bright-theme
  :ensure t
  ;;:defer
  :custom-face
  (hl-line ((nil :foreground "black" :background "#e3f4ff")))
  (deadgrep-match-face ((nil :foreground "#cf7900" :background "#fdf9f2")))
  (web-mode-current-column-highlight-face ((nil :background "#e3f4ff")))
  :config
  (load-theme 'twilight-bright t)
  (setq company-quickhelp-color-background "white"))

(use-package monokai-theme
  :ensure t
  :pin melpa
  :defer t
  :config
  (custom-theme-set-faces
   'monokai
   '(mode-line-buffer-id ((nil :foreground "goldenrod1")))
   '(deadgrep-match-face ((nil :foreground "black" :background "goldenrod1")))
   '(selectrum-current-candidate ((nil :foreground "black" :background "goldenrod1"))))
  (load-theme 'monokai t))

