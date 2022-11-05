(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 80
                    :weight 'normal
                    :width 'normal)

(copy-face 'default 'fixed-pitch)


(use-package solarized-theme :ensure t)
(use-package seoul256-theme :ensure t)
(use-package nimbus-theme :ensure t)
(use-package leuven-theme :ensure t)

(use-package twilight-bright-theme
  :ensure t
  :config
  (load-theme 'twilight-bright t)
  ;; (set-face-attribute 'deadgrep-match-face nil :foreground "#cf7900" :background "#fdf9f2")
  (set-face-attribute 'hl-line nil :foreground "black" :background "#e3f4ff"))

;; this wont work :(
(custom-theme-set-faces
   'twilight-bright
   '(deadgrep-match-face ((nil :foreground "black" :background "#e3f4ff"))))
;;(set-face-attribute 'deadgrep-match-face nil :foreground "black" :background "#e3f4ff") ;; https://www.reddit.com/r/emacs/comments/4v7tcj/comment/d5wyu1r/?utm_source=reddit&utm_medium=web2x&context=3

(use-package nano-theme
  :ensure t
  :defer
  :config
  (custom-theme-set-faces
   'nano-light
   '(mode-line-buffer-id ((nil :foreground "black")))
   '(deadgrep-match-face ((nil :foreground "black" :background "#b4eeb4")))
   '(selectrum-current-candidate ((nil foreground "black" :background "#b4eeb4"))))
  (load-theme 'nano-light t))

(use-package monokai-theme
  :ensure t
  :pin melpa
  :defer
  :config
  (custom-theme-set-faces
   'monokai
   '(mode-line-buffer-id ((nil :foreground "goldenrod1")))
   '(deadgrep-match-face ((nil :foreground "black" :background "goldenrod1")))
   '(selectrum-current-candidate ((nil :foreground "black" :background "goldenrod1"))))
  (load-theme 'monikai t))

