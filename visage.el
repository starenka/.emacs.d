;;(use-package solarized-theme :ensure t)
;;(use-package seoul256-theme :ensure t)
;;(use-package nimbus-theme :ensure t)
;;(use-package leuven-theme :ensure t)
;;((use-package nano-theme :ensure t)

(use-package monokai-theme :ensure t :pin melpa)
(use-package twilight-bright-theme :ensure t)

(with-eval-after-load "twilight-bright-theme"
  (custom-theme-set-faces
   'twilight-bright
   '(hl-line ((nil :foreground "black" :background "#e3f4ff")))
   '(web-mode-current-column-highlight-face ((nil :background "#e3f4ff")))
   '(deadgrep-match-face ((nil :foreground "#cf7900" :background "#fdf9f2")))))

(with-eval-after-load "monokai-theme"
  (custom-theme-set-faces
   'monokai
   ;;'(mode-line ((nil :foreground "black" :background "goldenrod1")))
   ;;'(mode-line-buffer-id ((nil :foreground "gray")))
   '(deadgrep-match-face ((nil :foreground "black" :background "goldenrod1")))
   '(selectrum-current-candidate ((nil :foreground "black" :background "goldenrod1")))))

(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new one."
  (mapc #'disable-theme custom-enabled-themes))

(defun sta:toggle-theme ()
  "Toggle bright and dark themes"
  (interactive)
  (let ((current-theme (car custom-enabled-themes)))
    (cond ((eq current-theme sta:theme-dark) (load-theme sta:theme-light t))
          ((eq current-theme sta:theme-light) (load-theme sta:theme-dark t))
          ((eq current-theme nil) (load-theme sta:theme-default t)))))


(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 80
                    :weight 'normal
                    :width 'normal)

(set-face-attribute 'fixed-pitch nil
                    :family "DejaVu Sans Mono"
                    :height 1.0 ;; relative to default
                    :weight 'normal
                    :width 'normal)

(set-face-attribute 'variable-pitch nil
                    :family "Noto Sans"
                    :height 1.0 ;; relative to default
                    :weight 'normal
                    :width 'normal)


(defvar sta:theme-dark 'monokai)
(defvar sta:theme-light 'twilight-bright)
(defvar sta:theme-default sta:theme-dark)
;;(defvar sta:theme-default sta:theme-light)

(setq-default cursor-type '(box . 4))
(sta:toggle-theme)
