;;(advice-add 'load-theme :before (lambda (&rest _) (sta:disable-themes)))

(use-package monokai-theme :ensure t :pin melpa)
(use-package twilight-bright-theme :ensure t)
(use-package doom-themes :ensure t)
(use-package alect-themes :ensure t)
(use-package hemisu-theme :ensure t)

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
   '(vertico-current ((nil :foreground "black" :background "goldenrod1")))))


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

(setq-default cursor-type '(box . 4))

;; https://pawelbx.github.io/emacs-theme-gallery/
(defvar favourite-themes '(twilight-bright
                           tsdh-light
                           alect-light
                           hemisu-light
                           doom-old-hope
                           doom-lantern
                           monokai))

(defvar sta:last-theme-file (expand-file-name "last-theme" user-emacs-directory))

(defun sta:save-last-theme (theme &rest _)
  (with-temp-file sta:last-theme-file
    (insert (symbol-name theme))))

(advice-add 'load-theme :after #'sta:save-last-theme)

(defun sta:load-default-theme ()
  (let ((theme (or (and (file-exists-p sta:last-theme-file)
                        (intern (string-trim
                                 (with-temp-buffer
                                   (insert-file-contents sta:last-theme-file)
                                   (buffer-string)))))
                   (car favourite-themes))))
    (sta:disable-themes)
    (load-theme theme t)))

(add-hook 'after-init-hook #'sta:load-default-theme)

(defun sta:disable-themes ()
  (mapc 'disable-theme custom-enabled-themes))

(defun sta:reload-theme ()
  (interactive)
  (when-let ((theme (car custom-enabled-themes)))
    (sta:disable-themes)
    (load-theme theme t)
    (force-mode-line-update t)))

(defun sta:cycle-themes ()
  (interactive)
  (let* ((current-theme (car custom-enabled-themes))
         (next-theme (or (cadr (memq current-theme favourite-themes))
                         (car favourite-themes))))
    (sta:disable-themes)
    (load-theme next-theme t)))
