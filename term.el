(use-package vterm
  :ensure t
  :config
  (setq
   vterm-max-scrollback 30000)
)

(use-package multi-vterm
  :ensure t
  :config
  (setq multi-vterm-dedicated-window-height-percent 32))

(defun my-term-shell-setup ()
  (when (derived-mode-p 'term-mode)
    (setq term-buffer-maximum-size 50000)
    (define-key term-raw-map (kbd "C-y") 'term-paste)
    (yas-minor-mode -1) ;; workaround a bug in yas
    (abbrev-mode -1)
    (electric-indent-local-mode -1))
  (ansi-color-for-comint-mode-on)
  (display-line-numbers-mode -1)
  (nlinum-mode -1)
  (setq-local global-hl-line-mode nil))

(dolist (hook '(term-mode-hook vterm-mode-hook shell-mode-hook eshell-mode-hook))
  (add-hook hook #'my-term-shell-setup))
