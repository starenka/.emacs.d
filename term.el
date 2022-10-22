(use-package vterm
  :ensure t
  :config
  (setq
   vterm-max-scrollback 30000)
)

(use-package multi-vterm :ensure t)

(add-hook 'term-mode-hook (lambda ()
                            (setq term-buffer-maximum-size 50000)
                            (define-key term-raw-map (kbd "C-y") 'term-paste)
                            (yas-minor-mode -1) ;; workaround a bug in yas
                            (abbrev-mode -1)
                            ;;(show-paren-mode -1)
                            (electric-indent-local-mode -1)
                            ))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(dolist (mode '(term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda()
                   (display-line-numbers-mode 0)
                   (nlinum-mode -1)
                   (setq-local global-hl-line-mode nil))))
