(use-package lisp-mode
  :commands emacs-lisp-mode
  :config
  (setq
   inferior-lisp-program "/usr/bin/sbcl"
   slime-contribs '(slime-fancy))
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
  (eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode)))

;;(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;;(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;;(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;;(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;;(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;;(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;;(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

