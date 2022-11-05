(server-start) ;; server mode
(fset 'yes-or-no-p 'y-or-n-p) ;; "y or n" instead of "yes or no"
(menu-bar-mode -1) ;; no menu bar
(tool-bar-mode -1) ;; no toolbar
(scroll-bar-mode -1) ;; no scrollbar
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; start maximized
;;(desktop-save-mode 1) ;; save on exit
(global-auto-revert-mode t) ;; sync buffers on disk change
(set-default 'fill-column 90) ;; Line-wrapping
(windmove-default-keybindings) ;;navigate between windows w/ shift+arrows

;;(initial-buffer-choice "~/TODO")
;;(display-battery-mode t) ;; Show battery stats

(setq
  inhibit-startup-message t ;; Don't show the startup screen
  ring-bell-function 'ignore ;; turn off hells & bells
  column-number-mode t ;; Display line and column numbers
  backup-directory-alist '(("." . "~/.emacs.d/backups")) ;; Make sure all backup files only live in one place
  bookmark-save-flag 1 ;; save bmarks
  load-prefer-newer t ;; always get newest files
  gc-cons-threshold 100000000 ;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
  ;; gc-cons-threshold most-positive-fixnum
  redisplay-dont-pause t ;; https://www.masteringemacs.org/article/improving-performance-emacs-display-engine
  frame-inhibit-implied-size t ;; Don't resize frame as i use tiling manager (saves startup time)
  read-process-output-max (* 1024 1024) ;; 1mb https://emacs-lsp.github.io/lsp-mode/page/performance/
  auto-window-vscroll nil ;; https://emacs.stackexchange.com/a/28746
  tab-stop-list (number-sequence 4 200 4)
  indent-line-function 'insert-tab
  initial-scratch-message ";; evaluate & print      C-j
;; evaluate defun        C-M-x

" ;; don't fuck w/ my scratches
  windmove-wrap-around t
)

(setq-default
 bidi-display-reordering nil  ;; should speedup long lines rendering https://emacs.stackexchange.com/a/603
 abbrev-mode t
 indent-tabs-mode nil
 tab-width 4
 kill-read-only-ok t ;; sta:copy-line (see defuns)
)

;; Trailing whitespace is unnecessary
;;(defvar whitespace-cleanup-on-save t)
;;(add-hook 'before-save-hook
;;    (lambda ()
;;    (if whitespace-cleanup-on-save (whitespace-cleanup))))

;; line length
;;(setq whitespace-line-column 100)
;;(setq whitespace-style '(face lines-tail))
;;(add-hook 'prog-mode-hook 'whitespace-mode)

;; chmod+x files w/ shebang
;; (add-hook 'after-save-hook 'sta:make-script-executable)

;; show matching parens
(use-package paren-face
  :ensure t
  :config
  (show-paren-mode t)
  (global-paren-face-mode t))

(use-package nlinum
  :ensure t
  :config
  (setq global-hl-line-mode nil)
  (global-nlinum-mode t))

;;(use-package idle-highlight-mode
;;  :ensure t
;;  :config (setq idle-highlight-idle-time .3)
;;  :hook ((prog-mode text-mode) . idle-highlight-mode))

;; hl line on idle
(use-package hl-line+
  :straight '(hl-line+
              :type git
              :host github
              :repo "starenka/hl-line-")
  :config
  (toggle-hl-line-when-idle 1))

(use-package tramp
  :config
  (setq tramp-default-method "ssh"))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


(use-package company
  :ensure t
  :config
  (setq
   company-minimum-prefix-length 1
   company-idle-delay 0.0 ;; default is 0.2
   company-selection-wrap-around t))

(use-package vlf-setup :ensure vlf)
(use-package goto-chg :ensure t)
(use-package buffer-move :ensure t)
(use-package duplicate-thing :ensure t)
(use-package ag :ensure t)
(use-package rg
  :ensure t
  :config
  (advice-add 'deadgrep--arguments :filter-return #'sta:deadgrep--include-args))

(use-package deadgrep
  :ensure t
  :pin melpa
  :bind (:map deadgrep-mode-map
              ("t" . sta:deadgrep-file-type)))


;; @FIXME (use-package dired-details+ :ensure t)
;; @FIXME (use-package hl-line+ :ensure t)

(use-package direx
  :ensure t
  :config
  (add-hook 'direx:direx-mode-hook (lambda ()
                            (nlinum-mode -1)
                            (setq-local global-hl-line-mode nil)
                            )))

;; buffer name handling
(use-package uniquify
  :config
  (setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator "/"))

;; yasnippets everywhere
(use-package yasnippet
  :ensure t
  :config
  (setq yas-prompt-functions '(yas/ido-prompt))
  (yas-global-mode 1))

(use-package undo-tree
  :ensure t
  :config
  ;; Prevent undo tree files from polluting conf dir
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode))

;; show help while pressing part of the chord f.e M-q
(use-package which-key
  :ensure t
  :config
  (setq
   which-key-show-early-on-C-h t
   which-key-idle-delay 10000
   which-key-idle-secondary-delay 0.05)
  (which-key-mode))

(winner-mode t)

(use-package helpful :ensure t)

;; find symbol at point
(use-package smartscan
  :ensure t
  :config
  (global-smartscan-mode 1))

(use-package dot-mode
  :ensure t
  :init
  (add-hook 'find-file-hook 'dot-mode-on))

(use-package direx :ensure t)

;; camel, snake, etc 
(use-package string-inflection :ensure t)

;; lisp AC
(use-package ac-slime :ensure t)

(use-package magit
  :ensure t
  :config
 ;;(setq magit-diff-auto-show 't) ; dont show diffs on comit and such
  )

(use-package git-timemachine :ensure t)

;;(use-package mini-frame
;;  :ensure t
;;  :config
;;  (mini-frame-mode +1))

(use-package lua-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

(use-package markdown-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package es-mode
  :ensure t
  :config
  (add-hook 'es-mode-hook (lambda () (company-mode)))
  (add-to-list 'auto-mode-alist '("\\.elastic$" . es-mode))
  (add-to-list 'auto-mode-alist '("\\.es$" . es-mode)))

(use-package restclient
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode))
  (add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode))
  (add-hook 'restclient-mode-hook (lambda ()
              (setq indent-tabs-mode t
                    tab-width 2))))

(use-package systemd
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.service$" . systemd-mode)))

(use-package poly-ansible
  :ensure t
  :config
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

(use-package dockerfile-mode :ensure t)

(use-package dash-docs
  :ensure t
  :config
  (setq
   dash-docs-enable-debugging            t
   dash-docs-browser-func                'eww  ;; browse-url, eww
   dash-docs-common-docsets              '("Python 3"
                                           "Django"
                                           "Bash"
                                           "Redis"
                                           "ElasticSearch"
                                           "PostgreSQL")))

;; epub
(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package web-mode ;; https://web-mode.org
  :ensure t
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-current-column-highlight t
        web-mode-engines-alist '(("django" . "\\.html\\'"))
        )
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;; (add-hook 'css-mode-hook
;;   (lambda () (rainbow-mode t)))

;; (add-hook 'html-mode-hook
;;   (lambda ()
;;     (set (make-local-variable 'sgml-basic-offset) 2)
;;     (setq indent-tabs-mode t)
;;     ;; load rainbow mode
;;     (rainbow-mode t)))

(use-package w3m
  :ensure t
  :config
  (setq browse-url-browser-function 'w3m-browse-url)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t))

(use-package xkcd :ensure t)

;; screensaver
(use-package zone
  :ensure t
  :config
  (setq zone-programs [zone-pgm-jitter
                       zone-pgm-putz-with-case
                       zone-pgm-whack-chars
                       zone-pgm-rotate
                       zone-pgm-rotate-LR-lockstep
                       zone-pgm-rotate-RL-lockstep
                       zone-pgm-rotate-LR-variable
                       zone-pgm-rotate-RL-variable
                       zone-pgm-drip
                       zone-pgm-drip-fretfully
                       zone-pgm-five-oclock-swan-dive
                       zone-pgm-rat-race
                       zone-pgm-paragraph-spaz
                       zone-pgm-random-life])
  (zone-when-idle 300)
)

;; startup time profiler
(use-package esup
  :ensure :pin melpa
  :config
  (setq esup-depth 0)) ;; https://github.com/jschaf/esup/issues/85#issuecomment-1130110196
