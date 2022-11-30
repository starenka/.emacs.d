(server-start) ;; server mode
(fset 'yes-or-no-p 'y-or-n-p) ;; "y or n" instead of "yes or no"
(menu-bar-mode -1) ;; no menu bar
(tool-bar-mode -1) ;; no toolbar
(scroll-bar-mode -1) ;; no scrollbar
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; start maximized
(desktop-save-mode 1) ;; save on exit
(global-auto-revert-mode t) ;; sync buffers on disk change
(set-default 'fill-column 90) ;; Line-wrapping
(windmove-default-keybindings) ;;navigate between windows w/ shift+arrows

(when (fboundp 'electric-indent-mode) (electric-indent-mode -1)) ;; dont autoindent
;;(initial-buffer-choice "~/TODO")
;;(display-battery-mode t) ;; Show battery stats

(setq
  inhibit-startup-message t ;; Don't show the startup screen
  ring-bell-function 'ignore ;; turn off hells & bells
  column-number-mode t ;; Display line and column numbers
  backup-directory-alist '(("." . "~/.emacs.d/.tmp/backups/")) ;; Make sure all backup files only live in one place
  auto-save-file-name-transforms '((".*" "~/.emacs.d/.tmp/auto-saves/" t)) ;; dont litter autosave files
  auto-save-list-file-prefix "~/.emacs.d/.tmp/.auto-save-list-"
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
  windmove-wrap-around t
  warning-minimum-level :error ;; don't shout at me if not necessary (esp. native-comp is loud)
  initial-scratch-message
  (concat ";; evaluate & print      C-j\n"
          ";; evaluate defun        C-M-x\n\n"
           ) ;; don't fuck w/ my scratches
)

(setq-default
 bidi-display-reordering nil  ;; should speedup long lines rendering https://emacs.stackexchange.com/a/603
 abbrev-mode nil
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

;; pimp my modeline
(use-package delight
  :ensure t
  :init
  (delight '((abbrev-mode nil "abbrev")
            (eldoc-mode nil "eldoc")
            (dot-mode nil)
            (overwrite-mode " Ov" t))))

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

;; @FIXME (use-package dired-details+ :ensure t)

(use-package tramp
  :config
  (setq tramp-default-method "ssh"))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; large file handling
(use-package vlf-setup :ensure vlf)

;; jumping to last modified lines
(use-package goto-chg
  :ensure t
  :bind ("C-M-r" . goto-last-change))

;; moving buffers
(use-package buffer-move :ensure t)

;; easy line/region duplication
(use-package duplicate-thing
  :ensure t
  :bind ("C-S-d" . duplicate-thing))

;; apt install silversearcher-ag
(use-package ag :ensure t)
;; apt install ripgrep
(use-package rg
  :ensure t
  :config
  ;; https://github.com/Wilfred/deadgrep/issues/24#issuecomment-942290197
  (defun sta:deadgrep--include-args (rg-args)
  "Adds flags to rigrep"
  ;;(push "--hidden" rg-args) ;; consider hidden folders/files
  (push "--multiline" rg-args))
  (advice-add 'deadgrep--arguments :filter-return #'sta:deadgrep--include-args))

(use-package deadgrep
  :ensure t
  :pin melpa
  :bind (:map deadgrep-mode-map
              ("t" . sta:deadgrep-file-type)))

;; dir tree
(use-package direx
  :ensure t
  ;;:straight '(direx
  ;;            :type git
  ;;            :host github
  ;;            :repo "starenka/direx")
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
  :delight yas-minor-mode
  :bind ("M-s" . yas-expand)
  :config
  (setq yas-prompt-functions '(yas/ido-prompt))
  (yas-global-mode 1))

;; undo buffer changes
(use-package undo-tree
  :ensure t
  :delight
  :config
  ;; Prevent undo tree files from polluting conf dir
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.tmp/undo"))
        undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

;; show help while pressing part of the chord f.e M-q
(use-package which-key
  :ensure t
  :delight
  :config
  (setq
   which-key-show-early-on-C-h t
   which-key-idle-delay 10000
   which-key-idle-secondary-delay 0.05)
  (which-key-mode))

(use-package hydra :commands defhydra :ensure t)

(winner-mode t)

;; help on stereoids
(use-package helpful
  :ensure t
  :bind (("C-h f"   . helpful-callable)
         ("C-h v"   . helpful-variable)
         ("C-h k"   . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F"   . helpful-function)
         ("C-h C"   . helpful-command)))

;; find symbol at point
(use-package smartscan
  :ensure t
  :config
  (global-smartscan-mode 1))

(use-package dot-mode
  :ensure t
  :init
  (add-hook 'find-file-hook 'dot-mode-on))

;; camel, snake, etc 
(use-package string-inflection :ensure t)

;; lisp AC
(use-package ac-slime :ensure t)

;; info pages more readable
(use-package info-colors
  :ensure t
  :config
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

;; apt install git
(use-package magit
  :ensure t
  :bind ("C-x c" . magit-status))
  ;;:config
  ;;(setq magit-diff-auto-show 't) ; dont show diffs on comit and such

;; git blame / history navigation simplified
(use-package git-timemachine :ensure t)

;; mini frame on otop instead of minibuffer
;;(use-package mini-frame
;;  :ensure t
;;  :config
;;  (mini-frame-mode +1))

;; dash
(use-package devdocs
  :ensure t
  :config
  (add-hook 'devdocs-mode-hook (lambda () (text-scale-set 2))))

;; jump to char/word
(use-package avy
  :ensure t
  :bind ("s-." . avy-goto-char-2)
  :config
  (setq avy-all-windows t))

;; jump troigh windows
(use-package ace-window
  :ensure t
  :bind ("s-j" . ace-select-window)
  :init (setq aw-dispatch-always t))

(use-package dimmer
  :ensure t
  :pin melpa
  :init
  (setq dimmer-fraction .25)
  (dimmer-configure-which-key)
  (dimmer-mode t))

(use-package whitespace-cleanup-mode :ensure t)

;; lua support
(use-package lua-mode
  :ensure t
  :mode ("\\.lua$" . lua-mode)
  :config
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

;; markdown support
(use-package markdown-mode
  :ensure t
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)))

;; rst support
(use-package rst
  :ensure t
  :mode (("\\.txt\\'" . rst-mode)
         ("\\.rst\\'" . rst-mode)
         ("\\.rest\\'" . rst-mode)))

;; adoc support
(use-package adoc-mode
  :ensure t
  :mode ("\\.adoc\\'" . rst-mode))

;; elastic support
(use-package es-mode
  :ensure t
  :mode (("\\.elastic$" . es-mode)
         ("\\.es$" . es-mode))
  :config
  (add-hook 'es-mode-hook (lambda () (company-mode))))

;; postman for big boys
(use-package restclient
  :ensure t
  :mode (("\\.rest$" . restclient-mode)
         ("\\.http$" . restclient-mode))
  :config
  (add-hook 'restclient-mode-hook (lambda ()
              (setq-local indent-tabs-mode t
                    tab-width 2))))

;; systemd units support
(use-package systemd
  :ensure t
  :mode ("\\.service$" . systemd-mode))

;; ansible
(use-package poly-ansible
  :ensure t
  :config
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

;; dockerfile
(use-package dockerfile-mode :ensure t)

;; epub
(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

;; html/js/css/django...
(use-package web-mode ;; https://web-mode.org
  :ensure t
  :mode ("\\.html?\\'" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-current-column-highlight t
        web-mode-engines-alist '(("django" . "\\.html\\'"))))

;; browser
(use-package w3m
  :ensure t
  :config
  (setq browse-url-browser-function 'w3m-browse-url)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t))

;; nicer pdf rendering
(use-package pdf-tools
  :ensure t
  :pin melpa
  :init
  (setq
   pdf-info-epdfinfo-program "/usr/bin/epdfinfo" ;; apt install elpa-pdf-tools-server
   pdf-view-incompatible-modes (nlinum-mode))
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook
  ((pdf-view-mode-hook . pdf-view-fit-height-to-window)
   (pdf-view-mode-hook . pdf-links-minor-mode)
   (pdf-view-mode-hook . pdf-annot-minor-mode)
   (pdf-view-mode-hook . pdf-view-auto-slice-minor-mode)
   (pdf-view-mode-hook . (lambda ()
                           (when (eq
                                  (frame-parameter nil 'background-mode)
                                  'dark)
                             (pdf-view-midnight-minor-mode))))))

;; well...
(use-package xkcd :ensure t)

;; screensaver
(use-package zone
  :ensure t
  :config
  (setq zone-programs [zone-pgm-putz-with-case
                       zone-pgm-drip-fretfully
  ;;                      zone-pgm-jitter
  ;;                      zone-pgm-whack-chars
  ;;                      zone-pgm-rotate
  ;;                      zone-pgm-rotate-LR-lockstep
  ;;                      zone-pgm-rotate-RL-lockstep
  ;;                      zone-pgm-rotate-LR-variable
  ;;                      zone-pgm-rotate-RL-variable
  ;;                      zone-pgm-drip
  ;;                      zone-pgm-five-oclock-swan-dive
  ;;                      zone-pgm-rat-race
  ;;                      zone-pgm-paragraph-spaz
  ;;                      zone-pgm-random-life
                       ])
  (zone-when-idle 300))

;; startup time profiler
(use-package esup
  :ensure :pin melpa
  :config
  (setq esup-depth 0)) ;; https://github.com/jschaf/esup/issues/85#issuecomment-1130110196
