(server-start) ;; server mode
(fset 'yes-or-no-p 'y-or-n-p) ;; "y or n" instead of "yes or no"
(setopt use-short-answers t)
(menu-bar-mode -1) ;; no menu bar
(tool-bar-mode -1) ;; no toolbar
(scroll-bar-mode -1) ;; no scrollbar
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; start maximized
(desktop-save-mode 1) ;; save on exit
(global-auto-revert-mode t) ;; sync buffers on disk change
(set-default 'fill-column 90) ;; Line-wrapping
(windmove-default-keybindings) ;;navigate between windows w/ shift+arrows
(pixel-scroll-precision-mode 1) ;; emacs 29
(global-company-mode -1)
;;(initial-buffer-choice "~/TODO")

(make-directory "/tmp/emacs" t)

(setq
  inhibit-startup-message t ;; Don't show the startup screen
  ring-bell-function 'ignore ;; turn off hells & bells
  column-number-mode t ;; Display line and column numbers
  backup-directory-alist '(("." . "~/.emacs.d/.tmp/backups/")) ;; Make sure all backup files only live in one place
  auto-save-file-name-transforms '((".*" "~/.emacs.d/.tmp/auto-saves/" t)) ;; dont litter autosave files
  auto-save-list-file-prefix "~/.emacs.d/.tmp/auto-saves/.auto-save-list-"
  create-lockfiles t
  flycheck-temp-prefix "/tmp/emacs"
  lock-file-name-transforms '(("\\`/.*/\\([^/]+\\)\\'" "/tmp/emacs/\\1" t))
  bookmark-save-flag 1 ;; save bmarks
  load-prefer-newer t ;; always get newest files
  gc-cons-threshold 100000000 ;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
  ;; gc-cons-threshold most-positive-fixnum
  undo-limit 633459
  redisplay-dont-pause t ;; https://www.masteringemacs.org/article/improving-performance-emacs-display-engine
  frame-inhibit-implied-size t ;; Don't resize frame as i use tiling manager (saves startup time)
  read-process-output-max (* 1024 1024) ;; 1mb https://emacs-lsp.github.io/lsp-mode/page/performance/
  auto-window-vscroll nil ;; https://emacs.stackexchange.com/a/28746
  tab-stop-list (number-sequence 4 200 4)
  indent-line-function 'insert-tab
  windmove-wrap-around t
  warning-minimum-level :error ;; don't shout at me if not necessary (esp. native-comp is loud)
  find-file-visit-truename t
  initial-scratch-message
  (concat ";; evaluate & print      C-j\n"
          ";; evaluate defun        C-M-x\n\n"
           ) ;; don't fuck w/ my scratches
)

;; dont autoindent after nl in this modes
(dolist (hook '(markdown-mode-hook org-mode-hook text-mode-hook))
  (add-hook hook (lambda () (electric-indent-local-mode -1))))


(setq-default
 bidi-display-reordering nil  ;; should speedup long lines rendering https://emacs.stackexchange.com/a/603
 abbrev-mode nil
 indent-tabs-mode nil
 tab-width 4
 kill-read-only-ok t ;; sta:copy-line (see defuns)
)

;; file lib
(use-package f :demand)

;; pimp my modeline
(use-package delight
  :ensure t
  :init
  (delight '((abbrev-mode nil "abbrev")
            (eldoc-mode nil "eldoc")
            (dot-mode nil)
            (overwrite-mode " Ov" t))))

;; minor modes as menu
;;(use-package minions
;;    :ensure t
;;    :custom
;;    (minions-mode-line-lighter "…")
;;   (minions-mode-line-delimiters '("" . ""))
;;    :config
;;    (minions-mode +1))

;; show matching parens
(use-package paren-face
  :ensure t
  :config
  (show-paren-mode t)
  (global-paren-face-mode t))

(use-package nlinum
  :ensure t
  :custom
  (global-hl-line-mode nil)
  :config
  (global-nlinum-mode t))

;; hl line on idle
(use-package hl-line+
  :straight '(hl-line+
              :type git
              :host github
              :repo "starenka/hl-line-")
  :config
  (toggle-hl-line-when-idle ))

(use-package tramp
  :custom
  (tramp-default-method "ssh"))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; large file handling
(use-package vlf-setup :ensure vlf :defer)

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

(use-package treemacs
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'treemacs

    (defun my-hidden-stuff (filename absolute-path)
      (or (string-equal filename "__pycache__")
          (string-prefix-p "/HOLD/MY/BEER/" absolute-path)))

    (add-to-list 'treemacs-ignored-file-predicates #'my-hidden-stuff))
  (progn
    (setq treemacs-buffer-name-function            #'treemacs-default-buffer-name
          treemacs-buffer-name-prefix              " *Treemacs-Buffer-"
          treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          nil
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           t
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    nil
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   t
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                t
          treemacs-silent-refresh                  t
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       nil
          treemacs-workspace-switch-cleanup        nil)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode )))


(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; buffer name handling
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-separator "/"))

;; widen currently used window
;; (use-package golden-ratio
;;   :ensure t
;;   :hook (after-init . golden-ratio-mode)
;;   :delight
;;   :custom
;;   (golden-ratio-auto-scale t)
;;   (golden-ratio-exclude-modes '(occur-mode)))

;; yasnippets everywhere
(use-package yasnippet
  :ensure t
  :delight yas-minor-mode
  :bind ("M-s" . yas-expand)
  :custom
  (yas-prompt-functions '(yas/ido-prompt))
  :config
  (yas-global-mode 1))

(use-package vundo
  :ensure t
  :defer t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  :bind (("C-x u"   . vundo)))

(use-package undo-fu-session
  :ensure t
  :config
  (global-undo-fu-session-mode))

;; show help while pressing part of the chord f.e M-q
(use-package which-key
  :ensure t
  :delight
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 10000)
  (which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode))

(use-package hydra :commands defhydra :ensure t)

(use-package winner :ensure t :config (winner-mode t))

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
  ;; :custom
  ;;(magit-diff-auto-show 't) ; dont show diffs on comit and such
  :bind ("C-x c" . magit-status))

;; git blame / history navigation simplified (fucked up rn)
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
  :custom
  (avy-all-windows t))

;; jump troigh windows
(use-package ace-window
  :ensure t
  :bind ("s-j" . ace-select-window)
  :init (setq aw-dispatch-always t))

(use-package dimmer
  :ensure t
  :pin melpa
  :custom
  (dimmer-fraction .25)
  :init
  (dimmer-configure-which-key)
  (dimmer-mode t))

(use-package whitespace-cleanup-mode :ensure t)
(use-package ascii-table :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; lua support
(use-package lua-mode
  :ensure t
  :mode ("\\.lua$" . lua-mode)
  :config
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

;; clojure
(use-package clojure-mode :ensure t :defer)
(use-package cider :ensure t :defer)

;;rust
(use-package rust-mode :ensure t :defer)

;;this is java
(use-package cc-mode
  :config
  (add-hook 'java-mode-hook (lambda ()
              (setq-local indent-tabs-mode t
                          tab-width 2
                          c-basic-offset 2))))
(use-package lsp-java :ensure t :defer)
(use-package gradle-mode :ensure t :defer)

;; markdown support
(use-package markdown-mode
  :ensure t
  :defer
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)))

;; rst support
(use-package rst
  :ensure t
  :defer
  :mode (("\\.txt\\'" . rst-mode)
         ("\\.rst\\'" . rst-mode)
         ("\\.rest\\'" . rst-mode)))

;; adoc support
(use-package adoc-mode
  :ensure t
  :defer
  :mode ("\\.adoc\\'" . rst-mode))

;; elastic support
(use-package es-mode
  :ensure t
  :mode (("\\.elastic$" . es-mode)
         ("\\.es$" . es-mode)))
  ;;:config
  ;;(add-hook 'es-mode-hook (lambda () (company-mode))))

;; postman for big boys
(use-package restclient
  :ensure t
  :straight (:host github :repo "pashky/restclient.el"
                   :commit "b9b373c8fbd5c5f764e34541d1b496e1bb0d7dc1")
  :mode (("\\.rest$" . restclient-mode)
         ("\\.http$" . restclient-mode))
  :config
  (add-hook 'restclient-mode-hook (lambda ()
              (setq-local indent-tabs-mode t
                    tab-width 2))))

;; jq for restclient stuff
(use-package restclient-jq :ensure t :defer)

;; systemd units support
(use-package systemd
  :ensure t
  :defer
  :mode ("\\.service$" . systemd-mode))

;; ;; ansible
(use-package poly-ansible
  :ensure t
  :defer
  :config
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

(use-package yaml-pro
  :ensure t
  :after yaml-mode
  :hook (yaml-mode . yaml-pro-mode))

(use-package ansible
  :ensure t
  :defer
  :config
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))


;; dockerfile
(use-package dockerfile-mode :ensure t :defer)

;; direnv
(use-package direnv
  :ensure t
  :config
  (direnv-mode))


(use-package reformatter
  :hook
  (python-mode . autopep8-format-on-save-mode)
  (python-ts-mode . autopep8-format-on-save-mode)
  (yaml-mode . prettier-yaml-format-on-save-mode)
  :config
  (reformatter-define autopep8-format
    :program "autopep8"
    :args `("-"))

  (reformatter-define prettier-yaml-format
    :program "prettier"
    :args '("--parser" "yaml"))
  )

(use-package csv-mode :ensure t)

;; epub
(use-package nov
  :ensure t
  :defer
  :mode ("\\.epub\\'" . nov-mode))

;; html/js/css/django...
(use-package web-mode ;; https://web-mode.org
  :ensure t
  :defer
  :mode ("\\.html?\\'" . web-mode)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-current-column-highlight t)
  (web-mode-engines-alist '(("django" . "\\.html\\'"))))

(use-package js-auto-format-mode
  :ensure t
  :defer
  :config
  (add-hook 'js-mode-hook #'js-auto-format-mode))

(use-package sqlite-mode-extras
  :bind (:map
         sqlite-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("b" . sqlite-mode-extras-backtab-dwim)
         ("f" . sqlite-mode-extras-tab-dwim)
         ("+" . sqlite-mode-extras-add-row)
         ("D" . sqlite-mode-extras-delete-row-dwim)
         ("C" . sqlite-mode-extras-compose-and-execute)
         ("E" . sqlite-mode-extras-execute)
         ("S" . sqlite-mode-extras-execute-and-display-select-query)
         ("DEL" . sqlite-mode-extras-delete-row-dwim)
         ("g" . sqlite-mode-extras-refresh)
         ("<backtab>" . sqlite-mode-extras-backtab-dwim)
         ("<tab>" . sqlite-mode-extras-tab-dwim)
         ("RET" . sqlite-mode-extras-ret-dwim)))

;; packer
(use-package hcl-mode :ensure t :defer)

;; browser
(use-package w3m
  :ensure t
  :defer
  :custom
  (browse-url-browser-function 'w3m-browse-url)
  :config
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t))

;; nicer pdf rendering
(use-package pdf-tools
  :ensure t
  :pin melpa
  :defer
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
(use-package xkcd :ensure t :defer)
(use-package snow :ensure t :defer)
(use-package fireplace :ensure t :defer)

;; screensaver
(use-package zone
  :ensure t
  :custom
  (zone-programs [zone-pgm-putz-with-case
                  zone-pgm-drip-fretfully
                  zone-pgm-random-life
                  zone-pgm-drip
  ;;                      zone-pgm-jitter
  ;;                      zone-pgm-whack-chars
  ;;                      zone-pgm-rotate
  ;;                      zone-pgm-rotate-LR-lockstep
  ;;                      zone-pgm-rotate-RL-lockstep
  ;;                      zone-pgm-rotate-LR-variable
  ;;                      zone-pgm-rotate-RL-variable
  ;;                      zone-pgm-five-oclock-swan-dive
  ;;                      zone-pgm-rat-race
  ;;                      zone-pgm-paragraph-spaz
                       ])
  :config
  (zone-when-idle 300))

;; startup time profiler
(use-package esup
  :ensure :pin melpa
  :custom
  (esup-depth 0)) ;; https://github.com/jschaf/esup/issues/85#issuecomment-1130110196


(use-package gptel
  :straight t
  :custom
  (gptel-model "gpt-4.1-mini")
  (gptel-default-model "gpt-4.1-mini"))

(use-package gptel-quick
  :straight (Gptel-quick :type git :host github :repo "karthink/gptel-quick"))

;; (package-vc-install '(ultra-scroll :vc-backend Git :url  "https://github.com/jdtsmith/ultra-scroll"))
(use-package ultra-scroll
  :ensure t
  ;:load-path "~/code/emacs/ultra-scroll" ; if you git clone'd instead of package-vc-install
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; show cursor on win change
(use-package beacon
  :ensure t
  :delight
  :config
  (beacon-mode 1))

(toggle-frame-maximized)
