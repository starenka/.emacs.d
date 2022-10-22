(global-unset-key (kbd "C-q"))
;; C-h c <key> to get the name
;; [C-x] r C-h C-h to show all keys starting with [C-x]

(global-set-key (kbd "C-q f") 'sta:helm-find-files-dwim)

;; move buffers between windows
(global-set-key (kbd "<M-S-up>")     'buf-move-up)
(global-set-key (kbd "<M-S-down>")   'buf-move-down)
(global-set-key (kbd "<M-S-left>")   'buf-move-left)
(global-set-key (kbd "<M-S-right>")  'buf-move-right)

(global-set-key (kbd "<C-x-B>")  'buffer-menu)

;; prev/next buffer kb
(global-set-key (kbd "<C-x-left>")    'previous-buffer)
(global-set-key (kbd "<C-x-right>")   'next-buffer)

(global-set-key "\C-x\C-k" 'kill-buffer) ;; always mistype this, when killing buff

(global-set-key (kbd "<s-SPC>") 'just-one-space)

;; move lines
(global-set-key [(meta up)]  'move-line-up)
(global-set-key [(meta down)]  'move-line-down)
(global-set-key [(meta super up)]  'move-text-up)
(global-set-key [(meta super down)]  'move-text-down)

;;(global-set-key (kbd "C-i") 'sta:delete-line-no-kill)

(global-unset-key "\M-.")
(global-set-key (kbd "M-.") 'avy-goto-word-1)
(global-unset-key "\M-,")
(global-set-key (kbd "M-,") 'avy-goto-char-2)

;; shell
(global-set-key "\C-x/"  'sta:get-term)

;; split windows (one|one + term)
(global-set-key "\C-xw"  'sta:epic-split)

;; google things
(global-set-key (kbd "C-q g")  'sta:google)

;; translate things
(global-set-key "\C-xt"  'sta:translate-to-en)

;; duplicate line
(global-set-key (kbd "C-S-d") 'duplicate-thing)

;; google things
(global-set-key (kbd "C-q c")  'quick-calc)

;; copy line
(global-set-key "\C-c\C-k" 'sta:copy-line)

;; kill to line start
(global-set-key (kbd "C-S-k") 'sta:kill-start-of-line)

;; go to last change
(global-set-key "\C-\M-r" 'goto-last-change)

(global-set-key "\M-s" 'yas-expand)

;; comment-dwim
(global-set-key [?\C-=] 'comment-dwim)

;;navigate between windows w/ shift+arrows
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; show file name
(global-set-key [C-f1] 'sta:copy-buffer-file-name-as-kill)
(global-set-key (kbd "C-q .") 'sta:copy-buffer-file-name-as-kill)

(global-set-key (kbd "C-q d") 'helm-dash-at-point)

;; package upgrades
;l(global-set-key (kbd "C-q p") 'spu-package-upgrade)
(global-set-key (kbd "C-q p") 'list-packages)
 ;;multiple cursors
(global-set-key (kbd "C-q m") 'mc/edit-lines)

;; indent shorcuts
(global-set-key (kbd "C->") 'increase-left-margin)
(global-set-key (kbd "C-<") 'decrease-left-margin)


;; ace jump
;;(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; run current file
(global-set-key "\C-x." 'sta:run-current-file)

;; boomkarks
;;(define-key global-map [f8] 'bookmark-jump)
(define-key global-map [f6] 'sta:ido-delete-bookmark)
(define-key global-map [f8] 'sta:ido-switch-bookmark)
(define-key global-map [f7] 'sta:projectile-bookmark-set)
(define-key global-map [f9] 'bookmark-bmenu-list)

;; dired/direx
;;(global-set-key (kbd "C-x d") 'direx:jump-to-directory)
(global-set-key (kbd "C-x d") 'direx-project:jump-to-project-root)
(global-set-key (kbd "C-x D") 'dired)
;; (add-hook 'direx-mode-hook
;;          (lambda () (local-set-key [(shift return)] #'direx:expand-item-recursively)))


;; occur
;;(global-set-key (kbd "C-S") 'occur)
(global-set-key (kbd "C-s") 'isearch-forward)

(global-set-key (kbd "C-q u") 'string-inflection-cycle)
(global-set-key (kbd "C-q t") 'sta:snakecase-translit-region)
(global-set-key (kbd "C-q a") 'sta:ascii-translit-region)
(global-set-key (kbd "C-q b") 'sta:show-http-response-in-browser)
(global-set-key (kbd "C-q s") 'sta:go-to-scratch-buffer)
(global-set-key (kbd "C-q l") 'sta:region-to-browser)

(global-set-key (kbd "C-x y")  'helm-show-kill-ring)
(global-set-key (kbd "C-x y")  'helm-show-kill-ring)
(global-set-key "\M-\C-y"      'helm-show-kill-ring)

;; perl shorts
(add-hook 'cperl-mode-hook
  (lambda ()
    (local-set-key "\C-ch" 'perl-insert-header)))

; Outline-minor-mode key map
(define-prefix-command 'cm-map nil "Outline-")
; HIDE
(define-key cm-map "q" 'hide-sublevels)    ; Hide everything but the top-level heaings
(define-key cm-map "t" 'hide-body)         ; Hide everything but headings (all bod lines)
(define-key cm-map "o" 'hide-other)        ; Hide other branches
(define-key cm-map "c" 'hide-entry)        ; Hide this entry's body
(define-key cm-map "l" 'hide-leaves)       ; Hide body lines in this entry and subentries
(define-key cm-map "d" 'hide-subtree)      ; Hide everything in this entry and subentries
; SHOW
(define-key cm-map "a" 'show-all)          ; Show (expand) everything
(define-key cm-map "e" 'show-entry)        ; Show this heading's body
(define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sb-headings
(define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this headng
(define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this headng & below
; MOVE
(define-key cm-map "u" 'outline-up-heading)                ; Up
(define-key cm-map "n" 'outline-next-visible-heading)      ; Next
(define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
(define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
(define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
(global-set-key "\M-o" cm-map)

(global-unset-key "\C-xc") ;; helm is hogging C-x-x
(global-set-key "\C-xc"  'magit-status) ;; magit status

;; edit files as root
;;(global-set-key (kbd "C-x C-r") 'sta:sudo-edit)

;; direx shorts
;;(add-hook 'direx:direx-mode-hook
;;          (lambda ()
;;           (local-set-key (kbd "*" 'sta:direx:create-file))))

;;(global-set-key (kbd "C-c b ,") 'goto-last-change)
;;(global-set-key (kbd "C-c b .") 'goto-last-change-reverse)

(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(define-key vterm-mode-map [prior] 'scroll-down-command)
(define-key vterm-mode-map [S-prior] 'scroll-down-command)
(define-key vterm-mode-map [next] 'scroll-up-command)
(define-key vterm-mode-map [S-next] 'scroll-up-command)

(define-key company-active-map (kbd "<tab>") 'company-select-next)
(define-key company-active-map (kbd "<S-tab>") 'company-select-previous)
