;; C-h c <key> to get the name
;; [C-x] r C-h C-h to show all keys starting with [C-x]

(global-unset-key (kbd "C-q"))
(global-unset-key (kbd "s-q"))

(defhydra sta:toggles ()
  "toggles & tools"
  ("i" text-scale-increase "zoom in")
  ("o" text-scale-decrease "zoom out")
  ("r" (text-scale-adjust 0) "zoom out")
  ("a" abbrev-mode "abbrev")
  ("d" toggle-debug-on-error "debug")
  ("f" auto-fill-mode "fill")
  ("t" toggle-truncate-lines "truncate")
  ("w" whitespace-mode "whitespace")
  ("q" nil "cancel"))

(dolist (kv '(
  ("/" . sta:toggles/body)
  ("." . sta:copy-buffer-file-name-as-kill)
  ("a" . sta:ascii-translit-region)
  ("b" . sta:show-http-response-in-browser)
  ("c" . quick-calc)
  ("d" . devdocs-lookup)
  ("f" . sta:find-files-dwim)
  ("g" . sta:google)
  ("k" . ar/quick-kill-process)
  ("l" . sta:region-to-browser)
  ("p" . list-packages)
  ("s" . sta:go-to-scratch-buffer)
  ("t" . sta:snakecase-translit-region)
  ("u" . string-inflection-cycle)
  ("v b" . git-timemachine-blame)
  ("v i" . sta:goto-github-issues)
  ("v l" . magit-log-buffer-file)
  ("v o" . sta:goto-github-org)
  ("v p" . sta:goto-github-prs)
  ("v r" . sta:goto-github-repo)
  ("v s" . magit-status)
  ("v t" . git-timemachine)
  ("w" . whitespace-cleanup)))

  (global-set-key (kbd (format "s-q %s" (car kv))) (cdr kv))
  (global-set-key (kbd (format "C-q %s" (car kv))) (cdr kv)))

(global-set-key (kbd "<f5>") #'deadgrep)
(global-set-key [?\s-s] #'deadgrep)

;; move buffers between windows
(global-set-key (kbd "<M-S-up>")     'buf-move-up)
(global-set-key (kbd "<M-S-down>")   'buf-move-down)
(global-set-key (kbd "<M-S-left>")   'buf-move-left)
(global-set-key (kbd "<M-S-right>")  'buf-move-right)

;; prev/next buffer kb
(global-set-key (kbd "<C-x-left>")    'previous-buffer)
(global-set-key (kbd "<C-x-right>")   'next-buffer)

(global-set-key "\C-x\C-k" 'kill-buffer) ;; always mistype this, when killing buff

(global-set-key (kbd "<s-SPC>") 'just-one-space)

;; move lines
(global-set-key [(meta up)]  'sta:move-line-up)
(global-set-key [(meta down)]  'sta:move-line-down)

;;(global-set-key (kbd "C-i") 'sta:delete-line-no-kill)

;; shell
(global-set-key "\C-x/"  'sta:get-term)

;; split windows (one|one + term)
(global-set-key "\C-xw"  'sta:epic-split)

;; translate things
(global-set-key "\C-xt"  'sta:translate-to-en)

;; copy line
(global-set-key "\C-c\C-k" 'sta:copy-line)

;; kill to line start
(global-set-key (kbd "C-S-k") 'sta:kill-start-of-line)

;; comment-dwim
(global-set-key [?\C-=] 'comment-dwim)

;; show file name
(global-set-key [C-f1] 'sta:copy-buffer-file-name-as-kill)

;;multiple cursors
;;(global-set-key (kbd "C-q m") 'mc/edit-lines)

;; indent shorcuts
(global-set-key (kbd "C->") 'increase-left-margin)
(global-set-key (kbd "C-<") 'decrease-left-margin)

;; boomkarks
;;(define-key global-map [f8] 'bookmark-jump)
(define-key global-map [f6] 'sta:ido-delete-bookmark)
(define-key global-map [f8] 'sta:ido-switch-bookmark)
(define-key global-map [f7] 'sta:projectile-bookmark-set)
(define-key global-map [f9] 'bookmark-bmenu-list)

;; dired/direx
(global-set-key (kbd "C-x d") 'sta:direx-project-or-direx)
(global-set-key (kbd "C-x D") 'dired)
;; (add-hook 'direx-mode-hook
;;          (lambda () (local-set-key [(shift return)] #'direx:expand-item-recursively)))


;; occur
;;(global-set-key (kbd "C-S") 'occur)
(global-set-key (kbd "C-s") 'isearch-forward)

(global-set-key (kbd "C-x y")  'sta:selectrum-yank-pop)

;; edit files as root
;;(global-set-key (kbd "C-x C-r") 'sta:sudo-edit)

;; direx shorts
;;(add-hook 'direx:direx-mode-hook
;;          (lambda ()
;;           (local-set-key (kbd "*" 'sta:direx:create-file))))

(define-key vterm-mode-map [prior] 'scroll-down-command)
(define-key vterm-mode-map [S-prior] 'scroll-down-command)
(define-key vterm-mode-map [next] 'scroll-up-command)
(define-key vterm-mode-map [S-next] 'scroll-up-command)

(define-key company-active-map (kbd "<tab>") 'company-select-next)
(define-key company-active-map (kbd "<S-tab>") 'company-select-previous)

(global-set-key (kbd "M-z") 'zap-to-char)
(global-set-key (kbd "M-a") 'zap-up-to-char)
