;; C-h c <key> to get the name
;; [C-x] r C-h C-h to show all keys starting with [C-x]

(global-unset-key (kbd "C-q"))
(global-unset-key (kbd "s-q"))

(defhydra sta:windows ()
  ("e" enlarge-window-horizontally "enlarge win horizontally" :column "Sizing")
  ("s" shrink-window-horizontally "shrink win horizontally")
  ("E" enlarge-window "enlarge win vertically")
  ("S" shrink-window "shrink win vertically")
  ("b" balance-windows "balance window height")
  ("m" maximize-window "maximize current window")
  ("M" minimize-window "minimize current window")

  ("h" split-window-below "split horizontally" :column "Split management")
  ("v" split-window-right "split vertically")
  ("d" delete-window "delete current window")
  ("x" delete-other-windows "delete-other-windows")

  ("j" ace-window "ace window" :column "Navigation")
  ("<left>" windmove-left "← window")
  ("<down>" windmove-down "↓ window")
  ("<up>" windmove-up "↑ window")
  ("<right>" windmove-right "→ window")

  ("q" nil "quit" :column nil))

(defhydra sta:toggles ()
  ("i" text-scale-increase "zoom in" :column "Zoom")
  ("o" text-scale-decrease "zoom out")
  ("r" sta:reset-zoom "reset zoom")

  ("a" abbrev-mode "abbrev" :column "Toggles")
  ("d" toggle-debug-on-error "debug")
  ("f" auto-fill-mode "fill")
  ("t" toggle-truncate-lines "truncate")
  ("w" whitespace-mode "whitespace")
  ("v" view-mode "toggle view mode")
  ("r" read-only-mode "toggle readonly mode")
  ("h" sta:toggle-theme "toggle b/w theme")

  ("q" nil "cancel" :column nil))

(dolist (kv '(
  ("/" . sta:toggles/body)
  ("\\" . sta:windows/body)
  ("." . sta:copy-buffer-file-name-as-kill)
  ("a" . sta:ascii-translit-region)
  ("b" . sta:show-http-response-in-browser)
  ("c" . quick-calc)
  ("d" . devdocs-lookup)
  ("D" . sta:diff-last-two-kills)
  ("f" . sta:find-files-dwim)
  ("F" . sta:sta:find-file-dired)
  ("g" . sta:google)
  ("i" . string-inflection-cycle)
  ("k" . ar/quick-kill-process)
  ("l c" . gptel)
  ("l C" . gptel-menu) ;; https://github.com/karthink/gptel?tab=readme-ov-file#i-want-the-transient-menu-options-to-be-saved-so-i-only-need-to-set-them-once
  ("l s" . gptel-send)
  ("l l" . gptel-quick)
  ("l r" . gptel-rewrite)
  ("p" . package-upgrade-all)
  ("r" . sta:region-to-browser)
  ("s" . sta:go-to-scratch-buffer)
  ("t" . sta:snakecase-translit-region)
  ("u" . lsp-find-references)
  ("v b" . git-timemachine-blame)
  ("v f" . sta:goto-github-file)
  ("v i" . sta:goto-github-issues)
  ("v l" . magit-log-buffer-file)
  ("v o" . sta:goto-github-org)
  ("v p" . sta:goto-github-prs)
  ("v r" . sta:goto-github-repo)
  ("v s" . magit-status)
  ("v t" . git-timemachine)
  ("w" . whitespace-cleanup)
  ("z" . alzheimer-show)))

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
;;(global-set-key (kbd "C-x d") 'sta:treeview-project-or-treeview)
(global-set-key (kbd "C-x d") 'sta:tree)
(global-set-key (kbd "C-x D") 'dired)
;; (add-hook 'direx-mode-hook
;;          (lambda () (local-set-key [(shift return)] #'direx:expand-item-recursively)))


;; occur
;;(global-set-key (kbd "C-S") 'occur)
(global-set-key (kbd "C-s") 'isearch-forward)

(global-set-key (kbd "C-x y")  'sta:yank-pop)

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

(global-set-key (kbd "M-z") 'zap-to-char)
(global-set-key (kbd "M-a") 'zap-up-to-char)

(global-set-key (kbd "C-c t") 'sta:cycle-themes)
