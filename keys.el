;; C-h c <key> to get the name
;; [C-x] r C-h C-h to show all keys starting with [C-x]

(global-unset-key (kbd "C-q"))
(global-unset-key (kbd "s-q"))


(defhydra sta:buffer-menu (:hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(define-key Buffer-menu-mode-map "." 'sta:buffer-menu/body)


(defhydra sta:windows (:hint nil)
  "
^Sizing^                    ^Split management^        ^Navigation^
─────────────────────────────────────────────────────────────────────────
_e_: enlarge horizontaly    _h_: split horizontaly    _j_: ace window
_s_: shrink horiz           _v_: split verticaly      _←_: windmove left
_E_: enlarge vert           _d_: delete window        _↓_: windmove down
_S_: shrink vert            _x_: delete others        _↑_: windmove up
_b_: balance                _u_: undo                 _→_: windmove right
_m_: maximize
_M_: minimize
"
  ("e" enlarge-window-horizontally)
  ("s" shrink-window-horizontally)
  ("E" enlarge-window)
  ("S" shrink-window)
  ("b" balance-windows)
  ("m" maximize-window)
  ("M" minimize-window)
  ("h" split-window-below)
  ("v" split-window-right)
  ("d" delete-window)
  ("x" delete-other-windows)
  ("x" winner-undo)
  ("j" ace-window)
  ("<left>" windmove-left)
  ("<down>" windmove-down)
  ("<up>" windmove-up)
  ("<right>" windmove-right)

  ("c" nil "cancel"))


(defhydra sta:toggles (:hint nil)
  "
^Zoom^             ^Toggles^           ^Misc^
───────────────────────────────────────────────────────────
[_i_] zoom in      [_a_] abbrev        [_d_] debug
[_o_] zoom out     [_f_] fill          [_t_] truncate lines
[_r_] resetzoom    [_w_] whitespace    [_v_] view mode
^ ^                [_r_] read-only     [_h_] cycle themes

"
  ("i" text-scale-increase)
  ("o" text-scale-decrease)
  ("r" sta:reset-zoom)

  ("a" abbrev-mode)
  ("d" toggle-debug-on-error)
  ("f" auto-fill-mode)
  ("t" toggle-truncate-lines)
  ("w" whitespace-mode)
  ("v" view-mode)
  ("r" read-only-mode)
  ("h" sta:cycle-themes)

  ("c" nil "cancel"))

;; C-q ?
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
  ("s" . sta:go-to-fundamental-scratch)
  ("e" . sta:go-to-scratch)
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
(global-set-key (kbd "s-0") 'treemacs-select-window)
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

(global-set-key (kbd "M-a") 'zap-to-char)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-c t") 'sta:cycle-themes)
