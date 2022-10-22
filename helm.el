(use-package helm
  :ensure t
  :straight t
  :config
  (setq
   helm-split-window-inside-p            t ;; open helm buffer inside current window, not occupy whole other window
   helm-move-to-line-cycle-in-source     t ;; move to end or beginning of source when reaching top or bottom of source.
   helm-ff-search-library-in-sexp        t ;; search for library in `require' and `declare-function' sexp.
   helm-scroll-amount                    8 ;; scroll 8 lines other window using M-<next>/M-<prior>
   helm-echo-input-in-header-line        t
   helm-ag-insert-at-point               'symbol ;; word
   helm-autoresize-max-height            0
   helm-autoresize-min-height            25
   x-wait-for-event-timeout              nil  ;; speedup helm spawnage https://github.com/emacs-helm/helm/issues/1976#issuecomment-378724670 https://github.com/emacs-helm/helm/wiki/FAQ#slow-frame-and-window-popup-in-emacs-26 
   )
  (helm-autoresize-mode t)
  (helm-mode 1)
  (add-hook 'helm-minibuffer-set-up-hook 'spacemacs//helm-hide-minibuffer-maybe)
  :bind
  (("M-x" . helm-M-x)
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-f" . helm-find-files)
   ;;("C-q f d" . helm-do-ag)
   ("C-x b" . helm-buffers-list)
   ("C-h a" . helm-apropos)))

(use-package helm-config)

(use-package helm-dash
  :ensure t
  :config
  (setq
   helm-dash-browser-func                'eww  ;; browse-url, eww
   helm-dash-common-docsets              '("Python 3"
                                           "Django"
                                           "Bash"
                                           "Redis"
                                           "ElasticSearch"
                                           "PostgreSQL")
   helm-dash-enable-debugging            nil))


(use-package helm-ag :ensure t)
(use-package helm-rg :ensure t)
