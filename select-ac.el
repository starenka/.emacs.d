(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;;(corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current t)      ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-echo-documentation t) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode)
  (setq
   corfu-auto-prefix 1
   corfu-auto-delay 0))

(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package selectrum
  :ensure t
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :ensure t
  :config
  ;; applied in order until one matches
  (setq prescient-filter-method '(literal regexp initialism fuzzy)))
  ;; to make sorting and filtering more intelligent
  (selectrum-prescient-mode +1)
  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1)


;; (use-package company
;;   :ensure t
;;   :config
;;   (setq
;;    company-minimum-prefix-length 1
;;    company-idle-delay 0.0 ;; default is 0.2
;;    company-selection-wrap-around t)
;;   (global-company-mode t))

;; (use-package company-prescient
;;   :ensure t
;;   :config
;;   (company-prescient-mode +1))

;; (use-package company-quickhelp
;;   :ensure t
;;   :config
;;   (company-quickhelp-mode t)
;;   (setq company-quickhelp-delay 2))
