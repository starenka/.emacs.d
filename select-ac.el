(use-package company
  :ensure t
  :config
  (setq
   company-minimum-prefix-length 1
   company-idle-delay 0.0 ;; default is 0.2
   company-selection-wrap-around t)
  (global-company-mode t))

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

;;(use-package orderless
;;  :ensure t
;;  :custom
;;  (completion-styles '(orderless basic))
;;  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package company-prescient
  :ensure t
  :config
  (company-prescient-mode +1))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode t)
  (setq company-quickhelp-delay 2))
