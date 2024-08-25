;; at point completion: company vs corfu
;; minibuffer completion: vertico (display), prescient (filter/sort), orderless (matches candidates)
;; marginalia anotates stuff in minibuffer

(use-package corfu-terminal
  :ensure t
  :config
  (unless (display-graphic-p)
  (corfu-terminal-mode +1)))

(use-package corfu
  :ensure t
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  (corfu-popupinfo-mode t)
  :init
  (global-corfu-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  ;:custom
  ; (kind-icon-blend-background t)
  ; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package vertico
  :ensure t
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode))

(use-package vertico-prescient
  :ensure t
  :config
  (setq prescient-filter-method '(literal fuzzy)
        ;; applied in order until one matches
        ;; Value `literal' means the subquery must be a substring of the
        ;; candidate. Supports char folding.

        ;; Value `literal-prefix' means the first subquery must be the
        ;; prefix of the candidate and the remaining subqueries must be
        ;; prefixes of words in the candidate. Supports char folding.

        ;; Value `regexp' means the subquery is interpreted directly as a
        ;; regular expression.

        ;; Value `initialism' means the subquery must match a substring of
        ;; the initials of the candidate.

        ;; Value `fuzzy' means the characters of the subquery must match
        ;; some subset of those of the candidate, in the correct order but
        ;; not necessarily contiguous.

        ;; Value `prefix' means the words (substrings of only word
        ;; characters) match the beginning of words found in the candidate,
        ;; in order, separated by the same non-word characters that separate
        ;; words in the query. This is similar to the completion style
        ;; `partial'.

        ;; Value `anchored' means words are separated by capital letters or
        ;; symbols, with capital letters being the start of a new word. This
        ;; is similar to `prefix', but allows for less typing.

        ;; Value can also be a list of any of the above methods, in which
        ;; case each method will be applied in order until one matches.

        ;; Value can also be a function which returns any of the allowable
        ;; values documented above.
        ;; to make sorting and filtering more intelligent

        prescient-use-char-folding t
        prescient-use-case-folding 'smart
        prescient-sort-full-matches-first t ; Works well with `initialism'.
        prescient-sort-length-enable t)


  (vertico-prescient-mode)
  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1))

;; (use-package vertico-posframe
;;   :ensure t
;;   :custom
;;   (vertico-posframe-parameters
;;    '((left-fringe . 8)
;;      (right-fringe . 8))))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))
