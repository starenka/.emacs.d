;; at point completion: company vs corfu
;; minibuffer completion: vertico (display), prescient (filter/sort), orderless (matches candidates)
;; marginalia anotates stuff in minibuffer

(use-package company
  :ensure t
  :delight
  :config
  (setq
   company-minimum-prefix-length 1
   company-idle-delay 0.0 ;; default is 0.2
   company-selection-wrap-around t)
  (global-company-mode t))

(use-package company-prescient
  :ensure t
  :straight t
  :config
  (company-prescient-mode +1))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode t)
  (setq company-quickhelp-delay 2))


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
