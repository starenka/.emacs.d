;;; flycheck-tests.el --- ERT tests for flycheck.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; stub use-package and flycheck's own checker-definition macro so we don't
;; need the real flycheck package installed to load this file
(cl-letf (((symbol-function 'use-package) (cons 'macro (lambda (&rest _) nil)))
          ((symbol-function 'flycheck-define-checker) (cons 'macro (lambda (&rest _) nil))))
  (load-file (expand-file-name "../flycheck.el"
                                (file-name-directory (or load-file-name buffer-file-name)))))

;;; vitek/python-flycheck-ruff

(ert-deftest flycheck-test-python-flycheck-ruff-sets-buffer-local-checker ()
  (with-temp-buffer
    (vitek/python-flycheck-ruff)
    (should (eq (symbol-value 'flycheck-checker) 'python-ruff))
    (should (local-variable-p 'flycheck-checker))))

(ert-deftest flycheck-test-python-flycheck-ruff-registered-on-hooks ()
  (dolist (hook '(python-mode-hook python-ts-mode-hook))
    (should (memq #'vitek/python-flycheck-ruff (symbol-value hook)))))

(provide 'flycheck-tests)
;;; flycheck-tests.el ends here
