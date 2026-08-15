;;; term-tests.el --- ERT tests for term.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'term)

;; stub use-package (vterm/multi-vterm) so we don't try to install/require
;; real packages during a batch test run
(cl-letf (((symbol-function 'use-package) (cons 'macro (lambda (&rest _) nil))))
  (load-file (expand-file-name "../term.el"
                                (file-name-directory (or load-file-name buffer-file-name)))))

;;; my-term-shell-setup

(ert-deftest term-test-shell-setup-in-term-mode ()
  ;; my-term-shell-setup is wired onto term-mode-hook, so entering
  ;; term-mode exercises it directly - yas-minor-mode isn't available in
  ;; a plain -batch run, so mock it
  (with-temp-buffer
    (let ((yas-called nil))
      (cl-letf (((symbol-function 'yas-minor-mode) (lambda (&rest _) (setq yas-called t))))
        (term-mode))
      (should (= term-buffer-maximum-size 50000))
      (should (eq (lookup-key term-raw-map (kbd "C-y")) 'term-paste))
      (should yas-called))))

(ert-deftest term-test-shell-setup-in-non-term-mode-buffer ()
  ;; the term-mode-only branch must not run outside term-mode
  (with-temp-buffer
    (fundamental-mode)
    (cl-letf (((symbol-function 'yas-minor-mode)
               (lambda (&rest _) (error "yas-minor-mode should not be called"))))
      (my-term-shell-setup))
    (should-not display-line-numbers-mode)
    (should-not hl-line-mode)))

(ert-deftest term-test-shell-setup-registered-on-all-hooks ()
  (dolist (hook '(term-mode-hook vterm-mode-hook shell-mode-hook eshell-mode-hook agent-shell-mode-hook))
    (should (memq #'my-term-shell-setup (symbol-value hook)))))

(provide 'term-tests)
;;; term-tests.el ends here
