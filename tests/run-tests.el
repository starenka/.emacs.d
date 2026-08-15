;;; run-tests.el --- load every *-tests.el file in this directory -*- lexical-binding: t; -*-

;; async native-comp trampolines spawn a background emacs subprocess via
;; make-process/call-process; if a test mocks either of those (e.g. to
;; assert on a defun's own process-launching calls), a trampoline firing
;; mid-test can collide with the mock and fail with a bogus
;; wrong-type-argument error. Not needed for a batch test run anyway.
(when (boundp 'native-comp-jit-compilation)
  (setq native-comp-jit-compilation nil))
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

(let* ((self (or load-file-name buffer-file-name))
       (here (file-name-directory self)))
  (dolist (file (directory-files here t "-tests\\.el\\'"))
    (unless (file-equal-p file self)
      (load-file file))))

;;; run-tests.el ends here
