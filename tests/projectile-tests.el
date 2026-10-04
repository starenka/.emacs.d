;;; projectile-tests.el --- ERT tests for projectile.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; stub use-package so we don't need the real projectile package installed
(defconst projectile-tests--source
  (expand-file-name "../projectile.el" (file-name-directory (or load-file-name buffer-file-name))))

(cl-letf (((symbol-function 'use-package) (cons 'macro (lambda (&rest _) nil))))
  (load-file (expand-file-name "../projectile.el"
                                (file-name-directory (or load-file-name buffer-file-name)))))

;;; projectile-after-switch-project-hook

(ert-deftest projectile-test-after-switch-project-hook-call-order ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'magit-status) (lambda (&rest _) (push 'magit-status calls)))
              ((symbol-function 'sta:tree) (lambda (&rest _) (push 'sta:tree calls)))
              ((symbol-function 'delete-other-windows) (lambda (&rest _) (push 'delete-other-windows calls))))
      (projectile-after-switch-project-hook)
      (should (equal (reverse calls) '(magit-status delete-other-windows sta:tree))))))

;;; sta:projectile-mode-line

(ert-deftest projectile-test-mode-line-shows-project-name ()
  (cl-letf (((symbol-function 'projectile-project-name) (lambda (&rest _) "foo")))
    (should (equal (sta:projectile-mode-line) " [pr:foo]"))))

(ert-deftest projectile-test-mode-line-without-project ()
  (cl-letf (((symbol-function 'projectile-project-name) (lambda (&rest _) nil)))
    (should (equal (sta:projectile-mode-line) " [pr:-]"))))

;; A live (:eval (projectile-project-name)) in the lighter runs during
;; redisplay and segfaults the daemon when a GUI client frame is created.
(ert-deftest projectile-test-no-eval-in-lighter ()
  (with-temp-buffer
    (insert-file-contents projectile-tests--source)
    (goto-char (point-min))
    (should-not (re-search-forward ":delight[^\n]*:eval" nil t))))

(provide 'projectile-tests)
;;; projectile-tests.el ends here
