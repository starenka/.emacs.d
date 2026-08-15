;;; projectile-tests.el --- ERT tests for projectile.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; stub use-package so we don't need the real projectile package installed
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

(provide 'projectile-tests)
;;; projectile-tests.el ends here
