;;; init-tests.el --- ERT tests for init.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; init.el does real, network-touching bootstrap at load time (el-get,
;; straight, package archives), so it can't be `load-file'd wholesale in
;; a test run - pull just the two forms under test out of the source and
;; eval those instead.
(defun init-tests--load-named-forms (path names)
  "Eval only the top-level forms in PATH that define one of NAMES."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (condition-case nil
        (while t
          (let ((form (read (current-buffer))))
            (when (and (consp form) (memq (cadr form) names))
              (eval form t))))
      (end-of-file nil))))

(init-tests--load-named-forms
 (expand-file-name "../init.el" (file-name-directory (or load-file-name buffer-file-name)))
 '(user-init-dir load-user-file))

;;; load-user-file

(ert-deftest init-test-load-user-file-expands-against-user-init-dir ()
  (let ((loaded nil))
    (cl-letf (((symbol-function 'load-file) (lambda (path) (setq loaded path)))
              (user-init-dir "/tmp/fake-emacs-dir/"))
      (load-user-file "foo.el")
      (should (equal loaded "/tmp/fake-emacs-dir/foo.el")))))

(provide 'init-tests)
;;; init-tests.el ends here
