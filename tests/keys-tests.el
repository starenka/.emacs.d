;;; keys-tests.el --- ERT tests for keys.el -*- lexical-binding: t; -*-

(require 'ert)

(defconst keys-tests--keys-file
  (expand-file-name "../keys.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun keys-tests--shortcut-bindings ()
  "Read and return the shortcut alist from keys.el."
  (with-temp-buffer
    (insert-file-contents keys-tests--keys-file)
    (goto-char (point-min))
    (catch 'bindings
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (eq (car-safe form) 'dolist)
                         (eq (car-safe (cadr form)) 'kv))
                (throw 'bindings (cadr (cadr (cadr form)))))))
        (end-of-file (ert-fail "Shortcut binding alist not found"))))))

(ert-deftest keys-test-git-navigation-bindings ()
  (let ((bindings (keys-tests--shortcut-bindings)))
    (dolist (expected '(("v a" . magit-blame-addition)
                        ("v b" . sta:git-history-dwim)
                        ("v d" . magit-diff-buffer-file)
                        ("v h" . magit-log-trace-definition)
                        ("v l" . magit-log-buffer-file)
                        ("v t" . sta:git-browse-revisions)
                        ("v v" . magit-file-dispatch)))
      (should (eq (cdr (assoc (car expected) bindings)) (cdr expected))))
    (should-not (rassq 'git-timemachine bindings))))

(provide 'keys-tests)
;;; keys-tests.el ends here
