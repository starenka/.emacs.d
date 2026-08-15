;;; alzheimer-tests.el --- ERT tests for alzheimer.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(load-file (expand-file-name "../alzheimer.el"
                              (file-name-directory (or load-file-name buffer-file-name))))

;;; alzheimer--get-first-sentence

(ert-deftest alzheimer-test-get-first-sentence-period ()
  (should (equal (alzheimer--get-first-sentence "First sentence. Second sentence.")
                  "First sentence.")))

(ert-deftest alzheimer-test-get-first-sentence-question-mark ()
  (should (equal (alzheimer--get-first-sentence "Really?\nYes.") "Really?")))

(ert-deftest alzheimer-test-get-first-sentence-no-terminator ()
  (should (equal (alzheimer--get-first-sentence "No terminator here\nsecond line")
                  "No terminator here")))

;;; alzheimer--get-binding / alzheimer--get-doc / alzheimer--get-short-doc

(ert-deftest alzheimer-test-get-doc ()
  (should (equal (alzheimer--get-doc "alzheimer-test-fixture-command")
                  "Fixture doc.\nSecond line.")))

(ert-deftest alzheimer-test-get-short-doc ()
  (should (equal (alzheimer--get-short-doc "alzheimer-test-fixture-command")
                  "Fixture doc.")))

(ert-deftest alzheimer-test-get-doc-unbound-symbol ()
  (should (null (alzheimer--get-doc "alzheimer-test-does-not-exist"))))

(ert-deftest alzheimer-test-get-binding-none ()
  ;; fixture command isn't bound to any key in a fresh keymap
  (should (equal (alzheimer--get-binding "alzheimer-test-fixture-command") nil)))

;;; alzheimer--mk-item

(ert-deftest alzheimer-test-mk-item-short ()
  (let ((result (alzheimer--mk-item (list "alzheimer-test-fixture-command" "a comment"))))
    (should (string-match-p "alzheimer-test-fixture-command" result))
    (should (string-match-p "a comment" result))
    (should (string-match-p "n/a" result))))

(ert-deftest alzheimer-test-mk-item-detailed ()
  (let ((result (alzheimer--mk-item (list "alzheimer-test-fixture-command" "") t)))
    (should (string-match-p "Fixture doc\\.\nSecond line\\." result))))

;;; alzheimer--get-pills

(ert-deftest alzheimer-test-get-pills-empty-file-created ()
  (let* ((dir (make-temp-file "alzheimer-test" t))
         (path (expand-file-name "pills" dir)))
    (unwind-protect
        (progn
          (should-not (file-exists-p path))
          (should (equal (alzheimer--get-pills path) nil))
          (should (file-exists-p path)))
      (delete-directory dir t))))

(ert-deftest alzheimer-test-get-pills-parses-sections-and-comments ()
  (let* ((dir (make-temp-file "alzheimer-test" t))
         (path (expand-file-name "pills" dir)))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "; a full-line comment, ignored\n")
            (insert "#Editing\n")
            (insert "alzheimer-test-fixture-command a nice comment\n")
            (insert "\n")
            (insert "#Nav\n")
            (insert "alzheimer-test-fixture-command\n"))
          (let ((pills (alzheimer--get-pills path)))
            (should (equal (mapcar #'car pills) '("Editing" "Nav")))
            (should (equal (car (car (cdr (assoc "Editing" pills))))
                            "alzheimer-test-fixture-command"))))
      (delete-directory dir t))))

(ert-deftest alzheimer-test-get-pills-misc-section-comes-first ()
  ;; regression test: MISC must sort first among sections, not last -
  ;; see .issues/tests-coverage/context.md for the bug this pins down
  (let* ((dir (make-temp-file "alzheimer-test" t))
         (path (expand-file-name "pills" dir)))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "alzheimer-test-fixture-command headerless entry\n")
            (insert "#Editing\n")
            (insert "alzheimer-test-fixture-command\n"))
          (let ((pills (alzheimer--get-pills path)))
            (should (equal (mapcar #'car pills) '("MISC" "Editing")))))
      (delete-directory dir t))))

;;; alzheimer-show / alzheimer--reload / alzheimer--toggle-details

(ert-deftest alzheimer-test-show-no-pills ()
  (let* ((dir (make-temp-file "alzheimer-test" t))
         (alzheimer-pills-file (expand-file-name "pills" dir)))
    (unwind-protect
        (progn
          (with-temp-file alzheimer-pills-file (insert ""))
          (alzheimer-show)
          (should (equal (buffer-name) "*alzheimer*"))
          (should (eq major-mode 'alzheimer-mode))
          (should buffer-read-only)
          (should (string-match-p "No pills yet" (buffer-string))))
      (when (get-buffer "*alzheimer*") (kill-buffer "*alzheimer*"))
      (delete-directory dir t))))

(ert-deftest alzheimer-test-show-with-pills ()
  (let* ((dir (make-temp-file "alzheimer-test" t))
         (alzheimer-pills-file (expand-file-name "pills" dir)))
    (unwind-protect
        (progn
          (with-temp-file alzheimer-pills-file
            (insert "#Editing\n")
            (insert "alzheimer-test-fixture-command a comment\n"))
          (alzheimer-show)
          (should (string-match-p "\\[Editing\\]" (buffer-string)))
          (should (string-match-p "alzheimer-test-fixture-command" (buffer-string))))
      (when (get-buffer "*alzheimer*") (kill-buffer "*alzheimer*"))
      (delete-directory dir t))))

(ert-deftest alzheimer-test-reload-recreates-buffer ()
  (let* ((dir (make-temp-file "alzheimer-test" t))
         (alzheimer-pills-file (expand-file-name "pills" dir)))
    (unwind-protect
        (progn
          (with-temp-file alzheimer-pills-file (insert ""))
          (alzheimer-show)
          (let ((inhibit-read-only t)) (insert "stale content"))
          (alzheimer--reload)
          (should-not (string-match-p "stale content" (buffer-string))))
      (when (get-buffer "*alzheimer*") (kill-buffer "*alzheimer*"))
      (delete-directory dir t))))

(ert-deftest alzheimer-test-toggle-details-flips-variable-and-shows-doc ()
  (let* ((dir (make-temp-file "alzheimer-test" t))
         (alzheimer-pills-file (expand-file-name "pills" dir))
         (alzheimer-mode-detailed nil))
    (unwind-protect
        (progn
          (with-temp-file alzheimer-pills-file
            (insert "alzheimer-test-fixture-command\n"))
          (alzheimer-show)
          (alzheimer--toggle-details)
          (should alzheimer-mode-detailed)
          (should (string-match-p "Fixture doc\\." (buffer-string))))
      (when (get-buffer "*alzheimer*") (kill-buffer "*alzheimer*"))
      (delete-directory dir t))))

;;; alzheimer--edit-pills

(ert-deftest alzheimer-test-edit-pills-opens-pills-file ()
  (let ((opened nil))
    (cl-letf (((symbol-function 'find-file-other-window)
               (lambda (path) (setq opened path)))
              (alzheimer-pills-file "/tmp/some-pills-file"))
      (alzheimer--edit-pills)
      (should (equal opened "/tmp/some-pills-file")))))

;; fixture command used by the tests above
(defun alzheimer-test-fixture-command ()
  "Fixture doc.
Second line."
  (interactive))

(provide 'alzheimer-tests)
;;; alzheimer-tests.el ends here
