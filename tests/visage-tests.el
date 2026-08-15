;;; visage-tests.el --- ERT tests for visage.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; visage.el's `use-package' forms install/require real theme packages,
;; which we don't want to touch from a batch test run - stub the macro out
;; for the duration of the load so only the plain defuns get defined.
(cl-letf (((symbol-function 'use-package) (cons 'macro (lambda (&rest _) nil))))
  (load-file (expand-file-name "../visage.el"
                                (file-name-directory (or load-file-name buffer-file-name)))))

;;; sta:save-last-theme

(ert-deftest visage-test-save-last-theme ()
  (let* ((dir (make-temp-file "visage-test" t))
         (sta:last-theme-file (expand-file-name "last-theme" dir)))
    (unwind-protect
        (progn
          (sta:save-last-theme 'doom-old-hope)
          (should (equal (with-temp-buffer
                            (insert-file-contents sta:last-theme-file)
                            (buffer-string))
                          "doom-old-hope")))
      (delete-directory dir t))))

;;; sta:disable-themes

(ert-deftest visage-test-disable-themes-calls-disable-theme-for-each ()
  (let ((disabled nil))
    (cl-letf (((symbol-function 'disable-theme)
               (lambda (theme) (push theme disabled)))
              (custom-enabled-themes '(foo-theme bar-theme)))
      (sta:disable-themes)
      (should (equal (sort disabled #'string<) '(bar-theme foo-theme))))))

(ert-deftest visage-test-disable-themes-noop-when-none-enabled ()
  (let ((disabled nil))
    (cl-letf (((symbol-function 'disable-theme)
               (lambda (theme) (push theme disabled)))
              (custom-enabled-themes nil))
      (sta:disable-themes)
      (should (null disabled)))))

;;; sta:load-default-theme

(ert-deftest visage-test-load-default-theme-uses-saved-theme ()
  (let* ((dir (make-temp-file "visage-test" t))
         (sta:last-theme-file (expand-file-name "last-theme" dir))
         (loaded nil))
    (unwind-protect
        (progn
          (with-temp-file sta:last-theme-file (insert "monokai\n"))
          (cl-letf (((symbol-function 'load-theme) (lambda (theme &rest _) (setq loaded theme)))
                    ((symbol-function 'disable-theme) (lambda (_) nil))
                    (custom-enabled-themes nil))
            (sta:load-default-theme)
            (should (eq loaded 'monokai))))
      (delete-directory dir t))))

(ert-deftest visage-test-load-default-theme-falls-back-when-no-saved-file ()
  (let* ((dir (make-temp-file "visage-test" t))
         (sta:last-theme-file (expand-file-name "does-not-exist" dir))
         (favourite-themes '(twilight-bright tsdh-light))
         (loaded nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'load-theme) (lambda (theme &rest _) (setq loaded theme)))
                    ((symbol-function 'disable-theme) (lambda (_) nil))
                    (custom-enabled-themes nil))
            (sta:load-default-theme)
            (should (eq loaded 'twilight-bright))))
      (delete-directory dir t))))

;;; sta:reload-theme

(ert-deftest visage-test-reload-theme-reloads-current ()
  (let ((loaded nil))
    (cl-letf (((symbol-function 'load-theme) (lambda (theme &rest _) (setq loaded theme)))
              ((symbol-function 'disable-theme) (lambda (_) nil))
              ((symbol-function 'force-mode-line-update) (lambda (&rest _) nil))
              (custom-enabled-themes '(hemisu-light)))
      (sta:reload-theme)
      (should (eq loaded 'hemisu-light)))))

(ert-deftest visage-test-reload-theme-noop-when-none-enabled ()
  (let ((loaded nil))
    (cl-letf (((symbol-function 'load-theme) (lambda (theme &rest _) (setq loaded theme)))
              ((symbol-function 'disable-theme) (lambda (_) nil))
              ((symbol-function 'force-mode-line-update) (lambda (&rest _) nil))
              (custom-enabled-themes nil))
      (sta:reload-theme)
      (should (null loaded)))))

;;; sta:cycle-themes

(ert-deftest visage-test-cycle-themes-goes-to-next ()
  (let ((loaded nil)
        (favourite-themes '(twilight-bright tsdh-light alect-light)))
    (cl-letf (((symbol-function 'load-theme) (lambda (theme &rest _) (setq loaded theme)))
              ((symbol-function 'disable-theme) (lambda (_) nil))
              (custom-enabled-themes '(twilight-bright)))
      (sta:cycle-themes)
      (should (eq loaded 'tsdh-light)))))

(ert-deftest visage-test-cycle-themes-wraps-around ()
  (let ((loaded nil)
        (favourite-themes '(twilight-bright tsdh-light alect-light)))
    (cl-letf (((symbol-function 'load-theme) (lambda (theme &rest _) (setq loaded theme)))
              ((symbol-function 'disable-theme) (lambda (_) nil))
              (custom-enabled-themes '(alect-light)))
      (sta:cycle-themes)
      (should (eq loaded 'twilight-bright)))))

(ert-deftest visage-test-cycle-themes-none-enabled-starts-at-first ()
  (let ((loaded nil)
        (favourite-themes '(twilight-bright tsdh-light alect-light)))
    (cl-letf (((symbol-function 'load-theme) (lambda (theme &rest _) (setq loaded theme)))
              ((symbol-function 'disable-theme) (lambda (_) nil))
              (custom-enabled-themes nil))
      (sta:cycle-themes)
      (should (eq loaded 'twilight-bright)))))

(provide 'visage-tests)
;;; visage-tests.el ends here
