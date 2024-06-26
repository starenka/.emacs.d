;; packaging

;; https://jeffkreeftmeijer.com/emacs-native-comp-log/
(setq native-comp-async-report-warnings-errors nil)
(setq straight-repository-branch "develop")

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/"))

      package-archive-priorities
      '(("melpa" . 10)
        ("melpa-stable" . 5)
        ("gnu" . 1)
        ("jcs-elpa" . 0)))
                          
(package-initialize)

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
(el-get 'sync)

;; use-package
(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))

;; straigt.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; https://github.com/syl20bnr/spacemacs/issues/12535
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; https://mail.gnu.org/archive/html/help-gnu-emacs/2024-04/msg00211.html
;; (setq package-check-signature nil)

;; func to load other init files
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;; store customized vars in separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(load-user-file "defuns.el")
(load-user-file "global.el")
(load-user-file "term.el")

(load-user-file "select-ac.el")
(load-user-file "projectile.el")
(load-user-file "flycheck.el")
(load-user-file "proced.el")

(load-user-file "lsp.el")
;;(load-user-file "py-autoruff.el")
(load-user-file "pythons.el")
(load-user-file "lisp.el")

(load-user-file "debug.el")

(load-user-file "visage.el")
(load-user-file "keys.el")

;; my modes
(load-user-file "alzheimer.el")
