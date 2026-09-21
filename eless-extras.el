;; -*- lexical-binding: t; -*-
;;
;; NOT loaded by init.el / `load-user-file' — `eless' (kitty's
;; `scrollback_pager', see ~/bin/eless) runs Emacs with `-Q', which skips
;; init.el entirely. This file is loaded explicitly via the `EMACS'
;; environment variable indirection set up by ~/bin/eless-with-clipboard
;; (see kitty.conf's `scrollback_pager'), so eless still gets:
;;
;; 1. System-clipboard sync via `xclip', since `-nw' + `-Q' means there is
;;    no X frame and no term.el to enable `xclip-mode' the normal way.
;; 2. Stripped OSC escape sequences (e.g. kitty's OSC 133 shell-integration
;;    prompt marks), which show up as literal garbage text in eless's
;;    Fundamental-mode buffer because eless only strips SGR/color codes.

;; Avoid `package-initialize'/`package-activate-all' here: it scans and
;; activates every installed package (~190 in this config), adding ~250ms+
;; of pure overhead just to reach `xclip'. Instead, find xclip's own elpa
;; directory and add only that to `load-path' directly.
(unless (featurep 'xclip)
  (let* ((xclip-dir
          (car (last (sort (file-expand-wildcards
                             (expand-file-name "elpa/xclip-*[0-9]" user-emacs-directory))
                            #'string<)))))
    (when xclip-dir
      (add-to-list 'load-path xclip-dir)))
  (require 'xclip nil 'noerror))
(when (fboundp 'xclip-mode)
  (xclip-mode 1))

;; User doesn't need syntax highlighting in a pager; skip fontification
;; entirely. Note: for a large source file (e.g. a 20k-line .py file) this
;; alone does NOT fix slow opening — that ~3.6s cost comes from the major
;; mode's own processing (e.g. python-mode's indent-offset guessing), not
;; font-lock. Still worth doing: free win, no downside for a view-only
;; pager, and does help for modes/files where fontification itself is the
;; bottleneck.
(global-font-lock-mode -1)

;; Force every buffer eless opens to stay in `fundamental-mode': skips any
;; expensive major-mode-specific setup entirely (this is what actually
;; fixes the ~3.6s open time for a large .py file — python-mode's own
;; indent-offset guessing, not font-lock, was the cost). Trade-off: eless's
;; own advertised "auto-unarchive tar/zip files" feature (`tar-mode' /
;; `archive-mode', both alist-driven) stops working too. `dired-mode' for
;; directories and eless's own content-sniffing (`diff-mode',
;; `ansi-color') are unaffected since neither goes through these alists.
(setq auto-mode-alist nil
      magic-mode-alist nil
      magic-fallback-mode-alist nil)

(defun sta:eless-strip-osc-sequences ()
  "Strip OSC (`ESC ] ... BEL-or-ST') escape sequences from the buffer.
Kitty's shell-integration prompt marks (OSC 133, etc.) end up as literal
text in eless's scrollback buffer because eless's own
`ansi-color-apply-on-region' call only handles SGR (color) sequences, not
OSC ones."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\e\\][0-9]*;?[^\a\e]*\\(?:\a\\|\e\\\\\\)" nil t)
        (replace-match "")))))
(add-hook 'find-file-hook #'sta:eless-strip-osc-sequences)
