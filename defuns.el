(defun sta:two-col-split ()
  "spawn left window and set its width"
  (interactive)
  (delete-other-windows)
  (split-window-right (floor (* 0.35 (window-width))))
  (other-window 1))

(defun sta:direx-split ()
  (interactive)
  (direx:jump-to-directory)
  (sta:two-col-split))

(defun sta:epic-split ()
  "[x|x + term]"
  (interactive)
  (delete-other-windows)
  (split-window-vertically (floor (* 0.68 (window-height))))
  (split-window-right (floor (* 0.5 (window-width))))
  (other-window 2)
  (sta:get-term)
  (other-window 2))

(defun sta:wterm-split ()
  "[x + term]"
  (interactive)
  (delete-other-windows)
  (split-window-vertically (floor (* 0.75 (window-height))))
  (sta:get-term)
  (other-window 2))

(defun sta:kill-start-of-line ()
  "kill from point to start of line"
  (interactive)
  (kill-line 0))

(defun sta:kill-line-no-kr ()
  "kill line but dont use kill ring"
  (delete-region (point) (line-end-position)))

(defun sta:vivaldi (url)
  (save-window-excursion
    (async-shell-command (format "vivaldi '%s' > /dev/null; echo 'awful.screen.focused().tags[4]:view_only()' | awesome-client" url))))

(defun sta:firefox (url)
  (save-window-excursion
    (async-shell-command (format "firefox '%s' > /dev/null; echo 'awful.screen.focused().tags[3]:view_only()' | awesome-client" url))))

(defun sta:google (string)
  "runs sta:vivaldi w/ googled phrase and switches desktop"
  (interactive "sGoogle for: ")
  (sta:vivaldi (format "https://www.google.com/search?q=%s" string)))

(defun sta:translate (from to string)
  (sta:vivaldi (format "http://translate.google.com/#%s/%s/%s" from to string)))

(defun sta:translate-to-en (string)
  (interactive "scs->en: ")
  (sta:translate "cs" "en" string))

(defun sta:last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'vterm-mode (with-current-buffer (car l) major-mode))
        (car l) (sta:last-term-buffer (cdr l)))))

(defun sta:get-term ()
  "Switch to the term buffer last used, or create a new one if
none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (sta:last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'vterm-mode major-mode))
        (multi-vterm)
      (switch-to-buffer b))))

(defun sta:show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun sta:copy-line (&optional arg)
  "Do a kill-line but copy rather than kill.  This function directly calls
kill-line, so see documentation of kill-line for how to use it including prefix
argument and relevant variables.  This function works by temporarily making the
buffer read-only, so I suggest setting kill-read-only-ok to t."
  (interactive "P")
  (toggle-read-only 1)
  (kill-line arg)
  (toggle-read-only 0))


(defun sta:make-script-executable ()
  "If file starts with a shebang, make `buffer-file-name' executable"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (and (looking-at "^#!")
                 (not (file-executable-p buffer-file-name)))
        (set-file-modes buffer-file-name
                        (logior (file-modes buffer-file-name) #o100))
        (message (concat "Made " buffer-file-name " executable"))))))

(defun sta:run-current-file ()
  "Runs current file"
  (interactive)
  (let* (
         (suffs
          `(
            ("py" . "python")
            ("lua" . "lua")
            ("pl" . "perl")
            ("rb" . "ruby")
            ("js" . "node")
            ("sh" . "bash")
            ("php" . "php")
            )
          )
         (file-name (buffer-file-name))
         (suff (file-name-extension file-name))
         (executable (cdr (assoc suff suffs)))
         (cmd (concat executable " \""   file-name "\""))
         )
    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
        (save-buffer)))

    (if (string-equal suff "el") ; special case for emacs lisp
        (load (file-name-sans-extension file-name))
      (if executable
          (progn
            (message "Running…")
            (shell-command cmd "*sta:run-current-file output*" ))
        (message "No recognized program file suffix for this file.")))))


(defun sta:ido-switch-bookmark ()
  (interactive)
  (bookmark-jump
   (ido-completing-read "Jump to bookmark: " (bookmark-all-names))))


(defun sta:ido-delete-bookmark ()
  (interactive)
  (bookmark-delete
   (ido-completing-read "Delete bookmark: " (bookmark-all-names))))


(defun sta:projectile-bookmark-set (name)
  (interactive (list
                (read-string (format "Bookmark name (%s): " (concat (projectile-project-name) "/"))
                             (concat (projectile-project-name) "/") nil (thing-at-point 'name))))
  (bookmark-set name))


(defun sta:python-insert-header ()
  "insert python file header at the start of buffer"
  (interactive)
  (beginning-of-buffer)
  (insert "# coding=utf-8
"))

(defun sta:python-insert-header-full ()
  "insert python file header at the start of buffer"
  (interactive)
  (beginning-of-buffer)
  (insert "#!/usr/bin/env python
# coding=utf-8

"))

(defun sta:python-insert-gettext ()
  "insert python gettext shortcut"
  (interactive)
  (insert "_(u'')")
  (left-char 2))


(defun sta:python-annotate-junk ()
  (interactive)
  ;;(highlight-lines-matching-regexp "import i?pdb")
  (highlight-lines-matching-regexp "i?pdb.set_trace()")
  ;;(highlight-lines-matching-regexp "^p?print(?")
  (highlight-lines-matching-regexp "assert False"))

(defun sta:nowplaying ()
  (concat "♫ "
          (shell-command-to-string
           "export PYTHONIOENCODING=UTF-8; /home/starenka/bin/sta:nowplaying")))

;; Ctrl-K with no kill
(defun sta:delete-line-no-kill ()
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
 (delete-char 1)
)


;; (defun sta:direx:create-file ()
;;   (interactive)
;;   (let* ((item (direx:item-at-point!))
;;          (file (direx:item-tree item))
;;          (parent-dir
;;           (if (typep file 'direx:directory)
;;               (direx:file-full-name file)
;;             (direx:directory-dirname
;;              (direx:file-full-name file))))
;;          (fname (read-directory-name "Create/visit file: " parent-dir)))
;;     (find-file fname)
;;     (direx:item-refresh-parent item)
;;     (direx:move-to-item-name-part item)))


(defun sta:sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"  ;;su::
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))) ;;su::2


(defun sta:reformat-xml ()
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))


(defun sta:show-http-response-in-browser ()
  (interactive)
  (let ((fname "/tmp/emacs_http_response.html")
        (buffname "*HTTP Response*"))
    (if (not (get-buffer buffname))
        (message (format "No %s buffer found, sorry." buffname))
        (switch-to-buffer buffname)
        (write-file fname)
        (kill-buffer)
      (sta:firefox fname))))


(defun sta:region-to-browser (start end)
  (interactive "r")
  (sta:firefox (buffer-substring start end)))


(defun sta:go-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))


(defun sta:ascii-translit-region (start end)
  "ASCIIfy region"
  (interactive "r")
  (shell-command-on-region start end
                           "iconv -t ASCII//TRANSLIT"
                           (current-buffer) t))


(defun sta:snakecase-translit-region (start end)
  "Makes region snake_case"
  (interactive "r")
  (downcase-region start end)
  (sta:ascii-translit-region start end)
  (subst-char-in-region start end ?\s (string-to-char "_")))


(defun sta:autopyvenv ()
  (pyvenv-activate (format "%s/.env" (projectile-project-root))))


(defun sta:helm-find-files-dwim ()
  "Invoke projectile file search / helm search in buffer dir if available, else /"
  (interactive)
  (if (projectile-project-p) ;; detect if current buffer is in a project
      (projectile-find-file-other-window)
    (helm-find-files-1 (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       "/"
                       ))))

(defun sta:find-files-dwim ()
  "Invoke projectile file search / search in buffer dir if available, else /"
  (interactive)
  (if (projectile-project-p) ;; detect if current buffer is in a project
      (projectile-find-file-dwim-other-window)
    (find-file-other-window (if (buffer-file-name)
                    (file-name-directory (buffer-file-name))
                  "/"
                  ))))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))


(defun diff-last-two-kills ()
  "Write the last two kills to temporary files and diff them."
  (interactive)
  (let ((old "/tmp/old-kill") (new "/tmp/new-kill"))
    (with-temp-file new
      (insert (current-kill 0 t)))
    (with-temp-file old
      (insert (current-kill 1 t)))
    (diff old new "-u" t)))


(defun sta:fpath-to-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (let ((x-select-enable-clipboard t)) (kill-new filename))
      (message filename))))



(cl-defun sta:get-file-python-path (fpath &optional (dominating-file ".flake8"))
  (string-join
   (split-string
    (file-name-sans-extension
     (string-remove-prefix
      (locate-dominating-file fpath dominating-file)
      fpath))
    "/")
   "."))

(defun sta:copy-buffer-file-name-as-kill (choice)
  "Copyies the buffer file stuffs to the kill-ring."
  
  (interactive "cKill (p) python path, (i) python import, (f) file path, (n) file name, (d) directory, (b) buffer name, (m) buffer mmode")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?p)
           (setq new-kill-string  (sta:get-file-python-path name)))
          ((eq choice ?i)
           (setq new-kill-string  (format "from %s import " (sta:get-file-python-path name))))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          ((eq choice ?b)
           (setq new-kill-string (buffer-name)))
          ((eq choice ?m)
           (setq new-kill-string (format "%s" major-mode)))
          (t (message "Quit")))
    (when new-kill-string
      (message "\"%s\" killed" new-kill-string)
      (kill-new new-kill-string))))


(defun zone-choose (pgm)
    "Choose a PGM to run for `zone'."
    (interactive
     (list
      (completing-read
       "Program: "
       (mapcar 'symbol-name zone-programs))))
    (let ((zone-programs (list (intern pgm))))
      (zone)))

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

;; https://emacsredux.com/blog/2013/06/25/boost-performance-by-leveraging-byte-compilation/
(defun er-byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))


(defun sta:activate-venv-in-project-vterm (venv)
  (require 'multi-vterm)
  (switch-to-buffer (multi-vterm-project-get-buffer-name))
  (vterm--goto-line -1)
  (vterm-send-string  (format "workon %s" venv))
  (vterm-send-return)
  (vterm-send-C-l))


(defun sta:spawn-vterm-and-activate-venv-if-py-project ()
  (require 'projectile)
  (require 'multi-vterm)
  (require 'pyvenv)
  (if (and pyvenv-virtual-env-name (locate-dominating-file (projectile-project-root) ".flake8"))
      (progn
        (multi-vterm-project)
        (sta:activate-venv-in-project-vterm pyvenv-virtual-env-name))))

(defun sta:direx-project-or-direx ()
  (interactive)
  (require 'projectile)
  (if (projectile-project-root) (direx-project:jump-to-project-root) (direx:jump-to-directory)))


(defun sta:dash-docs-search-str (string)
     (interactive "sSearch docs: ")
     (dash-docs-search string))


(defun sta:selectrum-yank-pop ()
  (interactive)
  (insert
   (selectrum-read "Search kill ring: "
                   (mapcar 'identity kill-ring))))

(defun sta:deadgrep-file-type ()
  "Prompt the user for a new file type, then restart the search."
  (interactive)
  (let ((new-file-type
         (deadgrep--read-file-type deadgrep--initial-filename)))
    (setq deadgrep--file-type (cons 'type new-file-type)))
  (rename-buffer
   (deadgrep--buffer-name deadgrep--search-term default-directory) t)
  (deadgrep-restart))
