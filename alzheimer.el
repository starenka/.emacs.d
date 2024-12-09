```emacs-lisp
(defvar alzheimer-mode-detailed nil
  "Toggle detailed documentation display in Alzheimer mode.")

(defcustom alzheimer-pills-file
  (expand-file-name "alzheimer-pills"
                    (or (bound-and-true-p user-emacs-directory)
                        (bound-and-true-p user-init-directory)
                        "~/.emacs.d/"))
  "File to store Alzheimer pills."
  :type 'file
  :group 'alzheimer)

(defface alzheimer-kb-face '((t :inherit font-lock-constant-face)) "Face for keybindings in Alzheimer mode.")
(defface alzheimer-item-face '((t :inherit font-lock-function-name-face)) "Face for items in Alzheimer mode.")
(defface alzheimer-doc-face '((t :inherit font-lock-doc-face)) "Face for documentation in Alzheimer mode.")
(defface alzheimer-heading-face '((t :inherit info-title-2)) "Face for headings in Alzheimer mode.")
(defface alzheimer-help-face '((t :inherit Info-quoted)) "Face for help messages in Alzheimer mode.")

(defun alzheimer--get-pills (&optional path)
  "Read pills from PATH or `alzheimer-pills-file`. Create file if it does not exist."
  (let ((path (or path alzheimer-pills-file)))
    (unless (file-exists-p path)
      (with-temp-buffer (write-file path)))
    (with-temp-buffer
      (insert-file-contents path)
      (split-string (string-trim (buffer-string)) "\n" t))))

(defun alzheimer--edit-pills (&optional path)
  "Open the pills file located at PATH or `alzheimer-pills-file` in another window."
  (interactive)
  (find-file-other-window (or path alzheimer-pills-file)))

(defun alzheimer--get-binding (command)
  "Retrieve the keybinding for COMMAND."
  (when-let* ((sym (intern-soft command))
              (key (and (commandp sym) (where-is-internal sym nil 'first-only))))
    (key-description key)))

(defun alzheimer--get-doc (command)
  "Get the full documentation string for COMMAND."
  (when-let ((sym (intern-soft command)))
    (documentation sym)))

(defun alzheimer--get-short-doc (command)
  "Get the first line of the documentation string for COMMAND."
  (when-let ((doc (alzheimer--get-doc command)))
    (car (split-string doc "\n" t))))

(defun alzheimer--mk-item (item &optional detailed-doc)
  "Format ITEM with optional DETAILED-DOC."
  (let* ((format-string (if detailed-doc
                            " %-10s  %s %s\n\n"
                          " %-10s  %s %s\n"))
         (kb (or (alzheimer--get-binding item) "n/a"))
         (doc (if detailed-doc
                  (alzheimer--get-doc item)
                (alzheimer--get-short-doc item)))
         (faced-key (propertize kb 'face 'alzheimer-kb-face))
         (faced-name (propertize item 'face 'alzheimer-item-face))
         (faced-doc (propertize (or doc "") 'face 'alzheimer-doc-face)))
    (format format-string faced-key faced-name faced-doc)))

(defun alzheimer--reload (&optional detailed-doc)
  "Reload the Alzheimer buffer with optional DETAILED-DOC."
  (interactive)
  (when (get-buffer "*alzheimer*")
    (kill-buffer "*alzheimer*"))
  (alzheimer-show detailed-doc))

(defun alzheimer--toggle-details ()
  "Toggle display of detailed documentation."
  (interactive)
  (setq alzheimer-mode-detailed (not alzheimer-mode-detailed))
  (alzheimer--reload alzheimer-mode-detailed))

(defun alzheimer-show (&optional detailed-doc)
  "Display the Alzheimer buffer with optional DETAILED-DOC."
  (interactive)
  (let ((detailed-doc (or detailed-doc alzheimer-mode-detailed))
        (pills (alzheimer--get-pills)))
    (switch-to-buffer-other-window "*alzheimer*")
    (alzheimer-mode)
    (erase-buffer)
    (if pills
        (progn
          (insert (propertize "Take your pill" 'face 'alzheimer-heading-face))
          (insert "\n\n")
          (dolist (item pills)
            (insert (alzheimer--mk-item item detailed-doc))))
      (progn
        (insert (propertize "No pills yet :/" 'face 'alzheimer-heading-face))
        (insert "\n\n")
        (insert (format "Add your commands (one per line) to '%s'.\nWe will fetch keybindings and documentation for you. Press `g` to reload when ready."
                        alzheimer-pills-file))))
    (setq buffer-read-only t)))

(define-derived-mode alzheimer-mode fundamental-mode "Alzheimer"
  "Major mode for viewing Alzheimer pills."
  (use-local-map alzheimer-mode-map))

(define-key alzheimer-mode-map (kbd "q") #'kill-buffer-and-window)
(define-key alzheimer-mode-map (kbd "g") #'alzheimer--reload)
(define-key alzheimer-mode-map (kbd "d") #'alzheimer--toggle-details)
(define-key alzheimer-mode-map (kbd "e") #'alzheimer--edit-pills)

(provide 'alzheimer)
