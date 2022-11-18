(setq alzheimer-mode-detailed nil)

(defface alzheimer-kb-face
  '((t :inherit 'font-lock-constant-face))
  "Item kb face")

(defface alzheimer-item-face
  '((t :inherit 'font-lock-function-name-face))
  "Item font face")

(defface alzheimer-doc-face
  '((t :inherit 'font-lock-doc-face))
  "Item doc face")

(defface alzheimer-heading-face
  '((t :inherit 'info-title-1))
  "Mode heading face")

(defun alzheimer--read-file (path)
  "Reads file"
  (with-temp-buffer
    (insert-file-contents path)
    (mapcar 'string-trim (split-string (buffer-string) "\n" t))))

(defun alzheimer--get-binding (callable)
  "Annotate command with keybinding"
  (when-let* ((sym (intern-soft callable))
              (key (and (commandp sym) (where-is-internal sym nil 'first-only))))
    (key-description key)))

(defun alzheimer--get-doc (callable)
  "Gets callable doc"
  (documentation (intern-soft callable) 'variable-documentation))

(defun alzheimer--get-short-doc (callable)
  "Gets callable short doc"
  (nth 0 (split-string (alzheimer--get-doc callable) "\n" t)))

(defun alzheimer--mk-item (item &optional detailed-doc)
  "Formats item"
  (let* ((format-string (if detailed-doc (format "%%%ds  %%s %%s\n\n" 10) (format "%%%ds  %%s %%s\n" 10)))
         (kb (alzheimer--get-binding item))
         (name item)
         (doc (if detailed-doc (alzheimer--get-doc item) (alzheimer--get-short-doc item)))
         (faced-key (propertize kb 'face 'alzheimer-kb-face))
         (faced-name (propertize name 'face 'alzheimer-item-face))
         (faced-doc (propertize doc 'face 'alzheimer-doc-face)))
    (format format-string faced-key faced-name faced-doc)))

(defun alzheimer--reload (&optional detailed-doc)
  "Reloads items"
  (interactive)
  (kill-buffer-and-window)
  (alzheimer-show detailed-doc))

(defun alzheimer-show (&optional detailed-doc)
  "Creates buffer and shows alzheimer"
  (interactive)
  (let ((detailed-doc (or detailed-doc nil))))
  (switch-to-buffer-other-window "*alzheimer*")
  (alzheimer-mode)
  (erase-buffer)
  (insert (propertize "Take your pill" 'face 'alzheimer-heading-face))
  (insert "\n\n")
  (dolist (item (alzheimer--read-file (expand-file-name "alzheimer-pills" user-init-dir)))
    (insert (alzheimer--mk-item item detailed-doc)))
  ;; (insert (format "\n\n%s" (format-time-string "%a %b %d %H:%M:%S %Z %Y" (current-time))))
  (setq buffer-read-only t))

(define-derived-mode alzheimer-mode fundamental-mode "Alzheimer"
  "Set major mode for viewing alzheimer's notes.")

(define-key alzheimer-mode-map (kbd "q") 'kill-buffer-and-window)
(define-key alzheimer-mode-map (kbd "g") 'alzheimer--reload)
(define-key alzheimer-mode-map (kbd "d") (lambda ()
                                           (interactive)
                                           (setq alzheimer-mode-detailed (not alzheimer-mode-detailed))
                                           (alzheimer--reload alzheimer-mode-detailed)))

(provide 'alzheimer)
