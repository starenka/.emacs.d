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

(defun alzheimer--format-item (item)
  "Formats item"
  (require 'marginalia)
  (let* ((format-string (format "%%%ds  %%s %%s\n" 10)) 
         (kb (alzheimer--get-binding item))
         (name item)
         (doc (alzheimer--get-short-doc item))
         (faced-key (propertize kb 'face 'alzheimer-kb-face))
         (faced-name (propertize name 'face 'alzheimer-item-face))
         (faced-doc (propertize doc 'face 'alzheimer-doc-face)))
    (format format-string faced-key faced-name faced-doc)))
  
(defun alzheimer-show ()
  "Creates buffer and shows alzheimer"
  (interactive)
  (switch-to-buffer-other-window "*alzheimer*")
  (alzheimer-mode)
  (erase-buffer)
  (insert (propertize "Take your pill" 'face 'alzheimer-heading-face))
  (insert "\n\n")
  (dolist (item (alzheimer--read-file (expand-file-name "alzheimer" user-init-dir)))
    (insert (alzheimer--format-item item)))
  (setq buffer-read-only t))

(define-derived-mode alzheimer-mode fundamental-mode "Alzheimer"
  "Set major mode for viewing alzheimer's notes.")

(define-key alzheimer-mode-map (kbd "q") 'kill-buffer-and-window)

(provide 'alzheimer)
