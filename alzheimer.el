(setq alzheimer-mode-detailed nil)
(defconst alzheimer-pills-file (expand-file-name "alzheimer-pills"
                                                 (cond ((boundp 'user-emacs-directory)
                                                        user-emacs-directory)
                                                       ((boundp 'user-init-directory)
                                                        user-init-directory)
                                                       (t "~/.emacs.d/"))))

(defface alzheimer-kb-face '((t :inherit 'font-lock-constant-face)) "Item kb face")
(defface alzheimer-item-face '((t :inherit 'font-lock-function-name-face)) "Item commandp face")
(defface alzheimer-doc-face '((t :inherit 'font-lock-doc-face)) "Item doc face")
(defface alzheimer-heading-face '((t :inherit 'info-title-2)) "Mode heading face")
(defface alzheimer-help-face '((t :inherit 'Info-quoted)) "Mode help face")

(defun alzheimer--get-pills (&optional path)
  "Reads file line by line or creates empty one"
  (let ((path (or path alzheimer-pills-file)))
    (when (not (file-exists-p path))
      (with-temp-buffer (write-file path)))
    (with-temp-buffer
      (insert-file-contents path)
      (mapcar 'string-trim (split-string (buffer-string) "\n" t)))))

(defun alzheimer--edit-pills (&optional path)
  "Opens pills file in other window"
  (interactive)
  (let ((path (or path alzheimer-pills-file)))
    (find-file-other-window path)))

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

(defun alzheimer--toggle-details ()
  "toggle docstring details"
  (interactive)
  (setq alzheimer-mode-detailed (not alzheimer-mode-detailed))
  (alzheimer--reload alzheimer-mode-detailed))

(defun alzheimer-show (&optional detailed-doc)
  "Creates buffer and shows alzheimer"
  (interactive)
  (let ((detailed-doc (or detailed-doc nil))
        (pills (alzheimer--get-pills)))

    (switch-to-buffer-other-window "*alzheimer*")
    (alzheimer-mode)
    (erase-buffer)

    (if pills (progn
                (insert (propertize "Take your pill" 'face 'alzheimer-heading-face))
                (insert "\n\n")
                (dolist (item (alzheimer--get-pills alzheimer-pills-file))
                  (insert (alzheimer--mk-item item detailed-doc))))
      (progn (insert (propertize "No pills yet :/" 'face 'alzheimer-heading-face))
             (insert "\n\n")
             (insert (format (concat "Add your precious commandps (one line each) to '%s'."
                                     "\nWe will fetch the keybindings and docstring for you. "
                                     "Just hit `g` to relaod when ready.") alzheimer-pills-file))))
    
    ;; (insert (propertize
    ;;          (format "\n\n\ngenerated: %s"
    ;;                  (format-time-string "%a %b %d %H:%M:%S %Z %Y" (current-time)))
    ;;          'face
    ;;          'alzheimer-help-face))
    (setq buffer-read-only t)))

(define-derived-mode alzheimer-mode fundamental-mode "Alzheimer"
  "Set major mode for viewing alzheimer's notes.")

(define-key alzheimer-mode-map (kbd "q") 'kill-buffer-and-window)
(define-key alzheimer-mode-map (kbd "g") 'alzheimer--reload)
(define-key alzheimer-mode-map (kbd "d") 'alzheimer--toggle-details)
(define-key alzheimer-mode-map (kbd "e") 'alzheimer--edit-pills)

(provide 'alzheimer)
