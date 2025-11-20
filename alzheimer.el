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
(defface alzheimer-comment-face '((t :inherit font-lock-comment-face)) "Face for comments in Alzheimer mode.")
(defvar alzheimer--pills-doc-toggle-state (make-hash-table :test 'equal)
  "Hash table to track toggle state of full documentation display per function.")

(defun alzheimer--get-first-sentence (doc)
  "Return the first sentence from DOC string."
  (if (string-match "\\`\\(.*?[.!?]\\)[ \n]" doc)
      (match-string 1 doc)
    (car (split-string doc "\n"))))

(defun alzheimer--get-pills (&optional path)
  "Read pills from PATH or `alzheimer-pills-file`.
The file can contain section headers defined as lines starting with `#SECTION-NAME`.
Function lines follow, each line with: function-name [comment].
Lines starting with `;` are ignored.
Returns an alist of (SECTION . ITEMS), where ITEMS are lists of (FUNC-NAME DISPLAY-DOC FULL-DOC).

DISPLAY-DOC contains the comment followed by the function's documentation first sentence.
FULL-DOC contains the comment followed by the full function's documentation."
  (let ((path (or path alzheimer-pills-file))
        (result (make-hash-table :test 'equal))
        (sections '())
        (current-section "MISC"))
    (unless (file-exists-p path)
      (with-temp-buffer (write-file path)))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
          (cond
           ;; Skip empty or comment lines
           ((or (string-empty-p line) (string-prefix-p ";" line)) nil)

           ;; Section header line: #SECTION-NAME
           ((string-match-p "^#\\(.+\\)" line)
            (setq current-section (string-trim (substring line 1)))
            (unless (member current-section sections)
              (push current-section sections)))

           ;; Otherwise, a function line with optional comment
           (t (let* ((parts (split-string line nil t))
                     (func (car parts))
                     (comment (mapconcat 'identity (cdr parts) " "))
                     (func-sym (intern func))
                     (doc (when (and (fboundp func-sym) (documentation func-sym))
                            (documentation func-sym)))
                     (first-sentence (if doc (alzheimer--get-first-sentence doc) ""))
                     (display-doc (string-trim (concat comment "  " first-sentence)))
                     (full-doc (string-trim (concat comment "  " (or doc "")))))
                (puthash current-section
                         (append (gethash current-section result) (list (list func display-doc full-doc)))
                         result)))))
          (forward-line 1)))
    ;; Convert hash table to alist, preserving section order
    (let (alist)
      ;; If "MISC" section not present in sections but has items, add it first
      (when (and (not (member "MISC" sections))
                 (gethash "MISC" result))
        (push "MISC" sections))
      (dolist (section (reverse sections))
        (let ((items (gethash section result)))
          (when items
            (push (cons section items) alist))))
      (nreverse alist))))

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
  "Format ITEM with optional DETAILED-DOC.
ITEM is a list (FUNC-NAME COMMENT)."
  (let* ((func (car item))
         (comment (cadr item))
         (format-string (if detailed-doc
                            " %-10s  %s\n  %s\n\n"
                          " %-10s  %s  %s\n"))
         (kb (or (alzheimer--get-binding func) "n/a"))
         (doc (if detailed-doc
                  (alzheimer--get-doc func)
                (alzheimer--get-short-doc func)))
         (faced-key (propertize kb 'face 'alzheimer-kb-face))
         (faced-name (propertize func 'face 'alzheimer-item-face))
         (faced-comment (and (not (string-empty-p comment))
                             (propertize comment 'face 'alzheimer-comment-face)))
         (faced-doc (propertize (or doc "") 'face 'alzheimer-doc-face)))
    (if detailed-doc
        (format " %-10s  %s\n  %s\n\n" faced-key faced-name (concat (if faced-comment (concat faced-comment "\n  ") "") faced-doc))
      (format " %-10s  %s  %s\n" faced-key faced-name (or faced-comment faced-doc "")))))

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
  (let* ((detailed-doc (or detailed-doc alzheimer-mode-detailed))
         (pills (alzheimer--get-pills)))
    (switch-to-buffer-other-window "*alzheimer*")
    (alzheimer-mode)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (if pills
          (progn
            (dolist (section pills)
              (insert (propertize (format "[%s]" (car section)) 'face 'alzheimer-heading-face))
              (insert "\n\n")
              (dolist (item (cdr section))
                (insert (alzheimer--mk-item item detailed-doc))))
            (goto-char (point-min)))
        (progn
          (insert (propertize "No pills yet :/" 'face 'alzheimer-heading-face))
          (insert "\n\n")
          (insert (format "Add your commands (one per line) to '%s'.\nFormat: function-name [section] [comment]. Lines starting with ';' are ignored.\nPress `g` to reload when ready."
                          alzheimer-pills-file)))))
    (setq buffer-read-only t)))

(defvar alzheimer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'kill-buffer-and-window)
    (define-key map (kbd "g") #'alzheimer--reload)
    (define-key map (kbd "d") #'alzheimer--toggle-details)
    (define-key map (kbd "e") #'alzheimer--edit-pills)
    map)
  "Keymap for `alzheimer-mode`.")

(define-derived-mode alzheimer-mode fundamental-mode "Alzheimer"
  "Major mode for viewing Alzheimer pills."
  (use-local-map alzheimer-mode-map))

(provide 'alzheimer)
