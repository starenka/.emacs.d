;; window pimp funcs

(defun sta:two-col-split ()
  "spawn left window and set its width"
  (interactive)
  (delete-other-windows)
  (split-window-right (floor (* 0.35 (window-width))))
  (other-window 1))

(defun sta:direx-split ()
  "[x|direx]"
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

;; browser funcs

(defun sta:vivaldi (url)
  "opens url in vivaldi and switches to coresponding awesome tag"
  (save-window-excursion
    (async-shell-command (format "vivaldi '%s' > /dev/null; echo 'awful.screen.focused().tags[4]:view_only()' | awesome-client" url))))

(defun sta:firefox (url)
  "opens url in firefox and switches to coresponding awesome tag"
  (save-window-excursion
    (async-shell-command (format "firefox '%s' > /dev/null; echo 'awful.screen.focused().tags[3]:view_only()' | awesome-client" url))))

(defun sta:google (string)
  "googles phrase and switches to coresponding awesome tag"
  (interactive "sGoogle for: ")
  (sta:vivaldi (format "https://www.google.com/search?q=%s" string)))

(defun sta:translate (from to string)
  "sends str to google translate and switches to coresponding awesome tag"
  (sta:vivaldi (format "http://translate.google.com/#%s/%s/%s" from to string)))

(defun sta:translate-to-en (string)
   "translates to english via GT and switches to coresponding awesome tag"
  (interactive "scs->en: ")
  (sta:translate "cs" "en" string))

(defun sta:show-http-response-in-browser ()
  "Sends RESTclient response in browser"
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
  "Sends region to browser"
  (interactive "r")
  (sta:firefox (buffer-substring start end)))

;; unsorted stuff

(defun sta:kill-start-of-line ()
  "kill from point to start of line"
  (interactive)
  (kill-line 0))

(defun sta:kill-line-no-kr ()
  "kill line but dont use kill ring"
  (interactive)
  (delete-region (point) (line-end-position)))

(defun sta:delete-line-no-kill ()
  "Delete line w/out kill"
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
 (delete-char 1))

(defun sta:last-term-buffer (l)
  "Return most recently used term buffer"
  (when l
    (if (eq 'vterm-mode (with-current-buffer (car l) major-mode))
        (car l) (sta:last-term-buffer (cdr l)))))


(defun sta:get-term ()
  "Switch to the term buffer last used, or create a new one if
none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (sta:last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'vterm-mode  major-mode))
        (multi-vterm)
      (switch-to-buffer b))))

(defun sta:copy-line (&optional arg)
  "Do a kill-line but copy rather than kill.  This function directly calls
kill-line, so see documentation of kill-line for how to use it including prefix
argument and relevant variables.  This function works by temporarily making the
buffer read-only, so I suggest setting kill-read-only-ok to t."
  (interactive "P")
  (toggle-read-only 1)
  (kill-line arg)
  (toggle-read-only 0))

(defun sta:ido-switch-bookmark ()
  "Jumps to bookmark"
  (interactive)
  (bookmark-jump
   (ido-completing-read "Jump to bookmark: " (bookmark-all-names))))

(defun sta:ido-delete-bookmark ()
  "Deletes boomkmark"
  (interactive)
  (bookmark-delete
   (ido-completing-read "Delete bookmark: " (bookmark-all-names))))

(defun sta:projectile-bookmark-set (name)
  "Sets bookmark"
  (interactive (list
                (read-string (format "Bookmark name (%s): " (concat (projectile-project-name) "/"))
                             (concat (projectile-project-name) "/") nil (thing-at-point 'name))))
  (bookmark-set name))

;; (defun sta:direx:create-file ()
;;   "creates file in direx at point"
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
  "Reformats xml, good for oneliners"
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))

(defun sta:go-to-scratch-buffer ()
  "Switches to scratch"
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun sta:ascii-translit-region (start end)
  "ASCIIfies region"
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

(defun sta:find-files-dwim ()
  "Invoke projectile file search / search in buffer dir if available, else /"
  (interactive)
  (if (projectile-project-p) ;; detect if current buffer is in a project
      (projectile-find-file)
    (find-file-other-window (if (buffer-file-name)
                    (file-name-directory (buffer-file-name))
                  "/"
                  ))))

(defun sta:move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun sta:move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun sta:diff-last-two-kills ()
  "Write the last two kills to temporary files and diff 'em."
  (interactive)
  (let ((old "/tmp/old-kill") (new "/tmp/new-kill"))
    (with-temp-file new
      (insert (current-kill 0 t)))
    (with-temp-file old
      (insert (current-kill 1 t)))
    (diff old new "-u" t)))

(cl-defun sta:get-file-python-path (fpath &optional (dominating-file ".flake8"))
  "Construts file python import path based on .flake8 file position"
  (string-join
   (split-string
    (file-name-sans-extension
     (string-remove-prefix
      (locate-dominating-file fpath dominating-file)
      fpath))
    "/")
   "."))

(defun sta:copy-buffer-file-name-as-kill (choice)
  "Copyies the buffer file name/path/python path/directory etc to the kill-ring."
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

(defun sta:zone-choose (pgm)
    "Choose zoneprg for `zone'."
    (interactive
     (list
      (completing-read
       "Program: "
       (mapcar 'symbol-name zone-programs))))
    (let ((zone-programs (list (intern pgm))))
      (zone)))

;; https://emacsredux.com/blog/2013/06/25/boost-performance-by-leveraging-byte-compilation/
(defun er-byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defun sta:activate-venv-in-project-vterm (venv)
  "Activates project virtualenv in term"
  (require 'multi-vterm)
  (switch-to-buffer (multi-vterm-project-get-buffer-name))
  (vterm--goto-line -1)
  (vterm-send-string  (format "workon %s" venv))
  (vterm-send-return)
  (vterm-send-C-l))

(defun sta:spawn-vterm-and-activate-venv-if-py-project ()
  "If pyvenv-virtual-env-name is set (.dir-locals f.e.) and it 'is a python project', spawn term with virtualenv"
  (require 'projectile)
  (require 'multi-vterm)
  (require 'pyvenv)

  (if (and (alist-get 'pyvenv-workon file-local-variables-alist) pyvenv-virtual-env-name)
      (progn
        (multi-vterm-project)
        (sta:activate-venv-in-project-vterm pyvenv-virtual-env-name))))

(defun sta:direx-project-or-direx ()
  "Open direx in project root or file dir"
  (interactive)
  (require 'projectile)
  (if (projectile-project-root) (direx-project:jump-to-project-root) (direx:jump-to-directory)))

(defun sta:yank-pop ()
  "Copies item from killring"
  (interactive)
  (insert
   (completing-read "Search kill ring: "
                   (mapcar 'identity kill-ring))))

(defun sta:deadgrep-file-type ()
  "Prompt the user for a new file type, then restart the search in deadgrep"
  (interactive)
  (let ((new-file-type
         (deadgrep--read-file-type deadgrep--initial-filename)))
    (setq deadgrep--file-type (cons 'type new-file-type)))
  (rename-buffer
   (deadgrep--buffer-name deadgrep--search-term default-directory) t)
  (deadgrep-restart))

(defun sta:magit-get-github-web-repo-url (&optional remote-name)
  "Gets github repo web url"

  (let* ((remote-name (or remote-name
                          (magit-get "branch" "main" "remote")
                          (magit-get "branch" "master" "remote")))
         (remote-url (magit-get "remote" remote-name "url")))
    (when (string-match-p "github\.com" remote-url)
      (format "https://github.com/%s"
              (string-trim-right (nth 1 (split-string remote-url ":")) "\.git")))))

(defun sta:goto-github-repo ()
  "spawn browser with gh repo"
  (interactive)
  (sta:vivaldi (sta:magit-get-github-web-repo-url)))

(defun sta:goto-github-issues ()
  "spawn browser with repo gh issues"
  (interactive)
  (sta:vivaldi (format "%s/issues" (sta:magit-get-github-web-repo-url))))

(defun sta:goto-github-prs ()
  "spawn browser with repo gh prs"
  (interactive)
  (sta:vivaldi (format "%s/pulls" (sta:magit-get-github-web-repo-url))))

(defun sta:goto-github-org ()
  "spawn browser with repo gh org"
  (interactive)
  (sta:vivaldi (string-join (butlast (split-string (sta:magit-get-github-web-repo-url) "/" )) "/")))

(defun sta:goto-github-file (&optional branch)
  "spawn browser with file on gh"
  (interactive)
  (sta:vivaldi
   (format "%s/tree/%s/%s#L%s:L%s"
           (sta:magit-get-github-web-repo-url)
           (or branch "master")
           (replace-regexp-in-string (regexp-quote (file-truename (projectile-project-root))) "" (file-truename buffer-file-name))
           (line-number-at-pos)
           (line-number-at-pos))))

(defun sta:lore-mastering-emacs ()
  "Open the bible"
  (interactive)
  (find-file-other-window "/data/bookz-tutorial/mastering-emacs-v4.epub")
  (text-scale-set 2))

(defun sta:lore-elisp ()
  "Open the bible"
  (interactive)
  (find-file-other-window "/data/bookz-tutorial/An Introduction to Programming in Emacs Li - GNU.pdf")
  (text-scale-set 2))

(defun sta:get-random-line-from-file (path)
  "gets random line from file"
  (with-temp-buffer
    (insert-file-contents path)
    (nth 1 (sta:shuffle-list (split-string (buffer-string) "\n" t)))))

(defun sta:shuffle-list (list)
  "inplace list shuffle"
  (dolist (i (reverse (number-sequence 1 (1- (length list)))))
    (let ((j (random (1+ i)))
	  (tmp (elt list i)))
      (setf (elt list i) (elt list j))
      (setf (elt list j) tmp)))
  list)

(defun sta:reset-zoom ()
  "resets text scale"
  (interactive)
  (text-scale-adjust 0))

(defun sta:prev-window ()
  "jumps to previous window"
  (interactive)
  (other-window -1))

(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                (let ((pkg (cadr (assq name where))))
                  (when pkg
                    (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))
