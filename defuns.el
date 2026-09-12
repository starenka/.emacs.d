;; window pimp funcs

(defun sta:two-col-split ()
  "spawn left window and set its width"
  (interactive)
  (delete-other-windows)
  (split-window-right (floor (* 0.35 (window-width))))
  (other-window 1))


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

(defun sta:awesome-run (cmd tag)
  "run CMD and switch to awesome TAG index"
  ;; make-process with :buffer nil/:noquery t fires the browser with no display
  ;; side effects — async-shell-command calls display-buffer which interferes
  ;; when invoked from within other commands (e.g. markdown-preview).
  ;; call-process runs awesome-client synchronously and immediately, decoupled
  ;; from the browser process so it works regardless of whether the browser
  ;; exits quickly (http://) or stays open (file://).
  (make-process
    :name "sta:awesome-tag-switch"
    :command (list shell-file-name shell-command-switch cmd)
    :buffer nil
    :noquery t)
  (call-process shell-file-name nil nil nil
    shell-command-switch
    (format "awesome-client 'awful.screen.focused().tags[%d]:view_only()'" tag)))

;; browser funcs

(defun sta:vivaldi (url &optional _new-window)
  "opens url in vivaldi and switches to coresponding awesome tag"
  ;; &optional _new-window satisfies the browse-url-browser-function signature
  (interactive (list (read-string "URL: " "https://")))
  (sta:awesome-run (format "vivaldi %s > /dev/null" (shell-quote-argument url)) 4))

(defun sta:firefox (url &optional _new-window)
  "opens url in firefox and switches to coresponding awesome tag"
  ;; &optional _new-window satisfies the browse-url-browser-function signature
  ;; --new-tab ensures firefox reuses the existing window (works for both http:// and file://)
  (interactive (list (read-string "URL: " "https://")))
  (sta:awesome-run (format "firefox --new-tab %s > /dev/null" (shell-quote-argument url)) 3))

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
        (message "No %s buffer found, sorry." buffname)
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

(defun sta:copy-line ()
  "Copy the current line to the kill ring."
  (interactive)
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position 2)))

(defun sta:ido-switch-bookmark ()
  "Jumps to bookmark"
  (interactive)
  (bookmark-jump
   (completing-read "Jump to bookmark: " (bookmark-all-names))))

(defun sta:ido-delete-bookmark ()
  "Deletes boomkmark"
  (interactive)
  (bookmark-delete
   (completing-read "Delete bookmark: " (bookmark-all-names))))

(defun sta:projectile-bookmark-set (name)
  "Sets bookmark"
  (interactive (list
                (read-string (format "Bookmark name (%s): " (concat (projectile-project-name) "/"))
                             (concat (projectile-project-name) "/") nil (thing-at-point 'name))))
  (bookmark-set name))

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

(defun sta:go-to-scratch ()
  "Switches to scratch buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun sta:go-to-text-scratch ()
  "Open a *scratch* buffer in text mode."
  (interactive)
  (let ((buf (get-buffer-create "*itchy-scratch*")))
    (with-current-buffer buf
      (text-mode))
    (switch-to-buffer buf)))


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

(defun sta:find-files-dwim (prefill)
  "Complete from open buffers and files in one minibuffer."
  (interactive "P")
  (let* ((initial (cond
                   ((use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end)))
                   (prefill
                    (or (thing-at-point 'filename t)
                        (thing-at-point 'symbol t)))))
         (project-root (and (projectile-project-p)
                            (projectile-project-root)))
         (file-root (or project-root
                        (and (buffer-file-name)
                             (file-name-directory (buffer-file-name)))
                        default-directory))
         (buffer-candidates
          (delq nil
                (mapcar (lambda (buffer)
                          (let ((name (buffer-name buffer)))
                            (unless (string-prefix-p " " name)
                              (cons (format "%s [b]" name) buffer))))
                        (buffer-list))))
         (file-candidates
          (mapcar (lambda (file)
                    (cons (format "%s" file)
                          (expand-file-name file file-root)))
                  (if project-root
                      (projectile-current-project-files)
                    (delq nil
                          (mapcar (lambda (file)
                                    (let ((path (expand-file-name file file-root)))
                                      (when (file-regular-p path)
                                        file)))
                                  (directory-files file-root nil directory-files-no-dot-files-regexp))))))
         (choice (completing-read
                  "Buffer or file: "
                  (append (mapcar #'car buffer-candidates)
                          (mapcar #'car file-candidates))
                  nil t initial))
         (buffer (cdr (assoc choice buffer-candidates)))
         (file (cdr (assoc choice file-candidates))))
    (cond
     (buffer
      (switch-to-buffer buffer))
     (file
      (if project-root
          (find-file file)
        (find-file-other-window file))))))

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


(cl-defun sta:get-file-python-path (fpath &optional (dominating-file "pyproject.toml"))
  "Python import path of FPATH, relative to the dir holding DOMINATING-FILE.
Returns nil when FPATH lives outside such a project.  A leading `src'
component is dropped: in a src-layout project the package root sits under
src/, which is never part of the import path."
  (when-let* ((root (locate-dominating-file fpath dominating-file))
              (relative (file-name-sans-extension (string-remove-prefix root fpath)))
              (parts (split-string relative "/" t)))
    (string-join (if (and (cdr parts) (equal (car parts) "src")) (cdr parts) parts) ".")))

(defun sta:file-line-reference (file)
  "Return FILE:LINE for FILE, or FILE:FIRST-LAST when a region is active.
FILE is made relative to the Projectile root when there is one.  A region
ending at the beginning of a line stops at the previous line, so marking
whole lines reports the lines actually covered."
  (let* ((root (and (projectile-project-p) (projectile-project-root)))
         (path (if root (file-relative-name file root) file))
         (first (line-number-at-pos (if (use-region-p) (region-beginning) (point))))
         (last (if (use-region-p)
                   (save-excursion
                     (goto-char (region-end))
                     (if (and (bolp) (> (line-number-at-pos) first))
                         (1- (line-number-at-pos))
                       (line-number-at-pos)))
                 first)))
    (if (> last first)
        (format "%s:%d-%d" path first last)
      (format "%s:%d" path first))))

(defun sta:copy-as-kill (what)
  "Copy WHAT flavour of the current buffer's identity to the kill ring.
WHAT is one of `file-path', `file-name', `directory', `line-reference',
`python-path', `python-import', `buffer-name' or `major-mode'."
  (let* ((file (if (derived-mode-p 'dired-mode)
                   (dired-get-filename nil t)
                 (buffer-file-name)))
         (text (pcase what
                 ('file-path file)
                 ('file-name (and file (file-name-nondirectory file)))
                 ('directory (and file (file-name-directory file)))
                 ('line-reference (and file (sta:file-line-reference file)))
                 ('python-path (and file (sta:get-file-python-path file)))
                 ('python-import (when-let ((path (and file (sta:get-file-python-path file))))
                                   (format "from %s import " path)))
                 ('buffer-name (buffer-name))
                 ('major-mode (symbol-name major-mode)))))
    (if text
        (progn
          (kill-new text)
          (message "\"%s\" killed" text))
      (message "Nothing to copy"))))

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

(defun sta:llm-offtopic-agent-shell (dir)
  "Open an agent-shell in DIR, a scratch run directory under /tmp/fap."
  (require 'agent-shell)
  (let ((default-directory (file-name-as-directory dir)))
    (agent-shell '(4))))

(defun llm-offtopic ()
  "Open a fresh emacsclient frame and start an agent-shell in a fresh /tmp/fap run directory."
  (interactive)
  (call-process "mkdir" nil nil nil "-p" "/tmp/fap")
  (let ((dir (make-temp-file "/tmp/fap/run-" t)))
    (start-process "llm-offtopic" nil "emacsclient" "-c" "-e"
                   (format "%S" `(sta:llm-offtopic-agent-shell ,dir)))))

(defun sta:tree ()
  "Ads and opens current project/dir in treemacs"
  (interactive)
  (require 'treemacs)
  (treemacs-add-and-display-current-project-exclusively))


(defun sta:yank-pop ()
  "Copies item from killring"
  (interactive)
  (insert
   (completing-read "Search kill ring: " kill-ring)))

(defun sta:deadgrep-file-type ()
  "Prompt the user for a new file type, then restart the search in deadgrep"
  (interactive)
  (let ((new-file-type
         (deadgrep--read-file-type deadgrep--initial-filename)))
    (setq deadgrep--file-type (cons 'type new-file-type)))
  (rename-buffer
   (deadgrep--buffer-name deadgrep--search-term default-directory) t)
  (deadgrep-restart))

;; Git history / forge helpers

(defun sta:git-history-dwim ()
  "Show Git history for the active region or the current line.
This uses Magit's line log, so each matching commit can be opened to
inspect its diff."
  (interactive)
  ;; defuns.el loads before global.el installs/configures Magit.
  (require 'magit-log)
  (let ((lines (and (use-region-p)
                    (magit-file-region-line-numbers))))
    (unless lines
      (let ((line (line-number-at-pos nil t)))
        (setq lines (list line line))))
    (magit-log-buffer-file nil (car lines) (cadr lines))))

(defun sta:git-browse-revisions ()
  "Visit the previous revision of the current file using Magit.
In the resulting historical-file buffer, use `p' and `n' to move
backward and forward through revisions, and `q' to quit."
  (interactive)
  (require 'magit-files)
  (magit-blob-previous))

;; forge-agnostic (github/gitlab/gitea/forgejo/bitbucket/sourcehut/...) repo
;; helpers, built on top of git-link (global.el) rather than hand-rolling
;; per-forge URL formats. git-link only covers file/commit/homepage links
;; itself; issues/PRs/org have no upstream equivalent, so those are built
;; here from the same remote-parsing + `git-link-remote-alist' host
;; detection git-link uses, to stay in sync with whatever hosts are
;; registered there (see .dir-locals.el note in global.el for self-hosted
;; instances). git-link is `require'd lazily inside the functions below,
;; not at top level - defuns.el loads before global.el (which use-package
;; :ensure's git-link) in init.el, so a top-level require here would fail
;; on every startup.

;; git-link has no `git-link-homepage-gitea' - Codeberg (a public Forgejo
;; instance) is its reference implementation for the gitea/forgejo URL
;; scheme, so the homepage handler is only named `-codeberg'. Alias it so
;; self-hosted Gitea/Forgejo registration never has to mention Codeberg,
;; matching git-link's own `git-link-gitea' alias for the file-link handler.
(with-eval-after-load 'git-link
  (defalias 'git-link-homepage-gitea 'git-link-homepage-codeberg))

(defconst sta:vc-forge-alist
  '((github    :remote-handler git-link-github    :homepage-handler git-link-homepage-github
               :issues-path "issues"   :prs-path "pulls")
    (gitlab    :remote-handler git-link-gitlab    :homepage-handler git-link-homepage-github
               :issues-path "-/issues" :prs-path "-/merge_requests")
    (gitea     :remote-handler git-link-gitea     :homepage-handler git-link-homepage-gitea
               :issues-path "issues"   :prs-path "pulls")
    (bitbucket :remote-handler git-link-bitbucket :homepage-handler git-link-homepage-github
               :issues-path "issues"   :prs-path "pull-requests")
    (sourcehut :remote-handler git-link-sourcehut :homepage-handler git-link-homepage-github
               :issues-path nil        :prs-path nil))
  "Single source of truth for everything forge-specific: which git-link
handlers build repo/file links for a forge, and the URL path suffixes for
its issues/PR pages (nil = not reachable from the repo URL - e.g. sourcehut
issues live on a separate todo.sr.ht domain and it has no PR concept at
all). Add a new forge here and every sta:goto-forge-* command picks it up;
nothing else needs touching.")

(defconst sta:vc-forge-aliases '((forgejo . gitea))
  "Forge-type aliases resolved before consulting `sta:vc-forge-alist' -
Forgejo and Gitea share the same URL scheme, so `forgejo' is just a more
recognizable name for the same table entry.")

(defvar-local sta:forge-type nil
  "Forge software for the current project's git remote - one of the keys
in `sta:vc-forge-alist' (`github', `gitlab', `gitea', `bitbucket',
`sourcehut'), or the `forgejo' alias for `gitea'.

Leave nil to rely on git-link's own hostname-based auto-detection, which
already covers github.com/gitlab.com and any self-hosted instance whose
hostname contains \"github\"/\"gitlab\" (e.g. GitHub Enterprise). Self-hosted
Gitea/Forgejo/Bitbucket/sourcehut instances have no such recognizable
hostname pattern, so set this explicitly via `.dir-locals.el' - the actual
remote hostname is read from `.git/config' automatically, you only need to
say what software it runs:

  ((nil . ((sta:forge-type . forgejo))))

Since dir-locals apply to every file under the directory they live in, one
`.dir-locals.el' at the root of a directory that holds many repos (e.g.
~/work/) covers all of them at once, regardless of each repo's individual
hostname - it's only ever used to look up which git-link handlers +
issues/PRs paths to use, per `sta:vc-forge-alist'.")

;; any symbol is safe here - just a lookup key, never eval'd - so
;; .dir-locals.el settings don't need a per-project confirmation prompt
(put 'sta:forge-type 'safe-local-variable #'symbolp)

(defun sta:vc-normalize-forge-type (type)
  "Resolve forge-type aliases (e.g. `forgejo' -> `gitea') to their
canonical `sta:vc-forge-alist' key."
  (or (cdr (assq type sta:vc-forge-aliases)) type))

(defun sta:vc--ensure-forge-registered ()
  "If `sta:forge-type' is set for this buffer, make sure git-link knows
how to build links for the current repo's remote host. The hostname comes
from the actual git remote (already in `.git/config', no need to repeat
it) - only the forge software itself needs to be told."
  (when sta:forge-type
    (require 'git-link)
    (let* ((type (sta:vc-normalize-forge-type sta:forge-type))
           (spec (cdr (assq type sta:vc-forge-alist))))
      (unless spec
        (user-error "Unknown sta:forge-type `%s' - must be one of: %s"
                    sta:forge-type (mapcar #'car sta:vc-forge-alist)))
      (let* ((remote-url (git-link--remote-url (git-link--remote)))
             (host (car (git-link--parse-remote remote-url)))
             (host-regexp (regexp-quote host)))
        (add-to-list 'git-link-remote-alist
                      (list host-regexp (plist-get spec :remote-handler)))
        (add-to-list 'git-link-homepage-remote-alist
                      (list host-regexp (plist-get spec :homepage-handler)))))))

(defun sta:vc-forge-type ()
  "Return the current repo's forge-type symbol (a key in
`sta:vc-forge-alist'), or nil if it can't be determined. Prefers the
explicit `sta:forge-type' dir-local; falls back to whatever git-link's own
`git-link-remote-alist' already recognizes by hostname."
  (sta:vc--ensure-forge-registered)
  (require 'git-link)
  (if sta:forge-type
      (sta:vc-normalize-forge-type sta:forge-type)
    (let* ((remote-url (git-link--remote-url (git-link--remote)))
           (host (car (git-link--parse-remote remote-url)))
           (handler (git-link--handler git-link-remote-alist host)))
      ;; compare resolved functions, not symbols: git-link's own alist may
      ;; recognize a host via a differently-named alias of the same handler
      ;; (e.g. codeberg.org resolves via `git-link-codeberg', while our
      ;; table's canonical gitea entry says `git-link-gitea' - both are the
      ;; same function, `git-link-gitea' is just a defalias)
      (car (cl-find handler sta:vc-forge-alist
                     :key (lambda (entry) (plist-get (cdr entry) :remote-handler))
                     :test (lambda (a b) (eq (indirect-function a) (indirect-function b))))))))

(defun sta:vc-web-repo-url ()
  "Return the web homepage URL for the current repo, forge-agnostically.
Unlike `git-link-homepage' this has no kill-ring/browser side effects -
it's meant for building further URLs (issues, PRs, ...) on top of."
  (sta:vc--ensure-forge-registered)
  (require 'git-link)
  (let* ((remote-url (git-link--remote-url (git-link--remote)))
         (parsed (git-link--parse-remote remote-url))
         (host (car parsed))
         (handler (git-link--handler git-link-homepage-remote-alist host)))
    (unless handler
      (user-error "Forge for host `%s' not recognized - set `sta:forge-type' via .dir-locals.el (see global.el)" host))
    (funcall handler (git-link--web-host host) (cadr parsed))))

(defun sta:goto-forge-repo ()
  "spawn browser with the current repo's homepage"
  (interactive)
  (sta:vivaldi (sta:vc-web-repo-url)))

(defun sta:goto-forge--action (path-prop unsupported-noun)
  "Open BASE + the PATH-PROP path (`:issues-path' or `:prs-path') for the
current repo's forge, per `sta:vc-forge-alist'."
  (let* ((type (or (sta:vc-forge-type) (user-error "Unrecognized forge for this repo")))
         (spec (cdr (assq type sta:vc-forge-alist)))
         (path (plist-get spec path-prop)))
    (unless path
      (user-error "%s doesn't expose %s under the repo URL" type unsupported-noun))
    (sta:vivaldi (concat (sta:vc-web-repo-url) "/" path))))

(defun sta:goto-forge-issues ()
  "spawn browser with the current repo's issue tracker"
  (interactive)
  (sta:goto-forge--action :issues-path "issues"))

(defun sta:goto-forge-prs ()
  "spawn browser with the current repo's pull/merge requests"
  (interactive)
  (sta:goto-forge--action :prs-path "pull/merge requests"))

(defun sta:goto-forge-org ()
  "spawn browser with the current repo's org/group/workspace/user page"
  (interactive)
  (sta:vivaldi (string-join (butlast (split-string (sta:vc-web-repo-url) "/")) "/")))

(defun sta:goto-forge-file ()
  "spawn browser with the current file at the current line/region on its forge.
Thin wrapper over `git-link' (which already builds forge-correct
file+line URLs and supports region selection, prefix-arg remote
selection, etc.) - kept as a stable, sta:-namespaced entry point. Honors
`sta:forge-type' the same way the other sta:goto-forge-* commands do."
  (interactive)
  (sta:vc--ensure-forge-registered)
  (call-interactively #'git-link))

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
    (nth 0 (sta:shuffle-list (split-string (buffer-string) "\n" t)))))

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

(defun sta:nuke-stale-elc ()
  "Delete all .elc files from package dirs to clear stale bytecode."
  (interactive)
  (dolist (dir (list (expand-file-name "elpa" user-emacs-directory)
                     (expand-file-name "el-get" user-emacs-directory)
                     (expand-file-name "straight/build" user-emacs-directory)
                     (expand-file-name "straight/repos" user-emacs-directory)
                     (expand-file-name "elpaca/builds" user-emacs-directory)))
    (when (file-directory-p dir)
      (dolist (elc (directory-files-recursively dir "\\.elc$"))
        (delete-file elc))))
  (message "Stale .elc files nuked. Restart Emacs."))

(with-eval-after-load 'straight
  (advice-add 'straight-pull-all :after #'sta:nuke-stale-elc)
  (advice-add 'straight-rebuild-all :after #'sta:nuke-stale-elc))

(with-eval-after-load 'elpaca
  (advice-add 'elpaca-upgrade-all :after #'sta:nuke-stale-elc))

(defun sta:restart-emacs-daemon ()
  "Restart the current Emacs daemon."
  (interactive)
  (unless (daemonp)
    (user-error "Current Emacs is not running as a daemon"))
  (save-some-buffers t)
  (let ((systemctl (executable-find "systemctl"))
        (emacs-bin (expand-file-name invocation-name invocation-directory)))
    (cond
     ((and systemctl
           (zerop (call-process systemctl nil nil nil
                                "--user" "--quiet" "is-active" "emacs.service")))
      ;; Let systemd recycle the daemon when it owns the process.
      (let ((status (call-process systemctl nil nil nil
                                  "--user" "--no-block"
                                  "restart" "emacs.service")))
        (unless (eq status 0)
          (user-error "Could not request an Emacs daemon restart (status %s)"
                      status))
        (message "Emacs daemon restart requested")))
     ((file-executable-p emacs-bin)
      ;; Delay startup so the current daemon can release its socket cleanly.
      (call-process-shell-command
       (format "nohup %s --daemon >/dev/null 2>&1 &"
               (shell-quote-argument emacs-bin)))
      (kill-emacs))
     (t
      (user-error "Could not determine how to restart this Emacs daemon")))))

(defun sta:package-upgrade-all ()
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
                (package-delete  old-package))))
          (sta:nuke-stale-elc)
          (if (daemonp)
              (when (yes-or-no-p "Packages upgraded. Restart the Emacs daemon now? ")
                (sta:restart-emacs-daemon))
            (message "Packages upgraded. Restart Emacs to load the new code.")))
      (message "All packages are up to date"))))


(defun sta:buffer-path ()
  "Return the directory of the current buffer if in a Projectile project,
   otherwise return the directory of the current buffer's file if visiting a file.
  "

   (if (projectile-project-p)
      (projectile-project-root)
    (when (buffer-file-name)
      (file-name-directory (buffer-file-name)))))


(defun sta:find-file-dired (filename)
  " runs (find-dired) with path taken from buffer location and limiting the find to just file name pattern "

  (interactive "sEnter filename pattern: ")
  (let ((dir (sta:buffer-path)))
    (if dir
        (find-dired dir (concat "-name " (shell-quote-argument filename)))
      (message "No valid directory found."))))


;; https://github.com/blahgeek/emacs-lsp-booster
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(defun sta:rae-wotd ()
  "Call the RAE WOTD script and open the resulting HTML file in EWW."
  (interactive)
  (let ((script-path (expand-file-name "~/.config/awesome/opt/rae_wotd"))
        (output "")
        (return-code 0)
        (buffer-name "*RAE-WOTD*"))
    ;; Close existing RAE-WOTD eww buffer if exists
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    ;; the eww/rename/switch/scale calls below must run *after*
    ;; with-temp-buffer exits, not inside it - with-temp-buffer restores
    ;; whatever buffer was current on entry, which would silently undo the
    ;; switch-to-buffer below (the selected window would still show the
    ;; right buffer, but `current-buffer' would revert behind your back)
    (with-temp-buffer
      (setq return-code (call-process "python3" nil t nil script-path))
      (setq output (buffer-string)))
    (if (zerop return-code)
        (let ((html-file (string-trim output)))
          (if (file-exists-p html-file)
              (progn
                (let ((shr-width (window-body-width)))
                  (eww-open-file html-file))
                ;; Rename buffer, focus it, and adjust text scale
                (rename-buffer buffer-name)
                (switch-to-buffer buffer-name)
                (text-scale-adjust 7))
            (message "No HTML file returned from the script.")))
      (message "Failed to execute Python script. Output:\n%s" output))))
