;;; defuns-tests.el --- ERT tests for defuns.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(load-file (expand-file-name "../defuns.el"
                              (file-name-directory (or load-file-name buffer-file-name))))

;;; sta:last-term-buffer

(ert-deftest defuns-test-last-term-buffer-finds-vterm ()
  (let ((term-buf (generate-new-buffer "*fake-vterm*"))
        (other-buf (generate-new-buffer "*other*")))
    (unwind-protect
        (progn
          (with-current-buffer term-buf (setq major-mode 'vterm-mode))
          (should (eq (sta:last-term-buffer (list other-buf term-buf)) term-buf)))
      (kill-buffer term-buf)
      (kill-buffer other-buf))))

(ert-deftest defuns-test-last-term-buffer-none-found ()
  (let ((other-buf (generate-new-buffer "*other*")))
    (unwind-protect
        (should (null (sta:last-term-buffer (list other-buf))))
      (kill-buffer other-buf))))

(ert-deftest defuns-test-last-term-buffer-empty-list ()
  (should (null (sta:last-term-buffer nil))))

;;; sta:shuffle-list

(ert-deftest defuns-test-shuffle-list-preserves-elements ()
  (let* ((original '(1 2 3 4 5))
         (shuffled (sta:shuffle-list (copy-sequence original))))
    (should (equal (sort (copy-sequence shuffled) #'<) original))
    (should (= (length shuffled) (length original)))))

(ert-deftest defuns-test-shuffle-list-single-element ()
  (should (equal (sta:shuffle-list (list 1)) (list 1))))

(ert-deftest defuns-test-shuffle-list-empty ()
  (should (null (sta:shuffle-list nil))))

;;; sta:get-random-line-from-file

(ert-deftest defuns-test-get-random-line-from-file ()
  (let ((tmpfile (make-temp-file "defuns-test")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile (insert "line-a\nline-b\nline-c\n"))
          (should (member (sta:get-random-line-from-file tmpfile)
                           '("line-a" "line-b" "line-c"))))
      (delete-file tmpfile))))

(ert-deftest defuns-test-get-random-line-from-file-single-line ()
  (let ((tmpfile (make-temp-file "defuns-test")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile (insert "only-line\n"))
          (should (equal (sta:get-random-line-from-file tmpfile) "only-line")))
      (delete-file tmpfile))))

;;; sta:get-file-python-path

(ert-deftest defuns-test-get-file-python-path ()
  (let* ((root (make-temp-file "defuns-test-py" t))
         (pkg-dir (expand-file-name "pkg/sub" root))
         (fpath (expand-file-name "mod.py" pkg-dir)))
    (unwind-protect
        (progn
          (make-directory pkg-dir t)
          (with-temp-file (expand-file-name "pyproject.toml" root) (insert ""))
          (with-temp-file fpath (insert ""))
          (should (equal (sta:get-file-python-path fpath) "pkg.sub.mod")))
      (delete-directory root t))))

;;; sta:kill-start-of-line / kill-line-no-kr / delete-line-no-kill

(ert-deftest defuns-test-kill-start-of-line ()
  (with-temp-buffer
    (insert "hello world")
    (goto-char (+ (point-min) 5))
    (sta:kill-start-of-line)
    (should (equal (buffer-string) " world"))))

(ert-deftest defuns-test-kill-line-no-kr ()
  (with-temp-buffer
    (insert "hello world")
    (goto-char (+ (point-min) 5))
    (let ((kill-ring nil))
      (sta:kill-line-no-kr)
      (should (equal (buffer-string) "hello"))
      (should (null kill-ring)))))

(ert-deftest defuns-test-delete-line-no-kill ()
  (with-temp-buffer
    (insert "first\nsecond")
    (goto-char (point-min))
    (let ((kill-ring nil))
      (sta:delete-line-no-kill)
      (should (equal (buffer-string) "second"))
      (should (null kill-ring)))))

;;; sta:copy-line

(ert-deftest defuns-test-copy-line ()
  (with-temp-buffer
    (insert "line one\nline two\n")
    (goto-char (point-min))
    (let ((kill-ring nil))
      (sta:copy-line)
      (should (equal (current-kill 0) "line one\n"))
      (should (equal (buffer-string) "line one\nline two\n")))))

;;; sta:move-line-up / sta:move-line-down

(ert-deftest defuns-test-move-line-up ()
  (with-temp-buffer
    (fundamental-mode)
    (insert "first\nsecond\n")
    (goto-char (point-min))
    (forward-line 1)
    (sta:move-line-up)
    (should (equal (buffer-string) "second\nfirst\n"))))

(ert-deftest defuns-test-move-line-down ()
  (with-temp-buffer
    (fundamental-mode)
    (insert "first\nsecond\n")
    (goto-char (point-min))
    (sta:move-line-down)
    (should (equal (buffer-string) "second\nfirst\n"))))

;;; sta:go-to-scratch / sta:go-to-text-scratch

(ert-deftest defuns-test-go-to-scratch ()
  (let ((scratch (get-buffer-create "*scratch*")))
    (save-window-excursion
      (with-temp-buffer
        (sta:go-to-scratch)
        (should (eq (current-buffer) scratch))))))

(ert-deftest defuns-test-go-to-text-scratch ()
  (unwind-protect
      (save-window-excursion
        (sta:go-to-text-scratch)
        (should (equal (buffer-name) "*itchy-scratch*"))
        (should (eq major-mode 'text-mode)))
    (when (get-buffer "*itchy-scratch*")
      (kill-buffer "*itchy-scratch*"))))

;;; sta:reset-zoom

(ert-deftest defuns-test-reset-zoom ()
  (with-temp-buffer
    (text-scale-set 3)
    (sta:reset-zoom)
    (should (= text-scale-mode-amount 0))))

;;; sta:copy-buffer-file-name-as-kill

(ert-deftest defuns-test-copy-buffer-file-name-as-kill-filename ()
  (with-temp-buffer
    (let ((buffer-file-name "/tmp/some/path/file.txt")
          (kill-ring nil))
      (sta:copy-buffer-file-name-as-kill ?f)
      (should (equal (current-kill 0) "/tmp/some/path/file.txt")))))

(ert-deftest defuns-test-copy-buffer-file-name-as-kill-name ()
  (with-temp-buffer
    (let ((buffer-file-name "/tmp/some/path/file.txt")
          (kill-ring nil))
      (sta:copy-buffer-file-name-as-kill ?n)
      (should (equal (current-kill 0) "file.txt")))))

(ert-deftest defuns-test-copy-buffer-file-name-as-kill-directory ()
  (with-temp-buffer
    (let ((buffer-file-name "/tmp/some/path/file.txt")
          (kill-ring nil))
      (sta:copy-buffer-file-name-as-kill ?d)
      (should (equal (current-kill 0) "/tmp/some/path/")))))

(ert-deftest defuns-test-copy-buffer-file-name-as-kill-python-path ()
  (with-temp-buffer
    (let* ((root (make-temp-file "defuns-test-py" t))
           (fpath (expand-file-name "pkg/mod.py" root))
           (buffer-file-name fpath)
           (kill-ring nil))
      (unwind-protect
          (progn
            (make-directory (file-name-directory fpath) t)
            (with-temp-file (expand-file-name "pyproject.toml" root) (insert ""))
            (with-temp-file fpath (insert ""))
            (sta:copy-buffer-file-name-as-kill ?p)
            (should (equal (current-kill 0) "pkg.mod")))
        (delete-directory root t)))))

;;; sta:buffer-path

(ert-deftest defuns-test-buffer-path-projectile-project ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'projectile-project-p) (lambda () t))
              ((symbol-function 'projectile-project-root) (lambda () "/proj/root/")))
      (should (equal (sta:buffer-path) "/proj/root/")))))

(ert-deftest defuns-test-buffer-path-non-project-file ()
  (with-temp-buffer
    (let ((buffer-file-name "/tmp/some/path/file.txt"))
      (cl-letf (((symbol-function 'projectile-project-p) (lambda () nil)))
        (should (equal (sta:buffer-path) "/tmp/some/path/"))))))

(ert-deftest defuns-test-buffer-path-no-file-no-project ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'projectile-project-p) (lambda () nil)))
      (should (null (sta:buffer-path))))))

;;; sta:nuke-stale-elc

(ert-deftest defuns-test-nuke-stale-elc-deletes-elc-files ()
  (let ((root (make-temp-file "defuns-test-elc" t)))
    (unwind-protect
        (let* ((elpa (expand-file-name "elpa" root))
               (elc-file (expand-file-name "pkg/foo.elc" elpa))
               (el-file (expand-file-name "pkg/foo.el" elpa)))
          (make-directory (file-name-directory elc-file) t)
          (with-temp-file elc-file (insert ""))
          (with-temp-file el-file (insert ""))
          (let ((user-emacs-directory root))
            (sta:nuke-stale-elc))
          (should-not (file-exists-p elc-file))
          (should (file-exists-p el-file)))
      (delete-directory root t))))

;;; sta:copy-buffer-file-name-as-kill (remaining branches)

(ert-deftest defuns-test-copy-buffer-file-name-as-kill-python-import ()
  (with-temp-buffer
    (let* ((root (make-temp-file "defuns-test-py" t))
           (fpath (expand-file-name "pkg/mod.py" root))
           (buffer-file-name fpath)
           (kill-ring nil))
      (unwind-protect
          (progn
            (make-directory (file-name-directory fpath) t)
            (with-temp-file (expand-file-name "pyproject.toml" root) (insert ""))
            (with-temp-file fpath (insert ""))
            (sta:copy-buffer-file-name-as-kill ?i)
            (should (equal (current-kill 0) "from pkg.mod import ")))
        (delete-directory root t)))))

(ert-deftest defuns-test-copy-buffer-file-name-as-kill-buffer-name ()
  (with-temp-buffer
    (rename-buffer "my-special-buffer" t)
    (let ((kill-ring nil))
      (sta:copy-buffer-file-name-as-kill ?b)
      (should (equal (current-kill 0) "my-special-buffer")))))

(ert-deftest defuns-test-copy-buffer-file-name-as-kill-major-mode ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((kill-ring nil))
      (sta:copy-buffer-file-name-as-kill ?m)
      (should (equal (current-kill 0) "emacs-lisp-mode")))))

(ert-deftest defuns-test-copy-buffer-file-name-as-kill-unknown-choice-quits ()
  (with-temp-buffer
    (let ((kill-ring nil) (msg nil))
      (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (sta:copy-buffer-file-name-as-kill ?x))
      (should (null kill-ring))
      (should (equal msg "Quit")))))

;;; sta:find-file-dired

(ert-deftest defuns-test-find-file-dired-with-dir ()
  (let (called)
    (cl-letf (((symbol-function 'sta:buffer-path) (lambda () "/some/dir/"))
              ((symbol-function 'find-dired) (lambda (dir args) (setq called (list dir args)))))
      (sta:find-file-dired "*.py")
      (should (equal called (list "/some/dir/" (concat "-name " (shell-quote-argument "*.py"))))))))

(ert-deftest defuns-test-find-file-dired-no-dir ()
  (let (msg)
    (cl-letf (((symbol-function 'sta:buffer-path) (lambda () nil))
              ((symbol-function 'message) (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (sta:find-file-dired "*.py")
      (should (equal msg "No valid directory found.")))))

;;; sta:zone-choose

(ert-deftest defuns-test-zone-choose ()
  (require 'zone)
  (let (captured)
    (cl-letf (((symbol-function 'zone) (lambda () (setq captured zone-programs))))
      (sta:zone-choose "zone-pgm-jitter")
      (should (equal captured '(zone-pgm-jitter))))))

;;; sta:prev-window

(ert-deftest defuns-test-prev-window ()
  (let (arg)
    (cl-letf (((symbol-function 'other-window) (lambda (n) (setq arg n))))
      (sta:prev-window)
      (should (= arg -1)))))

;;; er-byte-compile-init-dir

(ert-deftest defuns-test-byte-compile-init-dir ()
  (let (args)
    (cl-letf (((symbol-function 'byte-recompile-directory) (lambda (&rest a) (setq args a))))
      (er-byte-compile-init-dir)
      (should (equal args (list user-emacs-directory 0))))))

;;; sta:diff-last-two-kills

(ert-deftest defuns-test-diff-last-two-kills ()
  (let (diff-args)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'diff) (lambda (old new &rest args) (setq diff-args (list old new args)))))
            ;; current-kill reads via kill-ring-yank-pointer, not kill-ring
            ;; directly, so both need to be bound together
            (let* ((kill-ring (list "second-kill" "first-kill"))
                   (kill-ring-yank-pointer kill-ring))
              (sta:diff-last-two-kills)))
          (should (equal diff-args '("/tmp/old-kill" "/tmp/new-kill" ("-u" t))))
          (should (equal (with-temp-buffer (insert-file-contents "/tmp/new-kill") (buffer-string))
                          "second-kill"))
          (should (equal (with-temp-buffer (insert-file-contents "/tmp/old-kill") (buffer-string))
                          "first-kill")))
      (when (file-exists-p "/tmp/new-kill") (delete-file "/tmp/new-kill"))
      (when (file-exists-p "/tmp/old-kill") (delete-file "/tmp/old-kill")))))

;;; sta:reformat-xml

(ert-deftest defuns-test-reformat-xml ()
  (with-temp-buffer
    (insert "<a><b>1</b></a>")
    (sgml-mode)
    (sta:reformat-xml)
    (should (string-match-p "<a>" (buffer-string)))
    (should (string-match-p "<b>1" (buffer-string)))))

;;; sta:ascii-translit-region / sta:snakecase-translit-region

(ert-deftest defuns-test-ascii-translit-region ()
  (with-temp-buffer
    (let (cmd)
      (cl-letf (((symbol-function 'shell-command-on-region)
                 (lambda (start end command &optional _out replace &rest _)
                   (setq cmd command)
                   (when replace (delete-region start end) (goto-char start) (insert "translit-result")))))
        (insert "hello")
        (sta:ascii-translit-region (point-min) (point-max))
        (should (equal cmd "iconv -t ASCII//TRANSLIT"))
        (should (equal (buffer-string) "translit-result"))))))

(ert-deftest defuns-test-snakecase-translit-region ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'shell-command-on-region)
               (lambda (start end command &optional _out replace &rest _)
                 (when replace (delete-region start end) (goto-char start) (insert "hello world")))))
      (insert "Hello World")
      (sta:snakecase-translit-region (point-min) (point-max))
      (should (equal (buffer-string) "hello_world")))))

;;; bookmark helpers

(ert-deftest defuns-test-ido-switch-bookmark ()
  (let (jumped)
    (cl-letf (((symbol-function 'bookmark-all-names) (lambda () '("a" "b")))
              ((symbol-function 'completing-read) (lambda (&rest _) "a"))
              ((symbol-function 'bookmark-jump) (lambda (name) (setq jumped name))))
      (sta:ido-switch-bookmark)
      (should (equal jumped "a")))))

(ert-deftest defuns-test-ido-delete-bookmark ()
  (let (deleted)
    (cl-letf (((symbol-function 'bookmark-all-names) (lambda () '("a" "b")))
              ((symbol-function 'completing-read) (lambda (&rest _) "b"))
              ((symbol-function 'bookmark-delete) (lambda (name) (setq deleted name))))
      (sta:ido-delete-bookmark)
      (should (equal deleted "b")))))

(ert-deftest defuns-test-projectile-bookmark-set ()
  (let (set-name)
    (cl-letf (((symbol-function 'bookmark-set) (lambda (name) (setq set-name name))))
      (sta:projectile-bookmark-set "myproj/")
      (should (equal set-name "myproj/")))))

;;; sta:yank-pop

(ert-deftest defuns-test-yank-pop ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "picked-entry")))
      (sta:yank-pop))
    (should (equal (buffer-string) "picked-entry"))))

;;; sta:tree

(ert-deftest defuns-test-tree ()
  (provide 'treemacs)
  (let (called)
    (cl-letf (((symbol-function 'treemacs-add-and-display-current-project-exclusively)
               (lambda () (setq called t))))
      (sta:tree)
      (should called))))

;;; sta:get-term

(ert-deftest defuns-test-get-term-switches-to-existing-term ()
  (let ((term-buf (generate-new-buffer "*fake-vterm*")))
    (unwind-protect
        (progn
          (with-current-buffer term-buf (setq major-mode 'vterm-mode))
          (cl-letf (((symbol-function 'multi-vterm) (lambda (&rest _) (error "should not be called"))))
            (with-temp-buffer
              (sta:get-term)
              (should (eq (current-buffer) term-buf)))))
      (kill-buffer term-buf))))

(ert-deftest defuns-test-get-term-spawns-when-none-found ()
  (let (called)
    (cl-letf (((symbol-function 'multi-vterm) (lambda (&rest _) (setq called t))))
      (with-temp-buffer
        (sta:get-term)
        (should called)))))

(ert-deftest defuns-test-get-term-spawns-when-already-in-vterm ()
  (let ((term-buf (generate-new-buffer "*fake-vterm2*")) called)
    (unwind-protect
        (progn
          (with-current-buffer term-buf (setq major-mode 'vterm-mode))
          (cl-letf (((symbol-function 'multi-vterm) (lambda (&rest _) (setq called t))))
            (with-current-buffer term-buf
              (sta:get-term)
              (should called))))
      (kill-buffer term-buf))))

;;; window-split commands (split-window-* work fine in plain -batch)

(ert-deftest defuns-test-two-col-split ()
  (save-window-excursion
    (delete-other-windows)
    (sta:two-col-split)
    (should (= (length (window-list)) 2))))

(ert-deftest defuns-test-epic-split ()
  (save-window-excursion
    (delete-other-windows)
    (cl-letf (((symbol-function 'multi-vterm)
               (lambda (&rest _) (switch-to-buffer (generate-new-buffer "*fake-vterm*")))))
      (unwind-protect
          (progn
            (sta:epic-split)
            (should (= (length (window-list)) 3)))
        (when (get-buffer "*fake-vterm*") (kill-buffer "*fake-vterm*"))))))

(ert-deftest defuns-test-wterm-split ()
  (save-window-excursion
    (delete-other-windows)
    (cl-letf (((symbol-function 'multi-vterm)
               (lambda (&rest _) (switch-to-buffer (generate-new-buffer "*fake-vterm2*")))))
      (unwind-protect
          (progn
            (sta:wterm-split)
            (should (= (length (window-list)) 2)))
        (when (get-buffer "*fake-vterm2*") (kill-buffer "*fake-vterm2*"))))))

;;; sta:awesome-run / browser launchers

(ert-deftest defuns-test-awesome-run ()
  (let (mp-args cp-args)
    (cl-letf (((symbol-function 'make-process) (lambda (&rest args) (setq mp-args args)))
              ((symbol-function 'call-process) (lambda (&rest args) (setq cp-args args))))
      (sta:awesome-run "echo hi" 4)
      (should (equal (plist-get mp-args :command) (list shell-file-name shell-command-switch "echo hi")))
      (should (equal (car (last cp-args))
                      "awesome-client 'awful.screen.focused().tags[4]:view_only()'")))))

(ert-deftest defuns-test-vivaldi-calls-awesome-run ()
  (let (cap)
    (cl-letf (((symbol-function 'sta:awesome-run) (lambda (cmd tag) (setq cap (list cmd tag)))))
      (sta:vivaldi "https://x.com")
      (should (equal cap '("vivaldi https\\://x.com > /dev/null" 4))))))

(ert-deftest defuns-test-firefox-calls-awesome-run ()
  (let (cap)
    (cl-letf (((symbol-function 'sta:awesome-run) (lambda (cmd tag) (setq cap (list cmd tag)))))
      (sta:firefox "https://y.com")
      (should (equal cap '("firefox --new-tab https\\://y.com > /dev/null" 3))))))

(ert-deftest defuns-test-google-uses-vivaldi ()
  (let (cap)
    (cl-letf (((symbol-function 'sta:vivaldi) (lambda (url &rest _) (setq cap url))))
      (sta:google "foo bar")
      (should (equal cap "https://www.google.com/search?q=foo bar")))))

(ert-deftest defuns-test-translate-uses-vivaldi ()
  (let (cap)
    (cl-letf (((symbol-function 'sta:vivaldi) (lambda (url &rest _) (setq cap url))))
      (sta:translate "cs" "en" "ahoj")
      (should (equal cap "http://translate.google.com/#cs/en/ahoj")))))

(ert-deftest defuns-test-translate-to-en-uses-translate ()
  (let (cap)
    (cl-letf (((symbol-function 'sta:translate) (lambda (from to str) (setq cap (list from to str)))))
      (sta:translate-to-en "ahoj")
      (should (equal cap '("cs" "en" "ahoj"))))))

(ert-deftest defuns-test-region-to-browser ()
  (with-temp-buffer
    (let (cap)
      (insert "hello region")
      (cl-letf (((symbol-function 'sta:firefox) (lambda (s &rest _) (setq cap s))))
        (sta:region-to-browser (point-min) (point-max)))
      (should (equal cap "hello region")))))

(ert-deftest defuns-test-show-http-response-in-browser-no-buffer ()
  (should-not (get-buffer "*HTTP Response*"))
  (let (msg)
    (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (sta:show-http-response-in-browser))
    (should (equal msg "No *HTTP Response* buffer found, sorry."))))

(ert-deftest defuns-test-show-http-response-in-browser-with-buffer ()
  (let ((buf (generate-new-buffer "*HTTP Response*")) fname)
    (with-current-buffer buf (insert "<html>hi</html>"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'sta:firefox) (lambda (f &rest _) (setq fname f))))
            (sta:show-http-response-in-browser))
          (should (equal fname "/tmp/emacs_http_response.html"))
          (should (file-exists-p fname))
          (should-not (buffer-live-p buf)))
      (when (file-exists-p "/tmp/emacs_http_response.html")
        (delete-file "/tmp/emacs_http_response.html")))))

;;; sta:vc-forge-type / sta:vc-web-repo-url / sta:goto-forge-*
;;
;; git-link is a real dependency here (not stubbed) - `require'd directly
;; since these tests exercise git-link's own remote-parsing/host-detection
;; logic together with the sta: wrappers built on top of it. Only the two
;; functions that shell out to `git' (git-link--remote / git-link--remote-url)
;; are mocked, so real parsing/handler-lookup logic still runs.

(require 'git-link)

(defmacro defuns-test--with-remote-url (url &rest body)
  "Run BODY with the current repo's resolved remote URL mocked to URL."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'git-link--remote) (lambda () "origin"))
             ((symbol-function 'git-link--remote-url) (lambda (&rest _) ,url)))
     ,@body))

(ert-deftest defuns-test-vc-forge-type-github-ssh ()
  (defuns-test--with-remote-url "git@github.com:someuser/somerepo.git"
    (should (eq (sta:vc-forge-type) 'github))))

(ert-deftest defuns-test-vc-forge-type-github-https ()
  (defuns-test--with-remote-url "https://github.com/someuser/somerepo.git"
    (should (eq (sta:vc-forge-type) 'github))))

(ert-deftest defuns-test-vc-forge-type-gitlab ()
  (defuns-test--with-remote-url "git@gitlab.com:someuser/somerepo.git"
    (should (eq (sta:vc-forge-type) 'gitlab))))

(ert-deftest defuns-test-vc-forge-type-codeberg-is-gitea-family ()
  ;; codeberg.org runs Forgejo; git-link (and thus we) treat it as the
  ;; canonical example of the gitea/forgejo URL scheme
  (defuns-test--with-remote-url "git@codeberg.org:someuser/somerepo.git"
    (should (eq (sta:vc-forge-type) 'gitea))))

(ert-deftest defuns-test-vc-forge-type-bitbucket ()
  (defuns-test--with-remote-url "git@bitbucket.org:someuser/somerepo.git"
    (should (eq (sta:vc-forge-type) 'bitbucket))))

(ert-deftest defuns-test-vc-forge-type-sourcehut ()
  (defuns-test--with-remote-url "git@git.sr.ht:~someuser/somerepo"
    (should (eq (sta:vc-forge-type) 'sourcehut))))

(ert-deftest defuns-test-vc-forge-type-unrecognized-host ()
  (defuns-test--with-remote-url "git@git.unknown-host.test:someuser/somerepo.git"
    (should (null (sta:vc-forge-type)))))

(ert-deftest defuns-test-vc-forge-type-self-hosted-registered-via-raw-alist ()
  ;; advanced/manual fallback: directly registering with git-link's own
  ;; alists (e.g. for a forge outside sta:vc-forge-alist) still works
  (let ((git-link-remote-alist (cons '("git\\.mycompany\\.com" git-link-gitea) git-link-remote-alist)))
    (defuns-test--with-remote-url "git@git.mycompany.com:someteam/somerepo.git"
      (should (eq (sta:vc-forge-type) 'gitea)))))

;;; sta:forge-type dir-local + sta:vc-forge-alist-driven registration
;;
;; This is the primary, documented self-hosted-forge mechanism (see
;; sta:forge-type's docstring and the .dir-locals.el note in global.el) -
;; unlike the raw-alist test above, the user only names the forge software;
;; the hostname is read from the (mocked) git remote automatically.

(ert-deftest defuns-test-vc-forge-type-dir-local-registers-and-resolves ()
  (let ((sta:forge-type 'gitea))
    (defuns-test--with-remote-url "git@git.mycompany.com:someteam/somerepo.git"
      (should (eq (sta:vc-forge-type) 'gitea))
      (should (equal (sta:vc-web-repo-url) "https://git.mycompany.com/someteam/somerepo")))))

(ert-deftest defuns-test-vc-forge-type-forgejo-alias-normalizes-to-gitea ()
  (let ((sta:forge-type 'forgejo))
    (defuns-test--with-remote-url "git@git.mycompany.com:someteam/somerepo.git"
      (should (eq (sta:vc-forge-type) 'gitea)))))

(ert-deftest defuns-test-vc-forge-type-dir-local-covers-multiple-hosts-independently ()
  ;; the /www/vitek use case: one sta:forge-type setting, many repos with
  ;; different hostnames/paths, each resolved from its own actual remote
  (let ((sta:forge-type 'forgejo))
    (defuns-test--with-remote-url "git@code.ciw.cz:ciw/agent-skills.git"
      (should (equal (sta:vc-web-repo-url) "https://code.ciw.cz/ciw/agent-skills")))
    (defuns-test--with-remote-url "git@code.ciw.cz:yg/infra.git"
      (should (equal (sta:vc-web-repo-url) "https://code.ciw.cz/yg/infra")))))

(ert-deftest defuns-test-vc-forge-type-unknown-value-errors ()
  (let ((sta:forge-type 'not-a-real-forge))
    (defuns-test--with-remote-url "git@git.mycompany.com:someteam/somerepo.git"
      (should-error (sta:vc-forge-type) :type 'user-error))))

(ert-deftest defuns-test-goto-forge-file-honors-forge-type-dir-local ()
  (let ((sta:forge-type 'gitea)
        (git-link-remote-alist nil))
    (defuns-test--with-remote-url "git@git.mycompany.com:someteam/somerepo.git"
      (sta:vc--ensure-forge-registered)
      (should (eq (git-link--handler git-link-remote-alist "git.mycompany.com") 'git-link-gitea)))))

;;; sta:vc-web-repo-url

(ert-deftest defuns-test-vc-web-repo-url-ssh ()
  (defuns-test--with-remote-url "git@github.com:someuser/somerepo.git"
    (should (equal (sta:vc-web-repo-url) "https://github.com/someuser/somerepo"))))

(ert-deftest defuns-test-vc-web-repo-url-https ()
  ;; regression: the old sta:magit-get-github-web-repo-url produced a
  ;; malformed URL for https remotes - git-link's parser handles both
  ;; consistently
  (defuns-test--with-remote-url "https://github.com/someuser/somerepo.git"
    (should (equal (sta:vc-web-repo-url) "https://github.com/someuser/somerepo"))))

(ert-deftest defuns-test-vc-web-repo-url-gitlab ()
  (defuns-test--with-remote-url "git@gitlab.com:someuser/somerepo.git"
    (should (equal (sta:vc-web-repo-url) "https://gitlab.com/someuser/somerepo"))))

(ert-deftest defuns-test-vc-web-repo-url-unrecognized-host-errors ()
  (defuns-test--with-remote-url "git@git.unknown-host.test:someuser/somerepo.git"
    (should-error (sta:vc-web-repo-url) :type 'user-error)))

;;; sta:goto-forge-repo / -issues / -prs / -org

(ert-deftest defuns-test-goto-forge-repo ()
  (let (cap)
    (defuns-test--with-remote-url "git@github.com:foo/bar.git"
      (cl-letf (((symbol-function 'sta:vivaldi) (lambda (url &rest _) (setq cap url))))
        (sta:goto-forge-repo)))
    (should (equal cap "https://github.com/foo/bar"))))

(ert-deftest defuns-test-goto-forge-issues-github ()
  (let (cap)
    (defuns-test--with-remote-url "git@github.com:foo/bar.git"
      (cl-letf (((symbol-function 'sta:vivaldi) (lambda (url &rest _) (setq cap url))))
        (sta:goto-forge-issues)))
    (should (equal cap "https://github.com/foo/bar/issues"))))

(ert-deftest defuns-test-goto-forge-issues-gitlab ()
  (let (cap)
    (defuns-test--with-remote-url "git@gitlab.com:foo/bar.git"
      (cl-letf (((symbol-function 'sta:vivaldi) (lambda (url &rest _) (setq cap url))))
        (sta:goto-forge-issues)))
    (should (equal cap "https://gitlab.com/foo/bar/-/issues"))))

(ert-deftest defuns-test-goto-forge-issues-gitea ()
  (let (cap)
    (defuns-test--with-remote-url "git@codeberg.org:foo/bar.git"
      (cl-letf (((symbol-function 'sta:vivaldi) (lambda (url &rest _) (setq cap url))))
        (sta:goto-forge-issues)))
    (should (equal cap "https://codeberg.org/foo/bar/issues"))))

(ert-deftest defuns-test-goto-forge-issues-bitbucket ()
  (let (cap)
    (defuns-test--with-remote-url "git@bitbucket.org:foo/bar.git"
      (cl-letf (((symbol-function 'sta:vivaldi) (lambda (url &rest _) (setq cap url))))
        (sta:goto-forge-issues)))
    (should (equal cap "https://bitbucket.org/foo/bar/issues"))))

(ert-deftest defuns-test-goto-forge-issues-sourcehut-errors ()
  (defuns-test--with-remote-url "git@git.sr.ht:~foo/bar"
    (should-error (sta:goto-forge-issues) :type 'user-error)))

(ert-deftest defuns-test-goto-forge-prs-github ()
  (let (cap)
    (defuns-test--with-remote-url "git@github.com:foo/bar.git"
      (cl-letf (((symbol-function 'sta:vivaldi) (lambda (url &rest _) (setq cap url))))
        (sta:goto-forge-prs)))
    (should (equal cap "https://github.com/foo/bar/pulls"))))

(ert-deftest defuns-test-goto-forge-prs-gitlab ()
  (let (cap)
    (defuns-test--with-remote-url "git@gitlab.com:foo/bar.git"
      (cl-letf (((symbol-function 'sta:vivaldi) (lambda (url &rest _) (setq cap url))))
        (sta:goto-forge-prs)))
    (should (equal cap "https://gitlab.com/foo/bar/-/merge_requests"))))

(ert-deftest defuns-test-goto-forge-prs-gitea ()
  (let (cap)
    (defuns-test--with-remote-url "git@codeberg.org:foo/bar.git"
      (cl-letf (((symbol-function 'sta:vivaldi) (lambda (url &rest _) (setq cap url))))
        (sta:goto-forge-prs)))
    (should (equal cap "https://codeberg.org/foo/bar/pulls"))))

(ert-deftest defuns-test-goto-forge-prs-bitbucket ()
  (let (cap)
    (defuns-test--with-remote-url "git@bitbucket.org:foo/bar.git"
      (cl-letf (((symbol-function 'sta:vivaldi) (lambda (url &rest _) (setq cap url))))
        (sta:goto-forge-prs)))
    (should (equal cap "https://bitbucket.org/foo/bar/pull-requests"))))

(ert-deftest defuns-test-goto-forge-prs-sourcehut-errors ()
  (defuns-test--with-remote-url "git@git.sr.ht:~foo/bar"
    (should-error (sta:goto-forge-prs) :type 'user-error)))

(ert-deftest defuns-test-goto-forge-org ()
  (let (cap)
    (defuns-test--with-remote-url "git@github.com:foo/bar.git"
      (cl-letf (((symbol-function 'sta:vivaldi) (lambda (url &rest _) (setq cap url))))
        (sta:goto-forge-org)))
    (should (equal cap "https://github.com/foo"))))

(ert-deftest defuns-test-goto-forge-org-sourcehut ()
  ;; the ~-prefixed sourcehut user segment falls out of plain path
  ;; truncation with no special-casing needed
  (let (cap)
    (defuns-test--with-remote-url "git@git.sr.ht:~foo/bar"
      (cl-letf (((symbol-function 'sta:vivaldi) (lambda (url &rest _) (setq cap url))))
        (sta:goto-forge-org)))
    (should (equal cap "https://git.sr.ht/~foo"))))

;;; sta:goto-forge-file

(ert-deftest defuns-test-goto-forge-file-delegates-to-git-link ()
  (let (called)
    (cl-letf (((symbol-function 'git-link) (lambda (&rest _args) (interactive) (setq called t))))
      (call-interactively #'sta:goto-forge-file))
    (should called)))

;;; sta:rae-wotd

(ert-deftest defuns-test-rae-wotd-success ()
  (let* ((tmpdir (make-temp-file "rae-test" t))
         (html-file (expand-file-name "out.html" tmpdir))
         opened scaled)
    (unwind-protect
        (progn
          (with-temp-file html-file (insert "<html></html>"))
          (cl-letf (((symbol-function 'call-process)
                     (lambda (_prog &optional _infile dest _display &rest _args)
                       (when dest (insert html-file))
                       0))
                    ((symbol-function 'eww-open-file)
                     (lambda (f) (setq opened f) (switch-to-buffer (generate-new-buffer "*eww-fake*"))))
                    ((symbol-function 'text-scale-adjust) (lambda (n) (setq scaled n))))
            (sta:rae-wotd))
          (should (equal opened html-file))
          (should (= scaled 7))
          ;; regression: buffer-display side effects must survive past the
          ;; with-temp-buffer that captures the script's output
          (should (equal (buffer-name) "*RAE-WOTD*")))
      (delete-directory tmpdir t)
      (when (get-buffer "*RAE-WOTD*") (kill-buffer "*RAE-WOTD*")))))

(ert-deftest defuns-test-rae-wotd-script-failure ()
  (let (msg)
    (cl-letf (((symbol-function 'call-process) (lambda (&rest _) (insert "boom") 1))
              ((symbol-function 'message) (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (sta:rae-wotd))
    (should (string-match-p "Failed to execute Python script" msg))))

(ert-deftest defuns-test-rae-wotd-no-html-file ()
  (let (msg)
    (cl-letf (((symbol-function 'call-process) (lambda (&rest _) (insert "/no/such/file.html") 0))
              ((symbol-function 'message) (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (sta:rae-wotd))
    (should (equal msg "No HTML file returned from the script."))))

;;; sta:sudo-edit

(ert-deftest defuns-test-sudo-edit-no-buffer-file ()
  (with-temp-buffer
    (let (called (buffer-file-name nil))
      (cl-letf (((symbol-function 'ido-read-file-name) (lambda (&rest _) "/some/file"))
                ((symbol-function 'find-file) (lambda (path) (setq called path))))
        (sta:sudo-edit)
        (should (equal called "/sudo:root@localhost:/some/file"))))))

(ert-deftest defuns-test-sudo-edit-with-buffer-file ()
  (with-temp-buffer
    (let (called (buffer-file-name "/tmp/foo.txt"))
      (cl-letf (((symbol-function 'find-alternate-file) (lambda (path) (setq called path))))
        (sta:sudo-edit)
        (should (equal called "/sudo:root@localhost:/tmp/foo.txt"))))))

(ert-deftest defuns-test-sudo-edit-prefix-arg-forces-prompt ()
  (with-temp-buffer
    (let (called (buffer-file-name "/tmp/foo.txt"))
      (cl-letf (((symbol-function 'ido-read-file-name) (lambda (&rest _) "/some/other/file"))
                ((symbol-function 'find-file) (lambda (path) (setq called path))))
        (sta:sudo-edit t)
        (should (equal called "/sudo:root@localhost:/some/other/file"))))))

;;; sta:restart-emacs-daemon

(ert-deftest defuns-test-restart-emacs-daemon-not-a-daemon ()
  (cl-letf (((symbol-function 'daemonp) (lambda () nil)))
    (should-error (sta:restart-emacs-daemon) :type 'user-error)))

(ert-deftest defuns-test-restart-emacs-daemon-via-systemctl ()
  (let (msg)
    (cl-letf (((symbol-function 'daemonp) (lambda () t))
              ((symbol-function 'save-some-buffers) (lambda (&rest _) nil))
              ((symbol-function 'executable-find) (lambda (p) (when (equal p "systemctl") "/usr/bin/systemctl")))
              ((symbol-function 'call-process)
               (lambda (_prog &rest args)
                 (cond ((member "is-active" args) 0) ((member "restart" args) 0))))
              ((symbol-function 'message) (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (sta:restart-emacs-daemon)
      (should (equal msg "Emacs daemon restart requested")))))

(ert-deftest defuns-test-restart-emacs-daemon-systemctl-restart-fails ()
  (cl-letf (((symbol-function 'daemonp) (lambda () t))
            ((symbol-function 'save-some-buffers) (lambda (&rest _) nil))
            ((symbol-function 'executable-find) (lambda (p) (when (equal p "systemctl") "/usr/bin/systemctl")))
            ((symbol-function 'call-process)
             (lambda (_prog &rest args)
               (cond ((member "is-active" args) 0) ((member "restart" args) 1)))))
    (should-error (sta:restart-emacs-daemon) :type 'user-error)))

(ert-deftest defuns-test-restart-emacs-daemon-fallback-to-nohup ()
  (let (shell-cmd killed)
    (cl-letf (((symbol-function 'daemonp) (lambda () t))
              ((symbol-function 'save-some-buffers) (lambda (&rest _) nil))
              ((symbol-function 'executable-find) (lambda (_p) nil))
              ((symbol-function 'file-executable-p) (lambda (_p) t))
              ((symbol-function 'call-process-shell-command) (lambda (cmd &rest _) (setq shell-cmd cmd)))
              ((symbol-function 'kill-emacs) (lambda (&rest _) (setq killed t))))
      (sta:restart-emacs-daemon)
      (should killed)
      (should (string-match-p "--daemon" shell-cmd)))))

(ert-deftest defuns-test-restart-emacs-daemon-cannot-determine-how ()
  (cl-letf (((symbol-function 'daemonp) (lambda () t))
            ((symbol-function 'save-some-buffers) (lambda (&rest _) nil))
            ((symbol-function 'executable-find) (lambda (_p) nil))
            ((symbol-function 'file-executable-p) (lambda (_p) nil)))
    (should-error (sta:restart-emacs-daemon) :type 'user-error)))

(provide 'defuns-tests)
;;; defuns-tests.el ends here
