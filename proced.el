(use-package proced
  :ensure t
  :functions ar/proced--hook-fun
  :commands (proced ar/quick-kill-process)
  :hook (proced-mode . ar/proced--hook-fun)
  :config
  (defun ar/proced--hook-fun ()
    (setq proced-auto-update-flag t))

  (require 'map)
  (require 'proced)
  (require 'seq)

  (defun ar/proced--pad-string (string width &optional right-align)
    "Pad STRING with spaces to WIDTH.
If RIGHT-ALIGN is non-nil, align STRING to the right."
    (let* ((string (or string ""))
           (padding (max 0 (- width (string-width string))))
           (spaces (make-string padding ? )))
      (if right-align
          (concat spaces string)
        (concat string spaces))))

  (defun ar/proced--format-candidate (process widths)
    "Format PROCESS for completion using WIDTHS."
    (let* ((pid (number-to-string (or (map-elt process 'pid) 0)))
           (user (or (map-elt process 'user) ""))
           (state (or (map-elt process 'state) ""))
           (start (if-let ((value (map-elt process 'start)))
                      (proced-format-start value)
                    ""))
           (pcpu (proced-format-cpu (or (map-elt process 'pcpu) 0.0)))
           (pmem (proced-format-mem (or (map-elt process 'pmem) 0.0)))
           (command (or (map-elt process 'args)
                        (map-elt process 'comm)
                        "")))
      (format "%s %s %s %s %s %s %s"
              (ar/proced--pad-string pid (map-elt widths 'pid) t)
              (ar/proced--pad-string user (map-elt widths 'user))
              (ar/proced--pad-string state (map-elt widths 'state))
              (ar/proced--pad-string start (map-elt widths 'start))
              (ar/proced--pad-string pcpu (map-elt widths 'pcpu) t)
              (ar/proced--pad-string pmem (map-elt widths 'pmem) t)
              command)))

  (defun ar/quick-kill-process ()
    "Select a process from a compact, column-aligned list and kill it."
    (interactive)
    (let* ((processes (proced-process-attributes))
           (widths
            (seq-reduce
             (lambda (acc attributes)
               (let* ((process (cdr attributes))
                      (pid (number-to-string (or (map-elt process 'pid) 0)))
                      (user (or (map-elt process 'user) ""))
                      (state (or (map-elt process 'state) ""))
                      (start (if-let ((value (map-elt process 'start)))
                                 (proced-format-start value)
                               ""))
                      (pcpu (proced-format-cpu (or (map-elt process 'pcpu) 0.0)))
                      (pmem (proced-format-mem (or (map-elt process 'pmem) 0.0))))
                 (list
                  (cons 'pid (max (map-elt acc 'pid) (string-width pid)))
                  (cons 'user (max (map-elt acc 'user) (string-width user)))
                  (cons 'state (max (map-elt acc 'state) (string-width state)))
                  (cons 'start (max (map-elt acc 'start) (string-width start)))
                  (cons 'pcpu (max (map-elt acc 'pcpu) (string-width pcpu)))
                  (cons 'pmem (max (map-elt acc 'pmem) (string-width pmem))))))
             processes
             '((pid . 3)
               (user . 4)
               (state . 4)
               (start . 5)
               (pcpu . 4)
               (pmem . 4))))
           (candidates
            (mapcar (lambda (attributes)
                      (let* ((process (cdr attributes))
                             (label (ar/proced--format-candidate process widths)))
                        (cons label
                              process)))
                    processes))
           (selection (map-elt candidates
                               (completing-read "kill process (PID USER STAT START %CPU %MEM COMMAND): "
                                                (seq-sort
                                                 (lambda (p1 p2)
                                                   (string-lessp (or (map-elt (cdr p1) 'comm) "")
                                                                 (or (map-elt (cdr p2) 'comm) "")))
                                                 candidates) nil t)))
           (prompt-title (format "%s %s %s"
                                 (map-elt selection 'pid)
                                 (map-elt selection 'user)
                                 (map-elt selection 'comm))))
      (when (y-or-n-p (format "Kill? %s" prompt-title))
        (if (eq (signal-process (map-elt selection 'pid) 9) 0)
            (message "killed: %s" prompt-title)
          (message "error: could not kill %s" prompt-title)))))

 )
