(defvar scaga-timer nil)
(defvar scaga-next nil)

(defun scaga-process-summary ()
  (let (ret)
    (if (process-live-p scaga-process)
        (let* ((pid (process-id scaga-process))
               (pa (process-attributes pid))
               (rss (cdr (assq 'rss pa)))
               (state (cdr (assq 'state pa)))
               (pcpu (cdr (assq 'pcpu pa))))
          (if rss (push (format "RSS: %dKB" rss) ret))
          (if state (push (format "state: %S" state) ret))
          (if pcpu (push (format "CPU: %f%%" pcpu) ret)))
      (push "(dead)" ret))
    (mapconcat #'identity ret " | ")))

(defun scaga-process-status (&optional status)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (cdr (assq 'process-status scaga-markers)))
      (insert "Process status: " (scaga-process-summary) "  " (or status (format "%S\n" (process-status scaga-process))))
      (delete-region (point) (cdr (assq 'after-process-status scaga-markers))))))

(defun scaga-sentinel (process status)
  (let* ((pbuffer (process-buffer process))
         (buffer (with-current-buffer pbuffer scaga-buffer)))
    (with-current-buffer buffer
      (when (eq process scaga-process)
        (scaga-process-status (if (equal status "run") "run\n" status))))))

(defun scaga-start (&rest args)
  (let ((pbuffer (generate-new-buffer "*scaga*"))
        (buffer (current-buffer)))
    (setq scaga-rules (list (concat scaga-rules-dir "/newrules.scaga")))
    (setq scaga-calls (list (concat scaga-rules-dir "/calls.scaga")))
    (with-current-buffer pbuffer
      (set (make-local-variable 'scaga-buffer) buffer)
      (set (make-local-variable 'scaga-sequence) 0))
    (setq scaga-rules-buffer (find-file-noselect (car scaga-rules)))
    (setq scaga-process (apply #'start-process "SCAGA" pbuffer args))
    (set-process-sentinel scaga-process #'scaga-sentinel)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (cdr (assq 'process-status scaga-markers)))
        (insert "Process status: PID " (format "%S" (process-id scaga-process)) "\n")
        (delete-region (point) (cdr (assq 'after-process-status scaga-markers)))))))

(defun scaga-insert-excerpt (file around-line)
  (when nil
  (let ((excerpt
         (with-temp-buffer
           (insert-file-contents file)
           (goto-line (- around-line 10))
           (buffer-substring (point) (save-excursion (forward-line 21) (point))))))
    (insert (propertize excerpt 'face '(:background "#ffffcc"))))))

(defun scaga-r-command (beg end)
  "Create a new rule covering the selected region."
  (interactive "r")
  (when (or (not (use-region-p))
            (= beg end))
    (setq beg (save-excursion (beginning-of-line) (point))
          end (save-excursion (end-of-line) (point))))
  (let* ((path (scaga-region-to-path beg end))
         (comment (read-from-minibuffer (concat "Comment for rule " path ": "))))
    (with-current-buffer scaga-rules-buffer
      (save-excursion)
      (goto-char (point-max))
      (when comment
        (insert "# " comment "\n"))
      (insert path "\n")
      (save-buffer))))

(defun scaga-delete-excerpt (excerpt)
      (when excerpt
        (delete-region (caar excerpt) (cdar excerpt))
        (remove-text-properties (or (previous-single-property-change (point) 'scaga-excerpt)
                                    (point-min))
                                (or (next-single-property-change (point) 'scaga-excerpt)
                                    (point-max))
                                '(scaga-excerpt nil))))

(defun scaga-excerpt-beg (type)
  (cond
   ((numberp type)
    (forward-line (- type)))
   ((eq type 'defun)
    (beginning-of-defun)))
  (point))

(defun scaga-excerpt-end (type)
  (cond
   ((numberp type)
    (forward-line type))
   ((eq type 'defun)
    (end-of-defun)))
  (point))

(defun scaga-create-excerpt (type)
  (let ((filename (car flc))
        (lineno (cadr flc)))
    (save-excursion
      (setq m9 (copy-marker (point-at-bol)))
      (forward-line)
      (setq m0 (copy-marker (point-marker)))
      (let ((excerpts
             (with-current-buffer (find-file-noselect filename)
               (goto-line lineno)
               (let ((beg (save-excursion (scaga-excerpt-beg type)))
                     (end (save-excursion (scaga-excerpt-end type)))
                     (begl (save-excursion (back-to-indentation) (point)))
                     (endl (point-at-eol)))
                 ;;(font-lock-fontify-region beg end)
                 (list (buffer-substring beg begl)
                       (buffer-substring begl endl)
                       (buffer-substring endl end))))))
        (insert (propertize (nth 0 excerpts) 'face '(:background "#ffffcc")))
        (insert (propertize (nth 1 excerpts) 'face '(:background "#ffcccc")))
        (insert (propertize (nth 2 excerpts) 'face '(:background "#ffffcc"))))
      (setq m1 (copy-marker (point-marker)))
      (put-text-property m9 m1 'scaga-excerpt (list (cons m0 m1) type)))))

(defun scaga-plus-command ()
  "Display more context for defun at point."
  (interactive)
  (let* ((flc (get-text-property (point) 'scaga-flc))
         (excerpt (get-text-property (point) 'scaga-excerpt))
         (lines (1+ (or (and (numberp (cadr excerpt)) (cadr excerpt)) 4))))
    (when flc
      (scaga-delete-excerpt excerpt)
      (scaga-create-excerpt lines))))

(defun scaga-minus-command ()
  "Display less context for defun at point."
  (interactive)
  (let* ((flc (get-text-property (point) 'scaga-flc))
         (excerpt (get-text-property (point) 'scaga-excerpt))
         (lines (1- (or (and (numberp (cadr excerpt)) (cadr excerpt)) 4))))
    (when flc
      (scaga-delete-excerpt excerpt)
      (scaga-create-excerpt lines))))

(defun scaga-f-command ()
  "Display entire defun at point."
  (interactive)
  (let* ((flc (get-text-property (point) 'scaga-flc))
         (excerpt (get-text-property (point) 'scaga-excerpt)))
    (when flc
      (scaga-delete-excerpt excerpt)
      (unless excerpt
        (scaga-create-excerpt 'defun)))))

(defun scaga-l-command ()
  "Rerun current iteration."
  (interactive)
  (setq scaga-next "--next=retry"))

(defun scaga-L-command ()
  "Rerun loop from iteration 1."
  (interactive)
  (setq scaga-next "--next=rules-loop"))

(defun scaga-tab-command ()
  "Show source code for defun at point or hide it."
  (interactive)
  (let ((flc (get-text-property (point) 'scaga-flc))
        (excerpt (get-text-property (point) 'scaga-excerpt)))
    (when flc
      (if excerpt
          (scaga-delete-excerpt excerpt)
        (scaga-create-excerpt 5)))))

(defun scaga-ret-command ()
  "Visit the defun at point."
  (interactive)
  (let ((flc (get-text-property (point) 'scaga-flc))
        (excerpt (get-text-property (point) 'scaga-excerpt)))
    (when flc
      (let* ((filename (car flc))
             (lineno (cadr flc))
             (buffer (find-file filename)))
        (when buffer
          (goto-line lineno))))))

(defun scaga-region-to-path (beg end)
  "Convert the current region to a path."
  (interactive "r")
  (let ((next beg)
        patterns patterns2)
    (while (< next (1- end))
      (goto-char next)
      (push (get-text-property next 'scaga-pattern) patterns)
      (setq next (next-single-property-change next 'scaga-pattern (current-buffer) (1- end))))
    (dolist (pattern patterns)
      (when pattern
        (push pattern patterns2)))
    (message (mapconcat #'identity patterns2 " > "))))

(defun scaga-update (buffer)
  "Automated update function run when there is new SCAGA process output."
  (when (buffer-live-p (with-current-buffer buffer (process-buffer scaga-process)))
    (with-current-buffer
        (with-current-buffer buffer
          (scaga-process-status)
          (process-buffer scaga-process))
      (save-excursion
        (goto-char 1)
        (when (looking-at-p ".*\n[^!]")
          (re-search-forward "\n")
          (delete-region 1 (point)))
        (while (looking-at-p (concat ".*"
                                     "\n"
                                     "!!!sequence: "
                                     (regexp-quote (number-to-string scaga-sequence))
                                     "\n"))
          (save-match-data
            (re-search-forward (concat "^"
                                       "\\(.*\\)"
                                       "\n"
                                       "!!!sequence: "
                                       (regexp-quote (number-to-string scaga-sequence))
                                       "\n")
                               nil t)
            (incf scaga-sequence)
            (let* ((line (match-string 1))
                   (patterns (split-string line " > "))
                   after-line new-components lines
                   filename lineno colno)
              (if (= (length patterns) 1)
                  (with-current-buffer buffer
                    (save-excursion
                      (goto-char (point-max))
                      (let ((inhibit-read-only t))
                        (insert (propertize line
                                            'face `(:background "#888888")))
                        (insert "\n"))))
                (dolist (pattern patterns)
                  (setq new-components nil)
                  (dolist (component (split-string pattern " = "))
                    (cond
                     ((string-match "^FLC:\\(.*?\\):\\(.*?\\):\\(.*?\\)$" component)
                      (setq filename (concat scaga-source "/" (match-string 1 component)))
                      (setq lineno (string-to-number (match-string 2 component)))
                      (setq colno (string-to-number (match-string 3 component))))
                     ((string-match "^component:" component)
                      (push component new-components))
                     ((string-match "^intype:" component)
                      t)
                     ((string-match "^/" component)
                      t)
                     ((string-match "^'" component)
                      t)
                     ((string-match "0x" component)
                      t)
                     (t
                      (push component new-components))))
                  (push (propertize
                         (mapconcat #'identity (nreverse new-components) " = ")
                         'scaga-pattern
                         (mapconcat #'identity (nreverse new-components) " = ")
                         'scaga-flc
                         `(,filename ,lineno ,colno)
                         'face
                         `(:background "#ccccff"))
                        lines)
                  (with-current-buffer buffer
                    (save-excursion
                      (goto-char (point-max))
                      (let ((inhibit-read-only t))
                        (dolist (line lines)
                          (insert line)
                          (insert "\n"))
                        (setq lines nil)
                        (dolist (f after-line)
                          (funcall f))))))
                (when (process-live-p scaga-process)
                  (process-send-string scaga-process (concat (or scaga-next "--next") "\n")))
                (setq scaga-next nil)
                (with-current-buffer buffer
                  (save-excursion
                    (goto-char (point-max))
                    (let ((inhibit-read-only t))
                      (insert "\n")))))
              (delete-region 1 (point)))))))))

(define-derived-mode scaga-mode special-mode "SCAGA"
  "SCAGA mode, see https://github.com/pipcet/scaga"
  (setq buffer-read-only nil)
  t)

(define-key scaga-mode-map (kbd "TAB") #'scaga-tab-command)
(define-key scaga-mode-map (kbd "RET") #'scaga-ret-command)
(define-key scaga-mode-map (kbd "+") #'scaga-plus-command)
(define-key scaga-mode-map (kbd "-") #'scaga-minus-command)
(define-key scaga-mode-map (kbd "f") #'scaga-f-command)
(define-key scaga-mode-map (kbd "r") #'scaga-r-command)
(define-key scaga-mode-map (kbd "R") #'scaga-restart)
(define-key scaga-mode-map (kbd "l") #'scaga-l-command)
(define-key scaga-mode-map (kbd "L") #'scaga-L-command)

(defun scaga-restart ()
  (interactive)
  (when (process-live-p scaga-process)
    (ignore-errors (kill-process scaga-process)))
  (setq buffer-read-only nil)
  (when (buffer-live-p (process-buffer scaga-process))
    (with-current-buffer (process-buffer scaga-process)
      (delete-region (point-min) (point-max))))
  (apply #'scaga-start (scaga-args)))

(defvar scaga-path nil "Path to SCAGA Perl program")
(defvar scaga-exec nil "Path to executable being analyzed")
(defvar scaga-source nil "Path to source code for analyzed program")
(defvar scaga-rules-dir nil "Path to SCAGA rules files")
(defvar scaga-cc nil)
(defvar scaga-lto nil)
(defvar scaga-loop-forever nil)
(defvar scaga-ignore-noreturn nil)

(defvar scaga-sequence nil "Sequence number of next expected SCAGA reply.")

(defun scaga-args ()
  (let (args)
    (push "perl" args)
    (push scaga-path args)
    (push (concat "--executable=" scaga-exec) args)
    (push (concat "--cc=" scaga-cc) args)
    (push (concat "--lto=" scaga-lto) args)
    (setq scaga-rules (list (concat scaga-rules-dir "/newrules.scaga")))
    (setq scaga-calls (list (concat scaga-rules-dir "/calls.scaga")))
    (dolist (scaga-rules-file scaga-rules)
      (push (concat "--rules=" scaga-rules-file) args))
    (push (concat "--source-directory=" scaga-source) args)
    (dolist (scaga-calls-file scaga-calls)
      (push (concat "--calls=" scaga-calls-file) args))
    (push "--last=1" args)
    (push "--wait-for-next=1" args)
    (if scaga-loop-forever (push "--loop-rules=-1" args))
    (setq args (nreverse args))
    args))

(defun scaga ()
  (interactive)
  (let ((buffer (get-buffer-create "*SCAGA*"))
        args)
    (with-current-buffer buffer
      (scaga-mode)
      (set (make-local-variable 'scaga-path) "/home/pip/git/scaga/scaga-extend.pl")
      (set (make-local-variable 'scaga-exec) "/usr/local/bin/emacs")
      (set (make-local-variable 'scaga-source) "/home/pip/git/emacs/src")
      (set (make-local-variable 'scaga-rules-dir) "/home/pip/git/scaga")
      (set (make-local-variable 'scaga-cc) "gcc -Demacs -I/home/pip/git/emacs/src -I/home/pip/git/emacs/lib $(pkg-config --cflags gtk+-3.0) -fno-inline-functions -fno-inline-functions-called-once -fno-inline-small-functions -fno-optimize-sibling-calls -ffunction-sections -fno-function-cse -flto -fdevirtualize-speculatively -fdevirtualize-at-ltrans -ggdb3 -O3 -c")
      (set (make-local-variable 'scaga-lto) "/usr/lib/gcc/x86_64-linux-gnu/5.2.1/lto1 -S -ggdb3 -O3 -fno-inline-functions -fno-inline-functions-called-once -fno-inline-small-functions -ffunction-sections -flto -fno-function-cse -flto -fdevirtualize-speculatively -fdevirtualize-at-ltrans -flto -mtune=generic -march=x86-64 -mtune=generic -march=x86-64 -O3 -version -fmath-errno -fsigned-zeros -ftrapping-math -fno-trapv -fno-strict-overflow -fno-openmp -fno-openacc -fno-function-cse -fdevirtualize-speculatively -fdevirtualize-at-ltrans")
      (set (make-local-variable 'scaga-rules-dir) "/home/pip/git/scaga")
      (set (make-local-variable 'scaga-ignore-noreturn) t)
      (set (make-local-variable 'scaga-loop-forever) t)
      (set (make-local-variable 'scaga-rules) nil)
      (set (make-local-variable 'scaga-calls) nil)
      (if scaga-timer
          (cancel-timer scaga-timer))
      (make-local-variable 'scaga-timer)
      (if scaga-timer
          (cancel-timer scaga-timer))
      (setq scaga-timer (run-with-timer 0 .5 #'scaga-update (current-buffer)))
      (set (make-local-variable 'scaga-args) args)
      (scaga-widgets)
      )
    (pop-to-buffer buffer)))

(require 'wid-edit)
(require 'widget)

(defvar scaga-widgets nil)
(defun scaga-widgets ()
  (erase-buffer)
  (remove-overlays)
  (widget-insert "Arguments:\n")
  (setq scaga-widgets nil)
  (push (cons 'scaga-path
              (widget-create 'editable-field :format "SCAGA:            %v"
                             scaga-path))
        scaga-widgets)
  (push (cons 'scaga-rules-dir
              (widget-create 'editable-field :format "rules directory:  %v"
                             scaga-rules-dir))
        scaga-widgets)
  (push (cons 'scaga-cc
              (widget-create 'editable-field :format "CC:               %v"
                             scaga-cc))
        scaga-widgets)
  (push (cons 'scaga-lto
              (widget-create 'editable-field :format "LTO:              %v"
                             scaga-lto))
        scaga-widgets)
  (push (cons 'scaga-source
              (widget-create 'editable-field :format "source directory: %v"
                             scaga-source))
        scaga-widgets)
  (push (cons 'scaga-exec
              (widget-create 'editable-field :format "executable:       %v"
                             scaga-exec))
        scaga-widgets)
  (push (cons 'scaga-ignore-noreturn
              (widget-create 'checkbox
                             scaga-ignore-noreturn))
        scaga-widgets)
  (widget-insert " ignore noreturn code paths\n")
  (push (cons 'scaga-loop-forever
              (widget-create 'checkbox
                             scaga-loop-forever))
        scaga-widgets)
  (widget-insert " loop forever\n")
  (push (cons 'restart
              (widget-create 'push-button
                             :notify (lambda (&rest ignore)
                                       (dolist (pair scaga-widgets)
                                         (set (car pair) (widget-value (cdr pair))))
                                       (scaga-restart))
                             "Start SCAGA"))
        scaga-widgets)
  (widget-insert "\n")
  (setq buffer-read-only nil)
  (widget-setup)
  (scaga-mode)
  (setq buffer-read-only nil))
    (insert "Process status: not started\n")

(provide 'scaga-mode)
