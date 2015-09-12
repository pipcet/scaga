(defvar scaga-timer nil)

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
    (set (make-local-variable 'scaga-rules-hash) (make-hash-table :test 'equal))
    (scaga-fill-rules-hash)
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

(defun scaga-range-to-path-ranges (beg end)
  (setq beg (previous-single-property-change beg 'scaga-pattern-string
                                             (current-buffer) (point-min)))
  (setq end (next-single-property-change (1- end) 'scaga-pattern-string
                                         (current-buffer) (point-max)))
  (let ((pos0 beg)
        ranges pos1)
    (while (and pos0
                (< (setq pos1 (next-single-property-change pos0 'scaga-pattern-string
                                                           (current-buffer) end))
                   end))
      (if (get-text-property pos0 'scaga-pattern-string)
          (push (cons pos0 pos1) ranges))
      (setq pos0 pos1))
    (setq ranges (nreverse ranges))
    ranges))

(defun scaga-highlight-region-path (beg end)
  (interactive "r")
  (let ((ranges (scaga-range-to-path-ranges beg end))
        overlays)
    (dolist (range ranges)
      (let ((pos0 (car range))
            (pos1 (cdr range)))
        (push (make-overlay pos0 pos1) overlays)))
    (dolist (o overlays)
      (overlay-put o 'face `(:background "#ccffcc" :weight bold)))
    `(lambda () (dolist (o ',overlays)
                  (delete-overlay o)))))

(defun scaga-highlight-region (beg end window rol)
  (when scaga-region-cleanup
    (funcall scaga-region-cleanup)
    (setq scaga-region-cleanup nil))
  (setq scaga-region-cleanup (scaga-highlight-region-path beg end)))

(defun scaga-unhighlight-region (rol)
  (when scaga-region-cleanup
    (funcall scaga-region-cleanup)
    (setq scaga-region-cleanup nil)))

(defun scaga-path-fix (path)
  "Fix a path by dropping call chain information from the last pattern."
  (let ((lp (last path)))
    (setcar lp (cl-remove-if (lambda (component)
                                         (string-match-p "^\\(FLC:\\|'\\)" component))
                             (car lp))))
  path)

(defun scaga-region-to-path (beg end)
  "Convert the current region to a path."
  (interactive "r")
  (let ((ranges (scaga-range-to-path-ranges beg end))
        patterns)
    (dolist (range ranges)
      (let ((pattern (get-text-property (car range) 'scaga-pattern)))
        (push pattern patterns)))
    (if patterns
        (setcar patterns (cl-remove-if (lambda (component)
                                         (string-match-p "^\\(FLC:\\|'\\)" component))
                                       (car patterns))))
    (setq patterns (nreverse patterns))
    patterns))

(defun scaga-pattern-to-pattern-string (pattern)
  (mapconcat #'identity pattern " = "))

(defun scaga-path-to-path-string (path)
  (mapconcat #'scaga-pattern-to-pattern-string path " > "))

(defun scaga-region-to-path-string (beg end)
  "Convert the current region to a path."
  (interactive "r")
  (scaga-path-to-path-string (scaga-region-to-path beg end)))

(defun scaga-point-to-path-range (point)
  (let ((beg (previous-single-property-change (point) 'scaga-path))
        (end (next-single-property-change (point) 'scaga-path)))
    (list (cons beg end))))

(defun scaga-v-command (beg end)
  "Verify the selected call path is actually possible."
  (interactive "r")
  (when (use-region-p)
    (scaga-verify (scaga-region-to-path beg end))))

(defun scaga-x-command (beg end)
  "Delete data at point (WIP: or in region.)"
  (interactive "r")
  (if (use-region-p)
      nil
    (let ((q (get-char-property (point) 'scaga-q)))
      (when q
        (setq scaga-queue (delq q scaga-queue))
        (scaga-insert-queue)))))

(defun scaga-fill-rules-hash ()
  (let ((hash scaga-rules-hash))
    (with-current-buffer scaga-rules-buffer
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (cond
           ((looking-at-p "^#")
            t)
           ((looking-at "^\\(.*?\\) := \\(.*?\\) => \\(.*?\\)\\(#.*\\)?$")
            (puthash (match-string 2) (match-string 1) hash))
           ((looking-at "^\\(.*?\\) := \\(.*?\\)\\(#.*\\)?$")
            (puthash (match-string 2) (match-string 1) hash))
           (t
            (user-error "broken rules buffer")))
          (forward-line))))))

(defun scaga-add-rule (kind rule-in-string rule-out-string)
  (let ((hash scaga-rules-hash))
    (with-current-buffer scaga-rules-buffer
      (save-excursion)
      (goto-char (point-max))
      (insert kind " := " rule-in-string)
      (if rule-out-string
          (insert " => " rule-out-string))
      (insert "\n")
      (puthash rule-in-string kind hash)
      (save-buffer))
    (unless (equal kind "lto:unknown")
      (scaga-queue-q (cons "--reread-rules" "refresh rules")))))

(defun scaga-r-command (beg end)
  "Create a new rule covering the selected region."
  (interactive "r")
  (when (or (not (use-region-p))
            (= beg end))
    (let ((ranges (scaga-point-to-path-range (point))))
      (when ranges
        (setq beg (caar ranges)
              end (cdar ranges)))))
  (let ((cleanup (scaga-highlight-region-path beg end)))
    (unwind-protect
        (let* ((path (scaga-region-to-path-string beg end))
               (comment (read-from-minibuffer (concat "Keyword for rule " path ": "))))
          (scaga-add-rule comment path))
      (funcall cleanup))))

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
    (forward-line (1+ type)))
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
         (inhibit-read-only t)
         (lines (1+ (or (and (numberp (cadr excerpt)) (cadr excerpt)) 4))))
    (when flc
      (scaga-delete-excerpt excerpt)
      (scaga-create-excerpt lines))))

(defun scaga-minus-command ()
  "Display less context for defun at point."
  (interactive)
  (let* ((flc (get-text-property (point) 'scaga-flc))
         (excerpt (get-text-property (point) 'scaga-excerpt))
         (inhibit-read-only t)
         (lines (1- (or (and (numberp (cadr excerpt)) (cadr excerpt)) 4))))
    (when flc
      (scaga-delete-excerpt excerpt)
      (scaga-create-excerpt lines))))

(defun scaga-f-command ()
  "Display entire defun at point."
  (interactive)
  (let* ((flc (get-text-property (point) 'scaga-flc))
         (excerpt (get-text-property (point) 'scaga-excerpt))
         (inhibit-read-only t))
    (scaga-delete-excerpt excerpt)
    (when flc
      (unless excerpt
        (scaga-create-excerpt 'defun)))))

(defun scaga-queue-q (q)
  (setq scaga-queue (nconc scaga-queue (list q)))
  (scaga-insert-queue))

(defun scaga-l-command ()
  "Rerun current iteration."
  (interactive)
  (scaga-queue-q (cons "--next=retry"
                       "Repeat iteration")))

(defun scaga-L-command ()
  "Rerun loop from iteration 1."
  (interactive)
  (scaga-queue-q (cons "--next=retry"
                       "Repeat loop")))

(defun scaga-tab-command (arg)
  "Show source code for defun at point or hide it, or skip to the next widget button."
  (interactive "p")
  (if (get-char-property (point) 'button)
      (widget-forward arg)
    (let ((flc (get-text-property (point) 'scaga-flc))
          (excerpt (get-text-property (point) 'scaga-excerpt))
          (inhibit-read-only t))
      (when flc
        (if excerpt
            (scaga-delete-excerpt excerpt)
          (scaga-create-excerpt 5))))))

(defun scaga-ret-command ()
  "Visit the defun at point, or activate a widget button."
  (interactive)
  (if (get-char-property (point) 'button)
      (widget-button-press (point))
    (let ((flc (get-text-property (point) 'scaga-flc))
          (excerpt (get-text-property (point) 'scaga-excerpt)))
      (when flc
        (let* ((filename (car flc))
               (lineno (cadr flc))
               (buffer (find-file filename)))
          (when buffer
            (goto-line lineno)))))))

(defun scaga-set-busy (q)
  (let ((busy (cdr (assq 'busy scaga-markers)))
        (after-busy (cdr (assq 'queue scaga-markers)))
        (inhibit-read-only t))
    (save-excursion
      (goto-char busy)
      (let ((beg (point)))
        (insert "Current operation: ")
        (if (functionp (cdr q))
            (funcall (cdr q))
          (insert (cdr q)))
        (insert "\n")
        (put-text-property beg (point) 'scaga-q q))
      (delete-region (point) after-busy))))

(defun scaga-insert-queue ()
  (let ((queue (cdr (assq 'queue scaga-markers)))
        (after-queue (cdr (assq 'after-queue scaga-markers)))
        (inhibit-read-only t))
    (save-excursion
      (goto-char queue)
      (insert "Queue:")
      (if (null scaga-queue)
          (insert " empty\n\n")
        (insert "\n")
        (dolist (q scaga-queue)
          (let ((beg (point)))
            (if (functionp (cdr q))
                (funcall (cdr q))
              (insert (cdr q) "\n"))
            (put-text-property beg (point) 'scaga-q q))))
      (delete-region (point) after-queue))))

(defun scaga-verify (path)
  (let ((q (cons (concat "lto := " (scaga-path-to-path-string path))
                 `(lambda () (insert "verify\n") (scaga-insert-path ',path '(:background "#ccffff"))))))
    (scaga-queue-q q)))

(defun scaga-path-style (kind)
  (cond
   ((equal kind "baddie")
    `(:background "#ccccff"))
   ((equal kind "lto")
    `(:background "#cccccc"))
   ((equal kind "lto:unknown")
    `(:background "#cccccc"))
   ((equal kind "lto:noreturn")
    `(:background "#ffcccc"))
   ((equal kind "lto:impossible")
    `(:background "#ffccff"))))

;; path is a list of patterns which are lists of components
(defun scaga-insert-path (path &optional style)
  (let ((beg (point))
        lines)
    (dolist (pattern path)
      (let ((line "")
            filename lineno colno new-pattern)
        (dolist (component pattern)
          (cond
           ((string-match "^FLC:\\(.*?\\):\\(.*?\\):\\(.*?\\)$" component)
            (setq filename (concat scaga-source "/" (match-string 1 component)))
            (setq lineno (string-to-number (match-string 2 component)))
            (setq colno (string-to-number (match-string 3 component))))
           ((string-match "^component:" component)
            (push component new-pattern))
           ((string-match "^intype:" component)
            t)
           ((string-match "^/" component)
            t)
           ((string-match "^'" component)
            t)
           ((string-match "0x" component)
            t)
           (t
            (push component new-pattern))))
        (setq line (propertize
                    (mapconcat #'identity new-pattern " = ")
                    'scaga-pattern pattern
                    'scaga-pattern-string (mapconcat #'identity pattern " = ")
                    'scaga-flc `(,filename ,lineno ,colno)
                    'face (or style `(:background "#ccccff"))))
        (push line lines)))
    (dolist (line (nreverse lines))
      (insert line)
      (insert "\n"))
    (put-text-property beg (point) 'scaga-path path)))

(defun scaga-string-to-path (string)
  (let* ((pattern-strings (split-string string " > "))
         patterns)
    (dolist (pattern-string (split-string string " > "))
      (let (pattern)
        (dolist (component (split-string pattern-string " = "))
          (push component pattern))
        (setq pattern (nreverse pattern))
        (push pattern patterns)))
    (setq patterns (nreverse patterns))
    patterns))

(defun scaga-loopback (f)
  (goto-char (cdr (assq 'clearable scaga-markers)))
  (let ((beg (copy-marker (point-marker)))
        beg-path end-path end path)
    (when (looking-at-p "^\n")
      (delete-region (point) (1+ (point)))
      (setq beg (copy-marker (point-marker))))
    (when (looking-at "^\\(.*?\\):$")
      (let ((kind (match-string 1)))
        (forward-line)
        (setq beg-path (copy-marker (point-marker)))
        (re-search-forward "^$" nil 1)
        (forward-line)
        (setq end (copy-marker (point-marker)))
        (forward-line -2)
        (setq end-path (copy-marker (point-marker)))
        (setq path (scaga-region-to-path beg-path end-path))
        (when (equal kind "baddie")
          (funcall f path))
        (delete-region beg end)))))

(defun scaga-filter-path-components (path re)
  (mapcar (lambda (pattern)
            (cl-remove-if (lambda (component)
                            (string-match-p re component))
                          pattern))
          path))

(defun scaga-lto-one (path)
  (catch 'return
    (let ((n (length path)))
      (dotimes (li 3)
        (let ((l (+ li 2)))
          (dotimes (o (- n l))
            (let* ((subpath (scaga-path-fix (subseq path o (+ o l))))
                   (subpath-string (scaga-path-to-path-string subpath)))
              (unless (gethash subpath-string scaga-rules-hash)
                (scaga-verify subpath)
                (throw 'return t)))))))
    nil))

(defun scaga-incoming-rule (kind path-in &optional path-out)
  (goto-char (point-max))
  (insert kind ":\n")
  (scaga-insert-path path-in (scaga-path-style kind))
  (when path-out
    (insert "=>\n")
    (scaga-insert-path path-out (scaga-path-style kind)))
  (cond
   ;;((and scaga-lto-mode (equal kind "baddie"))
   ;; (scaga-verify path-in))
   ((string-match-p "^lto:" kind)
    (scaga-add-rule kind (scaga-path-to-path-string path-in)
                    (if path-out (scaga-path-to-path-string path-out))))))

(defun scaga-incoming-line (line)
  (if (or (string-match "^\\(.*?\\) := \\(.*\\) => \\(.*\\)$" line)
          (string-match "^\\(.*?\\) := \\(.*\\) =>\\(\\)$" line)
          (string-match "^\\(.*?\\) := \\(.*\\)\\(\\)$" line))
      (let* ((kind (match-string 1 line))
             (path-in-string (match-string 2 line))
             (path-out-string (match-string 3 line))
             (path-in (scaga-string-to-path path-in-string))
             (path-out (if (equal path-out-string "") nil (scaga-string-to-path path-out-string))))
        (scaga-incoming-rule kind path-in path-out))))

(defun scaga-update (buffer)
  "Automated update function run when there is new SCAGA process output."
  (when (buffer-live-p (with-current-buffer buffer (process-buffer scaga-process)))
    (with-current-buffer
        (with-current-buffer buffer
          (scaga-process-status)
          (save-excursion
            (let ((inhibit-read-only t))
              (if scaga-lto-mode
                  (scaga-loopback #'scaga-lto-one))))
          (process-buffer scaga-process))
      (save-excursion
        (goto-char 1)
        (while (looking-at-p ".*\n[^!]")
          (re-search-forward "\n")
          (delete-region 1 (point)))
        (while (looking-at-p (concat ".*"
                                     "\n"
                                     "!!!sequence: "
                                     (regexp-quote (number-to-string scaga-sequence))
                                     " \\(more\\|done\\)"
                                     "\n"))
          (save-match-data
            (re-search-forward (concat "^"
                                       "\\(.*?\\)"
                                       "\n"
                                       "!!!sequence: "
                                       (regexp-quote (number-to-string scaga-sequence))
                                       " \\(more\\|done\\)"
                                       "\n")
                               nil t)
            (incf scaga-sequence)
            (let ((line (match-string 1))
                  (more (equal (match-string 2) "more")))
              (with-current-buffer buffer
                (save-excursion
                  (let ((inhibit-read-only t))
                    (scaga-incoming-line line)
                    (if scaga-lto-mode
                        (scaga-loopback #'scaga-lto-one)))))
              (unless more
                (if (and scaga-altlb-mode (= (mod scaga-sequence 2) 0))
                    (scaga-loopback (lambda (path) (scaga-verify path) t)))
                (with-current-buffer buffer
                  (when (process-live-p scaga-process)
                    (let ((q (cons "--next" "automatic operation")))
                      (when scaga-queue
                        (setq q (car scaga-queue))
                        (setq scaga-queue (cdr scaga-queue)))
                      (process-send-string scaga-process (concat (car q) "\n"))
                      (scaga-set-busy q)
                      (scaga-insert-queue))))))
            (with-current-buffer buffer
              (save-excursion
                (goto-char (point-max))
                (let ((inhibit-read-only t))
                  (insert "\n")))))
          (delete-region 1 (point)))))))

(define-derived-mode scaga-mode special-mode "SCAGA"
  "SCAGA mode, see https://github.com/pipcet/scaga"
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
(define-key scaga-mode-map (kbd "v") #'scaga-v-command)
(define-key scaga-mode-map (kbd "V") #'scaga-V-command)
(define-key scaga-mode-map (kbd "x") #'scaga-x-command)

(defun scaga-restart ()
  (interactive)
  (when (process-live-p scaga-process)
    (ignore-errors (kill-process scaga-process)))
  (when (buffer-live-p (process-buffer scaga-process))
    (with-current-buffer (process-buffer scaga-process)
      (delete-region (point-min) (point-max))))
  (apply #'scaga-start (scaga-args)))

(defvar scaga-path nil "Path to SCAGA Perl program")
(defvar scaga-exec nil "Path to executable being analyzed")
(defvar scaga-source nil "Path to source code for analyzed program")
(defvar scaga-rules-dir nil "Path to SCAGA rules files")
(defvar scaga-rules-buffer nil)
(defvar scaga-rules-hash nil)
(defvar scaga-cc nil)
(defvar scaga-lto nil)
(defvar scaga-lto-mode nil)
(defvar scaga-altlb-mode nil)
(defvar scaga-loop-forever nil)
(defvar scaga-use-nytprof nil)
(defvar scaga-ignore-noreturn nil)
(defvar scaga-markers nil)
(defvar scaga-queue nil)
(defvar scaga-region-cleanup nil)

(defvar scaga-sequence nil "Sequence number of next expected SCAGA reply.")

(defun scaga-args ()
  (let (args)
    (push "perl" args)
    (if scaga-use-nytprof (push "-d:NYTProf" args))
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
      (if scaga-timer
          (cancel-timer scaga-timer))
      (scaga-mode)
      (set (make-local-variable 'scaga-path) "/home/pip/git/scaga/scaga-extend.pl")
      (set (make-local-variable 'scaga-exec) "/usr/local/bin/emacs")
      (set (make-local-variable 'scaga-source) "/home/pip/git/emacs/src")
      (set (make-local-variable 'scaga-rules-dir) "/home/pip/git/scaga")
      (set (make-local-variable 'scaga-cc) "gcc -Demacs -I/home/pip/git/emacs/src -I/home/pip/git/emacs/lib $(pkg-config --cflags gtk+-3.0) -fno-inline-functions -fno-inline-functions-called-once -fno-inline-small-functions -fno-optimize-sibling-calls -ffunction-sections -fno-function-cse -flto -fdevirtualize-speculatively -fdevirtualize-at-ltrans -ggdb3 -O3 -c")
      (set (make-local-variable 'scaga-lto) "/usr/lib/gcc/x86_64-linux-gnu/5.2.1/lto1 -S -ggdb3 -O3 -fno-inline-functions -fno-inline-functions-called-once -fno-inline-small-functions -ffunction-sections -flto -fno-function-cse -flto -fdevirtualize-speculatively -fdevirtualize-at-ltrans -flto -mtune=generic -march=x86-64 -mtune=generic -march=x86-64 -O3 -version -fmath-errno -fsigned-zeros -ftrapping-math -fno-trapv -fno-strict-overflow -fno-openmp -fno-openacc -fno-function-cse -fdevirtualize-speculatively -fdevirtualize-at-ltrans")
      (set (make-local-variable 'scaga-rules-dir) "/home/pip/git/scaga")
      (set (make-local-variable 'scaga-ignore-noreturn) t)
      (set (make-local-variable 'scaga-lto-mode) nil)
      (set (make-local-variable 'scaga-altlb-mode) nil)
      (set (make-local-variable 'scaga-loop-forever) t)
      (set (make-local-variable 'scaga-use-nytprof) nil)
      (set (make-local-variable 'scaga-rules) nil)
      (set (make-local-variable 'scaga-calls) nil)
      (set (make-local-variable 'scaga-markers) nil)
      (set (make-local-variable 'scaga-region-cleanup) nil)
      (setq-local redisplay-highlight-region-function #'scaga-highlight-region)
      (setq-local redisplay-unhighlight-region-function #'scaga-unhighlight-region)
      (if scaga-timer
          (cancel-timer scaga-timer))
      (make-local-variable 'scaga-timer)
      (if scaga-timer
          (cancel-timer scaga-timer))
      (setq scaga-timer (run-with-timer 0 .5 #'scaga-update (current-buffer)))
      (set (make-local-variable 'scaga-args) args)
      (scaga-widgets))
    (pop-to-buffer buffer)))

(require 'wid-edit)
(require 'widget)

(defvar scaga-widgets nil)
(defun scaga-widgets ()
  (let ((beg (point))
        (inhibit-read-only t))
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
    (push (cons 'scaga-use-nytprof
                (widget-create 'checkbox
                               scaga-use-nytprof))
          scaga-widgets)
    (widget-insert " use NYTProf\n")
    (push (cons nil
                (widget-create 'push-button
                               :notify (lambda (&rest ignore)
                                         (dolist (pair scaga-widgets)
                                           (when (car pair)
                                             (set (car pair) (widget-value (cdr pair)))))
                                         (scaga-restart))
                               "Start"))
          scaga-widgets)
    (widget-insert " ")
    (push (cons nil
                (widget-create 'push-button
                               :notify (lambda (&rest ignore)
                                         (when (process-live-p scaga-process)
                                           (ignore-errors
                                             (kill-process scaga-process))))
                               "Kill"))
          scaga-widgets)
    (widget-insert " ")
    (push (cons nil
                (widget-create 'push-button
                               :notify (lambda (&rest ignore)
                                         (when (process-live-p scaga-process)
                                           (if (eq (process-status scaga-process)
                                                   'stop)
                                               (continue-process scaga-process)
                                             (signal-process scaga-process 'SIGSTOP))
                                           (scaga-process-status)))
                               "Stop"))
          scaga-widgets)
    (widget-insert " ")
    (push (cons nil
                (widget-create 'push-button
                               :notify (lambda (&rest ignore)
                                         (let ((beg (cdr (assq 'clearable scaga-markers)))
                                               (end (point-max))
                                               (inhibit-read-only t))
                                           (when (and beg end)
                                             (delete-region beg end))))
                               "Clear"))
          scaga-widgets)
    (widget-insert "\n")
    (push (cons 'scaga-lto-mode
                (widget-create 'checkbox
                               :notify (lambda (widget &rest ignore)
                                         (let ((pair (rassq widget scaga-widgets)))
                                           (set (car pair) (widget-value (cdr pair)))))
                               scaga-lto-mode))
          scaga-widgets)
    (widget-insert " LTO mode")
    (widget-insert "\n")
    (push (cons 'scaga-altlb-mode
                (widget-create 'checkbox
                               :notify (lambda (widget &rest ignore)
                                         (let ((pair (rassq widget scaga-widgets)))
                                           (set (car pair) (widget-value (cdr pair)))))
                               scaga-altlb-mode))
          scaga-widgets)
    (widget-insert " alternating loopback mode")
    (widget-insert "\n")
    (widget-setup)
    (push (cons 'process-status (copy-marker (point-marker) nil)) scaga-markers)
    (insert "Process status: not started\n")
    (push (cons 'after-process-status (copy-marker (point-marker) nil)) scaga-markers)
    (push (cons 'busy (copy-marker (point-marker) nil)) scaga-markers)
    (insert "Current operation: none\n")
    (push (cons 'queue (copy-marker (point-marker) nil)) scaga-markers)
    (insert "Queue: empty\n")
    (push (cons 'after-queue (copy-marker (point-marker) nil)) scaga-markers)
    (push (cons 'clearable (copy-marker (point-marker) nil)) scaga-markers)))

(provide 'scaga-mode)
