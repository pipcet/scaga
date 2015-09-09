(defvar scaga-timer nil)
(defvar scaga-next nil)

(defun scaga-start (&rest args)
  (let ((pbuffer (generate-new-buffer "*scaga*"))
        (buffer (current-buffer)))
    (with-current-buffer pbuffer
      (set (make-local-variable 'scaga-buffer) buffer))
    (setq scaga-process (apply #'start-process "SCAGA" pbuffer args))))

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
        (delete-region (caar excerpt) (1+ (cdar excerpt)))
        (remove-text-properties (or (previous-single-property-change (point) 'scaga-excerpt)
                                    (point-min))
                                (or (next-single-property-change (point) 'scaga-excerpt)
                                    (point-max))
                                '(scaga-excerpt nil))))

(defun scaga-excerpt-start (type)
  (cond
   ((numberp type)
    (forward-line (- type)))
   ((eq type 'defun)
    (re-search-backward "^[^ \t\n]" nil t)
    (while (looking-at-p "^{")
      (re-search-backward "^[^ \t\n]" nil t))
    (re-search-backward "^[ \t\n]" nil t)
    (forward-line)))
  (point))

(defun scaga-excerpt-end (type)
  (cond
   ((numberp type)
    (forward-line type))
   ((eq type 'defun)
    (re-search-forward "^[^ \t\n]" nil t)))
  (point))

(defun scaga-create-excerpt (type)
  (let ((filename (car flc))
        (lineno (cadr flc)))
    (save-excursion
      (beginning-of-line)
      (setq m9 (copy-marker (point-marker)))
      (end-of-line)
      (insert "\n")
      (setq m0 (copy-marker (point-marker)))
      (let ((excerpt (with-temp-buffer
                       (insert-file-contents filename)
                       (goto-line lineno)
                       (buffer-substring (save-excursion (scaga-excerpt-start type))
                                         (save-excursion (scaga-excerpt-end type))))))
        (insert (propertize excerpt 'face '(:background "#ffffcc"))))
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
         (excerpt (get-text-property (point) 'scaga-excerpt))
         (lines (1+ (or (and (numberp (cadr excerpt)) (cadr excerpt)) 4))))
    (when flc
      (scaga-delete-excerpt excerpt)
      (scaga-create-excerpt 'defun))))

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
  (with-current-buffer
      (with-current-buffer buffer (process-buffer scaga-process))
    (goto-char 1)
    (while (looking-at-p ".*\n")
      (save-match-data
        (re-search-forward "\\(.*?\\)\n" nil t)
        (let* ((line (match-string 1))
               (patterns (split-string line " > "))
               after-line new-components lines
               filename lineno colno)
          (if (= (length patterns) 1)
              (with-current-buffer buffer
                (save-excursion
                  (goto-char (point-max))
                  (insert (propertize line
                                      'face `(:background "#888888")))
                  (insert "\n")))
            (dolist (pattern patterns)
              (setq new-components nil)
              (dolist (component (split-string pattern " = "))
                (cond
                 ((string-match "^FLC:\\(.*?\\):\\(.*?\\):\\(.*?\\)$" component)
                  (setq filename (concat "/home/pip/git/emacs/src/" (match-string 1 component)))
                  (setq lineno (string-to-number (match-string 2 component)))
                  (setq colno (string-to-number (match-string 3 component)))
                  (push `(lambda ()
                           (scaga-insert-excerpt (concat "/home/pip/git/emacs/src/" ,(match-string 1 component))
                                                 (string-to-number ,(match-string 2 component))))
                        after-line))
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
                  (dolist (line lines)
                    (insert line)
                    (insert "\n"))
                  (setq lines nil)
                  (dolist (f after-line)
                    (funcall f)))))
            (when (process-live-p scaga-process)
              (process-send-string scaga-process (concat (or scaga-next "--next") "\n")))
            (setq scaga-next nil)
            (with-current-buffer buffer
              (save-excursion
                (goto-char (point-max))
                (insert "\n"))))
          (delete-region 1 (point)))))))

(define-derived-mode scaga-mode special-mode "SCAGA"
  "SCAGA mode, see https://github.com/pipcet/scaga"
  (set (make-local-variable 'scaga-timer) (run-with-timer 0 1 #'scaga-update (current-buffer)))
  (setq buffer-read-only nil))

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
  (ignore-errors (kill-process scaga-process))
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))
  (with-current-buffer (process-buffer scaga-process)
    (delete-region (point-min) (point-max)))
  (apply #'scaga-start scaga-args))

(defvar scaga-path nil "Path to SCAGA Perl program")
(defvar scaga-exec nil "Path to executable being analyzed")
(defvar scaga-source nil "Path to source code for analyzed program")
(defvar scaga-rules-dir nil "Path to SCAGA rules files")

(defvar scaga-args nil "arguments to run SCAGA with")

(defun scaga ()
  (interactive)
  (let ((buffer (get-buffer-create "*SCAGA*"))
        args)
    (with-current-buffer buffer
      (scaga-mode)
      (set (make-local-variable 'scaga-path) (expand-file-name (read-file-name "path to scaga-extend.pl: ")))
      (set (make-local-variable 'scaga-exec) (expand-file-name (read-file-name "path to executable: ")))
      (set (make-local-variable 'scaga-source) (expand-file-name (read-directory-name "path to source code: ")))
      (set (make-local-variable 'scaga-rules-dir) (expand-file-name (read-directory-name "path to rules.scaga, badrules.scaga, calls.scaga, and call.scaga: ")))
      (set (make-local-variable 'scaga-rules) (list (concat scaga-rules-dir "/rules.scaga")))
      (set (make-local-variable 'scaga-calls) (list (concat scaga-rules-dir "/calls.scaga")))
      (set (make-local-variable 'scaga-badrules) (list (concat scaga-rules-dir "/badrules.scaga")))
      (set (make-local-variable 'scaga-call) (list (concat scaga-rules-dir "/call.scaga")))
      (set (make-local-variable 'scaga-rules-buffer) (find-file-noselect (car scaga-rules)))
      (push "perl" args)
      (push scaga-path args)
      (push (concat "--executable=" scaga-exec) args)
      (dolist (scaga-rules-file scaga-rules)
        (push (concat "--rules=" scaga-rules-file) args))
      (dolist (scaga-badrules-file scaga-badrules)
        (push (concat "--badrules=" scaga-badrules-file) args))
      (dolist (scaga-calls-file scaga-calls)
        (push (concat "--calls=" scaga-calls-file) args))
      (dolist (scaga-call-file scaga-call)
        (push scaga-call-file args))
      (push "--last=1" args)
      (push "--wait-for-next=1" args)
      (push "--loop-rules=-1" args)
      (setq args (nreverse args))
      (apply #'scaga-start args)
      (set (make-local-variable 'scaga-args) args))))

(provide 'scaga-mode)

