(defvar scaga-process nil)
(defvar scaga-marker nil)
(defvar scaga-out-buffer (find-file-noselect "/home/pip/git/scaga/scaga.out"))
(defvar scaga-timer nil)
(defvar scaga-rules-buffer (find-file-noselect "/home/pip/git/scaga/rules-from-elisp.scaga"))
(defvar scaga-next nil)

(defun scaga-start (&rest args)
  (setq scaga-process (apply #'start-process "SCAGA" "*scaga*" args))
  (with-current-buffer "*scaga*"
    (setq scaga-marker (copy-marker (point) nil)))
  (setq scaga-out-buffer (get-buffer-create "scaga.out")))

(defun scaga-insert-excerpt (file around-line)
  (when nil
  (let ((excerpt
         (with-temp-buffer
           (insert-file-contents file)
           (goto-line (- around-line 10))
           (buffer-substring (point) (save-excursion (forward-line 21) (point))))))
    (insert (propertize excerpt 'face '(:background "#ffffcc"))))))

(defun scaga-r-command (beg end)
  (interactive "r")
  (when (or (not (use-region-p))
            (= beg end))
    (setq beg (save-excursion (beginning-of-line) (point))
          end (save-excursion (end-of-line) (point))))
  (let ((comment (read-from-minibuffer "Comment: "))
        (path (scaga-region-to-path beg end)))
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
    (re-search-backward "^[^ \t\n]" nil t)))
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
  (interactive)
  (let* ((flc (get-text-property (point) 'scaga-flc))
         (excerpt (get-text-property (point) 'scaga-excerpt))
         (lines (1+ (or (and (numberp (cadr excerpt)) (cadr excerpt)) 4))))
    (when flc
      (scaga-delete-excerpt excerpt)
      (scaga-create-excerpt lines))))

(defun scaga-minus-command ()
  (interactive)
  (let* ((flc (get-text-property (point) 'scaga-flc))
         (excerpt (get-text-property (point) 'scaga-excerpt))
         (lines (1- (or (and (numberp (cadr excerpt)) (cadr excerpt)) 4))))
    (when flc
      (scaga-delete-excerpt excerpt)
      (scaga-create-excerpt lines))))

(defun scaga-f-command ()
  (interactive)
  (let* ((flc (get-text-property (point) 'scaga-flc))
         (excerpt (get-text-property (point) 'scaga-excerpt))
         (lines (1+ (or (and (numberp (cadr excerpt)) (cadr excerpt)) 4))))
    (when flc
      (scaga-delete-excerpt excerpt)
      (scaga-create-excerpt 'defun))))

(defun scaga-l-command ()
  (interactive)
  (setq scaga-next "--next=retry"))

(defun scaga-L-command ()
  (interactive)
  (setq scaga-next "--next=rules-loop"))

(defun scaga-tab-command ()
  (interactive)
  (let ((flc (get-text-property (point) 'scaga-flc))
        (excerpt (get-text-property (point) 'scaga-excerpt)))
    (when flc
      (if excerpt
          (scaga-delete-excerpt excerpt)
        (scaga-create-excerpt 5)))))

(defun scaga-ret-command ()
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

(defun scaga-update ()
  (set-marker scaga-marker 1 (marker-buffer scaga-marker))
  (with-current-buffer (marker-buffer scaga-marker)
    (goto-char scaga-marker)
    (while (looking-at-p ".*\n")
      (save-match-data
        (re-search-forward "\\(.*?\\)\n" nil t)
        (let* ((line (match-string 1))
               (patterns (split-string line " > "))
               after-line new-components lines
               filename lineno colno)
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
            (with-current-buffer scaga-out-buffer
              (save-excursion
                (goto-char (point-max))
                (dolist (line lines)
                  (insert line)
                  (insert "\n"))
                (setq lines nil)
                (dolist (f after-line)
                  (funcall f)))))
          (when (> (length patterns) 1)
            (when (process-live-p scaga-process)
              (process-send-string scaga-process (concat (or scaga-next "--next") "\n")))
            (setq scaga-next nil))
          (with-current-buffer scaga-out-buffer
            (save-excursion
              (goto-char (point-max))
              (insert "\n")))
          (delete-region scaga-marker (point)))))))

(defvar scaga-mode-map)

(define-derived-mode scaga-mode special-mode "SCAGA"
  "SCAGA mode, see https://github.com/pipcet/scaga")


(define-key scaga-mode-map (kbd "TAB") #'scaga-tab-command)
(define-key scaga-mode-map (kbd "RET") #'scaga-ret-command)
(define-key scaga-mode-map (kbd "+") #'scaga-plus-command)
(define-key scaga-mode-map (kbd "-") #'scaga-minus-command)
(define-key scaga-mode-map (kbd "f") #'scaga-f-command)
(define-key scaga-mode-map (kbd "r") #'scaga-r-command)
(define-key scaga-mode-map (kbd "R") #'scaga-restart)
(define-key scaga-mode-map (kbd "l") #'scaga-l-command)
(define-key scaga-mode-map (kbd "L") #'scaga-L-command)

(provide 'scaga-mode)

(defun scaga-restart ()
  (interactive)
  (ignore-errors (kill-process scaga-process))
  (with-current-buffer scaga-out-buffer
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max)))
  (with-current-buffer "*scaga*"
    (delete-region (point-min) (point-max)))
  (set-marker scaga-marker 1 (marker-buffer scaga-marker))
  (scaga-start "perl" "/home/pip/git/scaga/scaga-extend.pl" "--calls=/home/pip/git/scaga/emacs-calls.scaga" "--rules=/home/pip/git/scaga/rules-from-elisp.scaga" "--badrules=/home/pip/git/scaga/badrules.scaga" "--last=1" "--wait-for-next=1" "--loop-rules=-1" "/home/pip/git/scaga/call.scaga"))

(with-current-buffer scaga-out-buffer
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max)))
(with-current-buffer "*scaga*"
  (delete-region (point-min) (point-max)))


(run-with-timer 0 1 #'scaga-update)


