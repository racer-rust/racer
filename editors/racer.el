(defvar racer-cmd "/home/pld/src/rust/racer/bin/racer")
(defvar rust-srcpath "/usr/local/src/rust/src")

; rust-mode no longer requires cl, so am putting it here for now (this file uses 'case')
(require 'cl)

(defun racer--write-tmp-file (tmpfilename) 
    (push-mark)
    (setq filename (buffer-file-name))
    (setq linenum (count-lines 1 (point)))
    (setq col (current-column))
    (setq completion-results `())
    (write-region nil nil tmpfilename))

(defun racer--candidates () 
  (setq tmpfilename (concat (buffer-file-name) ".racertmp"))
  (racer--write-tmp-file tmpfilename)
  (setenv "RUST_SRC_PATH" rust-srcpath)
    (let ((lines (process-lines racer-cmd 
				"complete"
				(number-to-string linenum)
				(number-to-string col) 
				tmpfilename)))
      (delete-file tmpfilename)
      (dolist (line lines)
	(when (string-match "^MATCH \\([^,]+\\),\\(.+\\)$" line)
	  (let ((completion (match-string 1 line)))
	    (push completion completion-results))))
      completion-results))

(defun racer--prefix () 
  (setq tmpfilename (concat (buffer-file-name) ".racertmp"))
  (racer--write-tmp-file tmpfilename)
  (setenv "RUST_SRC_PATH" rust-srcpath)
  (let ((lines (process-lines racer-cmd 
			      "prefix"
			      (number-to-string linenum)
			      (number-to-string col) 
			      tmpfilename)))
    (delete-file tmpfilename)
    (when (string-match "^PREFIX \\(.+\\),\\(.+\\),\\(.*\\)$" (nth 0 lines))
      (match-string 3 (nth 0 lines)))))


(defun racer--complete-at-point-fn ()
  (setq tmpfilename (concat (buffer-file-name) ".racertmp"))
  (racer--write-tmp-file tmpfilename)
  (setenv "RUST_SRC_PATH" rust-srcpath)
  (save-excursion
    (let ((lines (process-lines racer-cmd 
				"complete"
				(number-to-string linenum)
				(number-to-string col) 
				tmpfilename)))
      (delete-file tmpfilename)
      (dolist (line lines)
	(when (string-match "^MATCH \\([^,]+\\),\\(.+\\)$" line)
	  (let ((completion (match-string 1 line)))
	    (push completion completion-results)))
	
	(when (string-match "^PREFIX \\(.+\\),\\(.+\\),\\(.*\\)$" line)
	  (setq start (string-to-number (match-string 1 line)))
	  (setq end (string-to-number (match-string 2 line))))
	)
      )
    )
  ;(message "start %s end %s" start end)
  (list (- (point) (- end start)) (point) completion-results)
  )

(defun racer-company-complete (command &optional arg &rest ignored)
  (interactive)
  ;(message "PHIL racer-company-complete %s %s %s" command arg ignored)
  (when (looking-back "[a-zA-z1-9:.]")
    (case command
      (prefix (racer--prefix))
      (candidates (racer--candidates))
      )))

(add-hook 'completion-at-point-functions 'racer-complete nil)

(defun racer--complete-or-indent ()
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
    (indent-according-to-mode)))

(defun string/ends-with (s ending)
      "return non-nil if string S ends with ENDING."
      (cond ((>= (length s) (length ending))
             (let ((elength (length ending)))
               (string= (substring s (- 0 elength)) ending)))
            (t nil)))

(defun racer-find-definition ()
  (interactive)
  (setq tmpfilename (concat (buffer-file-name) ".racertmp"))
  (racer--write-tmp-file tmpfilename)
  (setenv "RUST_SRC_PATH" rust-srcpath)
  (push-mark)
  (let ((lines (process-lines racer-cmd 
			      "find-definition"
			      (number-to-string linenum)
			      (number-to-string col) 
			      tmpfilename)))
    (delete-file tmpfilename)
    (dolist (line lines)
      (when (string-match "^MATCH \\([^,]+\\),\\([^,]+\\),\\([^,]+\\),\\([^,]+\\).*$" line)
	(let ((linenum (match-string 2 line))
	      (charnum (match-string 3 line))
	      (fname (match-string 4 line)))
	  (if (string/ends-with fname ".racertmp")
	      (find-file (substring fname 0 -9))
	    (find-file fname))
	  (goto-line (string-to-number linenum))
	  (forward-char (string-to-number charnum))
	  )))))

(add-hook 'rust-mode-hook
	  '(lambda ()
	     (company-mode)
	     (set (make-local-variable 'company-backends) '(racer-company-complete))
	     (local-set-key "\t" 'racer--complete-or-indent)
	     (local-set-key "\M-." 'racer-find-definition)
	     (setq company-idle-delay nil)) 
	  nil)


(provide 'racer)

