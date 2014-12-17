;;; racer.el --- Rust completion via racer

;;; Commentary:

;;; Code:

(defgroup racer nil
  "Support for Rust completion via racer."
  :link '(url-link "https://github.com/phildawes/racer/")
  :group 'rust-mode)

(defcustom racer-cmd "/usr/local/bin/racer"
  "Path to the racer binary."
  :type 'file
  :group 'racer)

(defcustom racer-rust-src-path "/usr/local/src/rust/src"
  "Path to the rust source tree."
  :type 'file
  :group 'racer)

(defvar racer-file-name)
(defvar racer-tmp-file-name)
(defvar racer-line-number)
(defvar racer-column-number)
(defvar racer-completion-results)
(defvar racer-start-pos)
(defvar racer-end-pos)

(require 'company)

(defun racer-get-line-number () 
  ; for some reason if the current-column is 0, then the linenumber is off by 1
  (if (= (current-column) 0) 
      (1+ (count-lines 1 (point))) 
    (count-lines 1 (point))))

(defun racer--write-tmp-file (tmp-file-name)
  "Write the racer temporary file to `TMP-FILE-NAME'."
    (push-mark)
    (setq racer-file-name (buffer-file-name))
    (setq racer-tmp-file-name tmp-file-name)
    (setq racer-line-number (racer-get-line-number))
    (setq racer-column-number (current-column))
    (setq racer-completion-results `())
    (write-region nil nil tmp-file-name))

(defun racer--candidates ()
  "Run the racer complete command and process the results."
  (setq racer-tmp-file-name (concat (buffer-file-name) ".racertmp"))
  (racer--write-tmp-file racer-tmp-file-name)
  (setenv "RUST_SRC_PATH" racer-rust-src-path)
    (let ((lines (process-lines racer-cmd
				"complete"
				(number-to-string racer-line-number)
				(number-to-string racer-column-number)
				racer-tmp-file-name)))
      (delete-file racer-tmp-file-name)
      (dolist (line lines)
	(when (string-match "^MATCH \\([^,]+\\),\\([^,]+\\),\\([^,]+\\),\\([^,]+\\),\\([^,]+\\),\\(.+\\)$" line)
	  (let ((completion (match-string 1 line))
		(linenum (match-string 2 line))
		(colnum (match-string 3 line))
		(fname (match-string 4 line))
		(matchtype (match-string 5 line))
		(contextstr (match-string 6 line)))
	    (put-text-property 0 1 'contextstr contextstr completion)
	    (put-text-property 0 1 'matchtype matchtype completion)
	    (push completion racer-completion-results))))
      racer-completion-results))

(defun racer--prefix ()
  "Run the racer prefix command and process the results."
  (setq racer-tmp-file-name (concat (buffer-file-name) ".racertmp"))
  (racer--write-tmp-file racer-tmp-file-name)
  (setenv "RUST_SRC_PATH" racer-rust-src-path)
  (let ((lines (process-lines racer-cmd
			      "prefix"
			      (number-to-string racer-line-number)
			      (number-to-string racer-column-number)
			      racer-tmp-file-name)))
    (delete-file racer-tmp-file-name)
    (when (string-match "^PREFIX \\(.+\\),\\(.+\\),\\(.*\\)$" (nth 0 lines))
      (match-string 3 (nth 0 lines)))))

(defun racer--complete-at-point-fn ()
  "Run the racer complete command and process the results."
  (setq racer-tmp-file-name (concat (buffer-file-name) ".racertmp"))
  (racer--write-tmp-file racer-tmp-file-name)
  (setenv "RUST_SRC_PATH" racer-rust-src-path)
  (save-excursion
    (let ((lines (process-lines racer-cmd
				"complete"
				(number-to-string racer-line-number)
				(number-to-string racer-column-number)
				racer-tmp-file-name)))
      (delete-file racer-tmp-file-name)
      (dolist (line lines)
	(when (string-match "^MATCH \\([^,]+\\),\\(.+\\)$" line)
	  (let ((completion (match-string 1 line)))
	    (push completion racer-completion-results)))
	
	(when (string-match "^PREFIX \\(.+\\),\\(.+\\),\\(.*\\)$" line)
	  (setq racer-start-pos (string-to-number (match-string 1 line)))
	  (setq racer-end-pos (string-to-number (match-string 2 line))))
	)
      )
    )
  ;(message "start %s end %s" start end)
  (list (- (point) (- racer-end-pos racer-start-pos))
        (point)
        racer-completion-results)
  )

(defun racer-company-complete (command &optional arg &rest ignored)
  "Run the racer command for `COMMAND' and format using `ARG'.
`IGNORED' is unused."
  (interactive)
  ;(message "PHIL racer-company-complete %s %s %s" command arg ignored)
  (when (looking-back "[a-zA-z1-9:.]")
    (cl-case command
      (prefix (racer--prefix))
      (candidates (racer--candidates))
      (duplicates t)
      (sorted nil)
      (annotation
       (progn
	 (format "%s %10s : %s"
		 (make-string (max 0 (- 20 (length arg))) ? )
		 (get-text-property 0 'matchtype arg)
		 (get-text-property 0 'contextstr arg))))
      (meta (format "%s" (get-text-property 0 'contextstr arg)))
;      (doc-buffer (company-doc-buffer "PHil hello Yeah"))
      )))

;; (defun racer-complete (command &optional arg &rest ignored)
;;   (interactive)
;;   (message "PHIL racer-company-complete %s %s %s" command arg ignored)
;;   (when (looking-back "[a-zA-z1-9:.]")
;;     (case command
;;       (prefix (racer--prefix))
;;       (candidates (racer--candidates))
;;       (duplicates t)
;;       (sorted nil)
;;       (annotation (format ":%s yeah mother" arg))
;;       (meta (format "This value is named %s" arg))
;; ;      (doc-buffer (company-doc-buffer "PHil hello Yeah"))
;;       )))


;; (add-hook 'completion-at-point-functions 'racer-complete nil)

(defun racer--complete-or-indent ()
  "Complete with company-mode or indent."
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
    (indent-according-to-mode)))

(defun string/ends-with (s ending)
  "Return non-nil if string S ends with ENDING."
  (cond ((>= (length s) (length ending))
         (let ((elength (length ending)))
           (string= (substring s (- 0 elength)) ending)))
        (t nil)))

(defun racer-find-definition ()
  "Run the racer find-definition command and process the results."
  (interactive)
  (setq racer-tmp-file-name (concat (buffer-file-name) ".racertmp"))
  (racer--write-tmp-file racer-tmp-file-name)
  (setenv "RUST_SRC_PATH" racer-rust-src-path)
  (push-mark)
  (let ((lines (process-lines racer-cmd
			      "find-definition"
			      (number-to-string racer-line-number)
			      (number-to-string racer-column-number)
			      racer-tmp-file-name)))
    (delete-file racer-tmp-file-name)
    (dolist (line lines)
      (when (string-match "^MATCH \\([^,]+\\),\\([^,]+\\),\\([^,]+\\),\\([^,]+\\).*$" line)
	(let ((linenum (match-string 2 line))
	      (charnum (match-string 3 line))
	      (fname (match-string 4 line)))
	  (if (string/ends-with fname ".racertmp")
	      (find-file (substring fname 0 -9))
	    (find-file fname))
	  (goto-char (point-min))
	  (forward-line (1- (string-to-number linenum)))
	  (forward-char (string-to-number charnum))
	  )))))

(add-hook 'rust-mode-hook
	  '(lambda ()
	     (company-mode)
	     (set (make-local-variable 'company-backends) '(racer-company-complete))
	     (local-set-key "\t" 'racer--complete-or-indent)
	     (local-set-key "\M-." 'racer-find-definition)
	     (setq-local company-idle-delay nil)
	     )
	  nil)

(provide 'racer)
;;; racer.el ends here
