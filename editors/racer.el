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

(defcustom racer-rust-src-path
  (if (getenv "RUST_SRC_PATH")
      (getenv "RUST_SRC_PATH")
    "/usr/local/src/rust/src")
  "Path to the rust source tree."
  :type 'file
  :group 'racer)

(defcustom racer-begin-after-member-access t
  "When non-nil, begins completion automatically when the cursor is preceded
by :: or ."
  :type 'boolean
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
    (write-region nil nil tmp-file-name nil -1))

(defun racer--candidates ()
  "Run the racer complete command and process the results."
  (setq racer-tmp-file-name (concat (buffer-file-name) ".racertmp"))
  (racer--write-tmp-file racer-tmp-file-name)
  (setenv "RUST_SRC_PATH" (expand-file-name racer-rust-src-path))
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
          (put-text-property 0 1 'fname fname completion)
          (put-text-property 0 1 'linenum linenum completion)
          (push completion racer-completion-results))))
    racer-completion-results))

(defun racer--company-prefix ()
  "Returns the symbol to complete. If racer-begin-after-member-access is t,
begins completion automatically if the cursor is followed by a dot or ::."
  (if racer-begin-after-member-access
    (company-grab-symbol-cons "\\.\\|::" 2)
    (company-grab-symbol)))

(defun racer--company-annotation (arg)
  "Gets and formats annotation data from the arg match."
  (let ((meta (get-text-property 0 'contextstr arg)))
    (format "%10s : %s"
            (or (save-match-data
                  (and (string-match "\\(.+\\) {" meta)
                       (match-string 1 meta)))
                meta)
            (get-text-property 0 'matchtype arg))))

(defun racer--company-location (arg)
  (let ((fname (get-text-property 0 'fname arg))
        (linenum (get-text-property 0 'linenum arg)))
    (cons fname (string-to-number linenum))))


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
  (setenv "RUST_SRC_PATH" (expand-file-name racer-rust-src-path))
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

;;;###autoload
(defun racer-company-complete (command &optional arg &rest ignored)
  "Run the racer command for `COMMAND' and format using `ARG'.
`IGNORED' is unused."
  (interactive)
  (cl-case command
    (interactive (company-begin-backend 'racer-company-complete))
    (prefix (and (derived-mode-p 'rust-mode)
                 (not (company-in-string-or-comment))
                 (or (racer--company-prefix) 'stop)))
    (candidates (racer--candidates))
    (duplicates t)
    (location (racer--company-location arg))
    (sorted nil)
    (meta (format "%s" (get-text-property 0 'contextstr arg)))
    (annotation (racer--company-annotation arg))))

(provide 'racer)
;;; racer.el ends here
