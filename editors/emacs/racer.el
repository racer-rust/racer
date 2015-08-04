;;; racer.el --- Rust completion via racer with company

;; Copyright (c) 2014 Phil Dawes

;; Author: Phil Dawes
;; URL: https://github.com/phildawes/racer
;; Version: 0.0.2
;; Package-Requires: ((emacs "24.3") (company "0.8.0") (rust-mode "0.2.0"))
;; Keywords: abbrev, convenience, matching, rust, tools

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any
;; person obtaining a copy of this software and associated
;; documentation files (the "Software"), to deal in the
;; Software without restriction, including without
;; limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software
;; is furnished to do so, subject to the following
;; conditions:

;; The above copyright notice and this permission notice
;; shall be included in all copies or substantial portions
;; of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
;; ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
;; TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
;; SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
;; IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; You will need to configure Emacs to find racer:
;;
;; (setq racer-rust-src-path "<path-to-rust-srcdir>/src/")
;; (setq racer-cmd "<path-to-racer>/target/release/racer")
;;
;; To set up racer for completion in Rust buffers, add it
;; `rust-mode-hook':
;;
;; (add-hook 'rust-mode-hook #'racer-activate)
;;
;; To use TAB for both indent and completion in Rust:
;;
;; (require 'rust-mode)
;; (define-key rust-mode-map (kbd "TAB") #'racer-complete-or-indent)
;;
;; You can also use racer to find definitions. To bind this to a key:
;;
;; (require 'rust-mode)
;; (define-key rust-mode-map (kbd "M-.") #'racer-find-definition)

;;; Code:

(require 'cl-lib)
(require 'etags)
(require 'company)
(require 'rust-mode)

(defgroup racer nil
  "Support for Rust completion via racer."
  :link '(url-link "https://github.com/phildawes/racer/")
  :group 'rust-mode)

(defcustom racer-cmd
  (if (locate-file "racer" exec-path)
      (locate-file "racer" exec-path)
    "/usr/local/bin/racer")
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

(defun racer-get-line-number ()
  "Gets the current line number at point."
  ; for some reason if the current-column is 0, then the linenumber is off by 1
  (if (= (current-column) 0)
      (1+ (count-lines 1 (point)))
    (count-lines 1 (point))))

(defun racer--write-tmp-file (tmp-file-name)
  "Write the racer temporary file to `TMP-FILE-NAME'."
    (push-mark)
    (write-region nil nil tmp-file-name nil 'silent))

(defun racer--candidates ()
  "Run the racer complete command and process the results."
  (let ((racer-tmp-file-name (make-temp-file "racer")))
    (racer--write-tmp-file racer-tmp-file-name)
    (setenv "RUST_SRC_PATH" (expand-file-name racer-rust-src-path))
    (let ((lines (process-lines racer-cmd
                                "complete"
                                (number-to-string (racer-get-line-number))
                                (number-to-string (current-column))
				(buffer-file-name)
                                racer-tmp-file-name))
          (racer-completion-results '()))
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
      racer-completion-results)))

(defun racer--prefix ()
  "Run the racer prefix command and process the results."
  (let ((racer-tmp-file-name (make-temp-file "racer")))
    (racer--write-tmp-file racer-tmp-file-name)
    (setenv "RUST_SRC_PATH" (expand-file-name racer-rust-src-path))
    (let ((lines (process-lines racer-cmd
                                "prefix"
                                (number-to-string (racer-get-line-number))
                                (number-to-string (current-column))
                                racer-tmp-file-name)))
      (delete-file racer-tmp-file-name)
      (when (string-match "^PREFIX \\(.+\\),\\(.+\\),\\(.*\\)$" (nth 0 lines))
        (match-string 3 (nth 0 lines))))))

(defun racer--complete-at-point-fn ()
  "Run the racer complete command and process the results."
  (let ((racer-tmp-file-name (make-temp-file "racer"))
        (racer-completion-results '()))
    (racer--write-tmp-file racer-tmp-file-name)
    (setenv "RUST_SRC_PATH" (expand-file-name racer-rust-src-path))
    (save-excursion
      (let ((lines (process-lines racer-cmd
                                  "complete"
                                  (number-to-string (racer-get-line-number))
                                  (number-to-string (current-column))
				  (buffer-file-name)
                                  racer-tmp-file-name))
            racer-start-pos
            racer-end-pos)
        (delete-file racer-tmp-file-name)
        (dolist (line lines)
          (when (string-match "^MATCH \\([^,]+\\),\\(.+\\)$" line)
            (let ((completion (match-string 1 line)))
              (push completion racer-completion-results)))

          (when (string-match "^PREFIX \\(.+\\),\\(.+\\),\\(.*\\)$" line)
            (setq racer-start-pos (string-to-number (match-string 1 line)))
            (setq racer-end-pos (string-to-number (match-string 2 line)))))
        (list (- (point) (- racer-end-pos racer-start-pos))
              (point)
              racer-completion-results)))))

(defun racer-company-complete (command &optional arg &rest ignored)
  "Run the racer command for `COMMAND' and format using `ARG'.
`IGNORED' is unused."
  (interactive)
  (when (looking-back "[a-zA-z1-9:.]" nil)
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
      (meta (format "%s" (get-text-property 0 'contextstr arg))))))

;;;###autoload
(defun racer-complete-or-indent ()
  "Complete with company-mode or indent."
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
    (indent-for-tab-command)))

(defun racer--string-ends-with (s ending)
  "Return non-nil if string S ends with ENDING."
  (cond ((>= (length s) (length ending))
         (let ((elength (length ending)))
           (string= (substring s (- 0 elength)) ending)))
        (t nil)))

;;;###autoload
(defun racer-find-definition ()
  "Run the racer find-definition command and process the results."
  (interactive)
  (let ((racer-tmp-file-name (make-temp-file "racer")))
    (racer--write-tmp-file racer-tmp-file-name)
    (setenv "RUST_SRC_PATH" (expand-file-name racer-rust-src-path))
    (ring-insert find-tag-marker-ring (point-marker))
    (let ((lines (process-lines racer-cmd
                                "find-definition"
                                (number-to-string (racer-get-line-number))
                                (number-to-string (current-column))
				(buffer-file-name)
                                racer-tmp-file-name)))
      (delete-file racer-tmp-file-name)
      (dolist (line lines)
        (when (string-match "^MATCH \\([^,]+\\),\\([^,]+\\),\\([^,]+\\),\\([^,]+\\).*$" line)
          (let ((linenum (match-string 2 line))
                (charnum (match-string 3 line))
                (fname (match-string 4 line)))
	    (find-file fname)
            (goto-char (point-min))
            (forward-line (1- (string-to-number linenum)))
            (forward-char (string-to-number charnum))))))))

;;;###autoload
(defun racer-activate ()
  "Add Racer as the completion backend for the current buffer."
  (company-mode)
  (set (make-local-variable 'company-backends) '(racer-company-complete))
  (setq-local company-idle-delay nil))

(provide 'racer)
;;; racer.el ends here
