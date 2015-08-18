;;; racer.el --- Rust completion via racer with company

;; Copyright (c) 2014 Phil Dawes

;; Author: Phil Dawes
;; URL: https://github.com/phildawes/racer
;; Version: 0.0.2
;; Package-Requires: ((emacs "24.3") (company "0.8.0") (rust-mode "0.2.0") (dash "2.0"))
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
;;
;; Finally, you can also use Racer to show the signature of the
;; current function in the minibuffer:
;;
;; (add-hook 'rust-mode-hook #'racer-turn-on-eldoc)

;;; Code:

(require 'cl-lib)
(require 'etags)
(require 'company)
(require 'rust-mode)
(require 'dash)

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

(defcustom racer-begin-after-member-access t
  "When non-nil, begins completion automatically when the cursor is preceded
by :: or ."
  :type 'boolean
  :group 'racer)

(defun racer-get-line-number ()
  "Gets the current line number at point."
  ; for some reason if the current-column is 0, then the linenumber is off by 1
  (if (= (current-column) 0)
      (1+ (count-lines 1 (point)))
    (count-lines 1 (point))))

(defun racer--write-tmp-file (tmp-file-name)
  "Write the racer temporary file to `TMP-FILE-NAME'."
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
            (put-text-property 0 1 'fname fname completion)
            (put-text-property 0 1 'linenum linenum completion)
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

(defun racer--company-prefix ()
    "Returns the symbol to complete. If racer-begin-after-member-access is t,
begins completion automatically if the cursor is followed by a dot or ::."
  (if racer-begin-after-member-access
    (company-grab-symbol-cons "\\.\\|::" 2)
    (company-grab-symbol)))

(defun racer--company-location (arg)
  (let ((fname (get-text-property 0 'fname arg))
        (linenum (get-text-property 0 'linenum arg)))
    (cons fname (string-to-number linenum))))

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

(defun racer--company-annotation (arg)
  "Gets and formats annotation data from the arg match."
  (let ((meta (get-text-property 0 'contextstr arg)))
    (format "%10s : %s"
            (or (save-match-data
                  (and (string-match "\\(.+\\) {" meta)
                       (match-string 1 meta)))
                meta)
            (get-text-property 0 'matchtype arg))))

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
      (annotation (racer--company-annotation arg))
      (meta (format "%s" (get-text-property 0 'contextstr arg)))))

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
  (set (make-local-variable 'company-backends) '(racer-company-complete)))

(defun racer--syntax-highlight (str)
  "Apply font-lock properties to a string of Rust code."
  (with-temp-buffer
    (insert str)
    ;; Use rust-mode for syntax highlighting, but don't run any of its
    ;; hooks.
    (let ((rust-mode-hook))
      (rust-mode))
    (font-lock-fontify-buffer)
    (buffer-substring (point-min) (point-max))))

(defun racer--goto-func-name ()
  "If point is inside a function call, move to the function name.

foo(bar, |baz); -> foo|(bar, baz);"
  (let ((last-paren-pos (nth 1 (syntax-ppss)))
        (start-pos (point)))
    (when last-paren-pos
      ;; Move to just before the last paren.
      (goto-char last-paren-pos)
      ;; If we're inside a round paren, we're inside a function call.
      (unless (looking-at "(")
        ;; Otherwise, return to our start position, as point may have been on a
        ;; function already:
        ;; foo|(bar, baz);
        (goto-char start-pos)))))

(defun racer--eldoc ()
  "Show eldoc for context at point."
  (save-excursion
    (racer--goto-func-name)
    ;; If there's a variable at point:
    (-when-let (rust-sym (symbol-at-point))
      (-some->>
       ;; then look at the current completion possiblities,
       (racer--candidates)
       ;; extract the possibility that matches this symbol exactly
       (--filter (equal it (symbol-name rust-sym)))
       (-first-item)
       ;; and return the prototype that Racer gave us.
       (get-text-property 0 'contextstr)
       ;; Finally, apply syntax highlighting for the minibuffer.
       (racer--syntax-highlight)))))

;;;###autoload
(defun racer-turn-on-eldoc ()
  "Enable eldoc using Racer."
  (make-local-variable 'eldoc-documentation-function)
  (setq-local eldoc-documentation-function #'racer--eldoc)
  (eldoc-mode))

;;;###autoload
(defun racer-turn-off-eldoc ()
  "Disable eldoc using Racer."
  (kill-local-variable 'eldoc-documentation-function)
  (eldoc-mode -1))

(provide 'racer)
;;; racer.el ends here
