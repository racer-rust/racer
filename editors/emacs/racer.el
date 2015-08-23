;;; racer.el --- Rust completion via racer with company

;; Copyright (c) 2014 Phil Dawes

;; Author: Phil Dawes
;; URL: https://github.com/phildawes/racer
;; Version: 0.0.2
;; Package-Requires: ((emacs "24.3") (rust-mode "0.2.0") (dash "2.0") (s "1.9.0"))
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

(require 'dash)
(require 'etags)
(require 'rust-mode)
(require 's)
(require 'thingatpt)

(defgroup racer nil
  "Support for Rust completion via racer."
  :link '(url-link "https://github.com/phildawes/racer/")
  :group 'rust-mode)

(defcustom racer-cmd
  (or (executable-find "racer") "/usr/local/bin/racer")
  "Path to the racer binary."
  :type 'file
  :group 'racer)

(defcustom racer-rust-src-path
  (or (getenv "RUST_SRC_PATH") "/usr/local/src/rust/src")
  "Path to the rust source tree."
  :type 'file
  :group 'racer)

(defun racer--call (command &rest args)
  "Call racer command COMMAND with args ARGS."
  (setenv "RUST_SRC_PATH" (expand-file-name racer-rust-src-path))
  (apply #'process-lines racer-cmd command args))

(defun racer--call-at-point (command)
  "Call racer command COMMAND at point of current buffer."
  (let ((tmp-file (make-temp-file "racer")))
    (write-region nil nil tmp-file nil 'silent)
    (prog1 (racer--call command
                        (number-to-string (line-number-at-pos))
                        (number-to-string (current-column))
                        (buffer-file-name)
                        tmp-file)
      (delete-file tmp-file))))

(defun racer-complete-at-point ()
  "Complete the symbol at point."
  (unless (nth 3 (syntax-ppss)) ;; not in string
    (-let [(start . end) (bounds-of-thing-at-point 'symbol)]
      (list (or start (point)) (or end (point))
            (completion-table-dynamic #'racer-complete)
            :annotation-function #'racer-complete--annotation
            :company-docsig #'racer-complete--docsig
            :company-location #'racer-complete--location))))

(defun racer-complete (&optional _ignore)
  "Completion candidates at point."
  (->> (racer--call-at-point "complete")
       (--filter (s-starts-with? "MATCH" it))
       (--map (-let [(name line col file matchtype ctx)
                     (s-split-up-to "," (s-chop-prefix "MATCH " it) 5)]
                (put-text-property 0 1 'line (string-to-number line) name)
                (put-text-property 0 1 'col (string-to-number col) name)
                (put-text-property 0 1 'file file name)
                (put-text-property 0 1 'matchtype matchtype name)
                (put-text-property 0 1 'ctx ctx name)
                name))))

(defun racer-complete--annotation (arg)
  "Return an annotation for completion candidate ARG."
  (format "%s : %s"
          (s-chop-suffix " {" (get-text-property 0 'ctx arg))
          (get-text-property 0 'matchtype arg)))

(defun racer-complete--docsig (arg)
  "Return a signature for completion candidate ARG."
  (racer--syntax-highlight (format "%s" (get-text-property 0 'ctx arg))))

(defun racer-complete--location (arg)
  "Return location of completion candidate ARG."
  (cons (get-text-property 0 'file arg)
        (get-text-property 0 'line arg)))

;;;###autoload
(defun racer-find-definition ()
  "Run the racer find-definition command and process the results."
  (interactive)
  (-when-let (match (--first (s-starts-with? "MATCH" it)
                             (racer--call-at-point "find-definition")))
    (-let [(_name line col file _matchtype _ctx)
           (s-split-up-to "," (s-chop-prefix "MATCH " match) 5)]
      (if (fboundp 'xref-push-marker-stack)
          (xref-push-marker-stack)
        (with-no-warnings
          (ring-insert find-tag-marker-ring (point-marker))))
      (find-file file)
      (goto-char (point-min))
      (forward-line (1- (string-to-number line)))
      (forward-char (string-to-number col)))))

(defun racer--syntax-highlight (str)
  "Apply font-lock properties to a string STR of Rust code."
  (with-temp-buffer
    (insert str)
    (delay-mode-hooks (rust-mode))
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings
        (font-lock-fontify-buffer)))
    (buffer-string)))

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

(defun racer-eldoc ()
  "Show eldoc for context at point."
  (save-excursion
    (racer--goto-func-name)
    ;; If there's a variable at point:
    (-when-let (rust-sym (symbol-at-point))
      (-some->>
       ;; then look at the current completion possiblities,
       (racer-complete)
       ;; extract the possibility that matches this symbol exactly
       (--filter (string= it (symbol-name rust-sym)))
       (-first-item)
       ;; and return the prototype that Racer gave us.
       (get-text-property 0 'ctx)
       ;; Finally, apply syntax highlighting for the minibuffer.
       (racer--syntax-highlight)))))

(defvar racer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") #'racer-find-definition)
    map))

(define-minor-mode racer-mode
  "Minor mode for racer."
  :lighter " racer"
  :keymap racer-mode-map
  (setq-local eldoc-documentation-function #'racer-eldoc)
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions #'racer-complete-at-point))

(provide 'racer)
;;; racer.el ends here
