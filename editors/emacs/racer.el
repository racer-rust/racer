;;; racer.el --- Support for Rust completion via racer      -*- lexical-binding: t; -*-

;; Copyright (c) 2014 Phil Dawes

;; Author: Phil Dawes
;; URL: https://github.com/phildawes/racer
;; Version: 0.0.2
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (deferred "0.3.1"))
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
;;     (require 'racer)
;;
;;     (setq racer-cmd "/path/to/executable/racer")
;;     (setq racer-rust-src-path "/path/to/rust/src")
;;
;; To set up racer for completion in Rust buffers, add it
;; `rust-mode-hook':
;;
;;     (add-hook 'rust-mode-hook #'racer-mode)

;;; Code:
(eval-when-compile (require 'cl-lib))

(require 'ring)
(require 'deferred)

(defgroup racer nil
  "Support for Rust completion via racer."
  :link '(url-link "https://github.com/phildawes/racer/")
  :prefix "racer-"
  :group 'rust-mode)

(defcustom racer-cmd "racer"
  "Path to the racer executable."
  :type 'file
  :group 'racer)

(defcustom racer-rust-src-path (getenv "RUST_SRC_PATH")
  "Path to rust lang sources, needs to be an absolute path.

If non nil overwrites the value of the environment variable 'RUST_SRC_PATH'."
  :type 'directory
  :group 'racer)

(defcustom racer-goto-definition-marker-ring-length 16
  "Length of marker ring to store `racer-goto-definition' call positions."
  :type 'integer
  :group 'racer)

(defvar racer-temp-file (make-temp-file "company-racer")
  "Temporal file used to call racer.")

(defun racer-process (&rest args)
  "Call `racer-cmd' with ARGS, return a deferred object."
  (let ((process-environment (if racer-rust-src-path
                                 (append (list
                                          (format "RUST_SRC_PATH=%s" (expand-file-name racer-rust-src-path)))
                                         process-environment)
                               process-environment)))
    (apply 'deferred:process racer-cmd args)))

(defun racer-current-line ()
  "Return a string of the current line number."
  (number-to-string (count-lines (point-min)
                                 (min (1+ (point)) (point-max)))))

(defun racer-current-column ()
  "Return a string of the current column number."
  (number-to-string (current-column)))

(defun racer-collect-response (prefix output)
  "Collect lines which start with PREFIX from a racer OUTPUT."
  (delq nil
        (mapcar (lambda (line)
                  (let ((match (and (string-prefix-p prefix line)
                                    (cadr (split-string line " ")))))
                    (and match (split-string match ","))))
                (split-string output "\n"))))


(defun racer-prefix ()
  "Call racer prefix command."
  (let ((line (racer-current-line))
        (column (racer-current-column))
        (fname (or (buffer-file-name) "")))
    (write-region nil nil racer-temp-file nil 0)
    (deferred:nextc
      (racer-process "find-definition" line column fname racer-temp-file)
      (lambda (output) (racer-collect-response "PREFIX" output)))))

(defun racer-complete ()
  "Call racer complete command."
  (let ((line (racer-current-line))
        (column (racer-current-column))
        (fname (or (buffer-file-name) "")))
    (write-region nil nil racer-temp-file nil 0)
    (deferred:nextc
      (racer-process "complete" line column fname racer-temp-file)
      (lambda (output) (racer-collect-response "MATCH" output)))))

(defun racer-find-definition ()
  "Call find-definition racer command."
  (let ((line (racer-current-line))
        (column (racer-current-column))
        (fname (or (buffer-file-name) "")))
    (write-region nil nil racer-temp-file nil 0)
    (deferred:nextc
      (racer-process "find-definition" line column fname racer-temp-file)
      (lambda (output) (racer-collect-response "MATCH" output)))))


(defvar racer-goto-definition-marker-ring (make-ring racer-goto-definition-marker-ring-length)
  "Marker ring that stores `racer-goto-definition' call positions.")

(defun racer-goto-definition-push-marker ()
  "Push point onto goto-definition marker ring."
  (ring-insert racer-goto-definition-marker-ring (point-marker)))

;;;###autoload
(defun racer-goto-definition ()
  "Go to the definition of the object at point."
  (interactive)
  (deferred:nextc
    (racer-find-definition)
    (lambda (values)
      (and values (cl-multiple-value-bind (_ linenum charnum filepath _ _) (car values)
                    (racer-goto-definition-push-marker)
                    (find-file filepath)
                    (goto-char (point-min))
                    (forward-line (1- (string-to-number linenum)))
                    (forward-char (string-to-number charnum)))))))

;;;###autoload
(defun racer-goto-definition-pop-marker ()
  "Pop racer marker."
  (interactive)
  (if (ring-empty-p racer-goto-definition-marker-ring)
      (error "Marker ring is empty, can't pop")
    (let ((marker (ring-remove racer-goto-definition-marker-ring 0)))
      (switch-to-buffer (or (marker-buffer marker)
                            (error "Buffer has been deleted")))
      (goto-char (marker-position marker))
      (set-marker marker nil nil))))

(defvar racer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'racer-goto-definition)
    (define-key map (kbd "M-,") 'racer-goto-definition-pop-marker)
    map))

;;;###autoload
(define-minor-mode racer-mode
  "racer mode.

\\{racer-mode-map}"
  :keymap racer-mode-map
  :group 'racer)

(provide 'racer)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; racer.el ends here
