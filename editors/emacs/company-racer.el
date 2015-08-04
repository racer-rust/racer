;;; company-racer.el --- Rust completion via racer with company            -*- lexical-binding: t; -*-

;; Copyright (c) 2014 Phil Dawes

;; Author: Phil Dawes
;; URL: https://github.com/phildawes/racer
;; Version: 0.0.2
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (company "0.8.0") (deferred "0.3.1") (racer "0.0.2"))
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
;;
;; Setup:
;;
;; Install and configure racer, you need to add to your `init.el':
;;
;;     (require 'company-racer)
;;
;;     (with-eval-after-load 'company
;;       (add-to-list 'company-backends 'company-racer))

;;; Code:
(eval-when-compile (require 'cl-lib))

(require 'racer)
(require 'company)
(require 'deferred)

(defun company-racer-parse-candidate (value)
  "Return a completion candidate from a VALUE."
  (and value (cl-multiple-value-bind (matchstr _ _ _ matchtype contextstr) value
               (put-text-property 0 1 :matchtype matchtype matchstr)
               (put-text-property 0 1 :contextstr contextstr matchstr)
               matchstr)))

(defun company-racer-candidates (callback)
  "Return company candidates for PREFIX with CALLBACK."
  (deferred:nextc
    (racer-complete)
    (lambda (values)
      (funcall callback (mapcar #'company-racer-parse-candidate values)))))

(defun company-racer-prefix ()
  "Get a company prefix from current position."
  (company-grab-symbol-cons "\\.\\|::" 2))

(defun company-racer-meta (candidate)
  "Return a company meta string for a CANDIDATE."
  (get-text-property 0 :contextstr candidate))

(defun company-racer-annotation (candidate)
  "Return a company annotation string for a CANDIDATE."
  (format "%s %10s : %s"
          (make-string (max 0 (- 20 (length candidate))) ? )
          (get-text-property 0 :matchtype candidate)
          (get-text-property 0 :contextstr candidate)))


;;;###autoload
(defun company-racer (command &optional arg &rest ignored)
  "`company-mode' completion back-end for racer.
Provide completion info according to COMMAND and ARG.  IGNORED, not used."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-racer))
    (prefix (and (derived-mode-p 'rust-mode)
                 (not (company-in-string-or-comment))
                 (or (company-racer-prefix) 'stop)))
    (candidates (cons :async 'company-racer-candidates))
    (annotation (company-racer-annotation arg))
    (meta (company-racer-meta arg))
    (doc-buffer nil)
    (duplicates t)
    (location nil)))

(provide 'company-racer)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; company-racer.el ends here
