;;; copy this to your load-path and add following to your ~/.emacs

;; (setq racer-cmd "/your/path/to/bin/racer")
;; (setq rust-srcpath "/your/path/to/rust/src")
;; (require 'racer-autocomplete)
;; (add-hook 'rust-mode-hook
;;           #'(lambda ()
;;             (add-to-list 'ac-sources 'ac-source-racer)
;;             ))

(defvar racer-cmd "/home/pld/src/rust/racer/bin/racer")
(defvar rust-srcpath "/usr/local/src/rust/src")

;;; rust-mode no longer requires cl, so am putting it here for now (this file uses 'case')
(require 'cl)

(defun racer--write-tmp-file (tmpfilename)
    (push-mark)
    (write-region nil nil tmpfilename))

(defun racer--tempfilename ()
  (concat temporary-file-directory (file-name-nondirectory (buffer-file-name)) ".racertmp"))

(defun racer--candidates ()
  (setenv "RUST_SRC_PATH" rust-srcpath)
  (let ((tmpfilename (racer--tempfilename)))
    (racer--write-tmp-file tmpfilename)
    (unwind-protect
        (progn
          (let ((completion-results (list))
                (lines (process-lines racer-cmd
                                      "complete"
                                      (number-to-string (count-lines 1 (point)))
                                      (number-to-string (current-column))
                                      tmpfilename)))
            (dolist (line lines)
              (when (string-match "^MATCH \\([^,]+\\),\\(.+\\)$" line)
                (let ((completion (match-string 1 line)))
                  (push completion completion-results))))
            completion-results))
      (delete-file tmpfilename))))

(defun racer--prefix ()
  (setenv "RUST_SRC_PATH" rust-srcpath)
  (let ((tmpfilename (racer--tempfilename)))
    (racer--write-tmp-file tmpfilename)
    (unwind-protect
        (progn
          (let ((lines (process-lines racer-cmd
                                      "prefix"
                                      (number-to-string (count-lines 1 (point)))
                                      (number-to-string (current-column))
                                      tmpfilename)))

            (when (string-match "^PREFIX \\(.+\\),\\(.+\\),\\(.*\\)$" (nth 0 lines))
              (+ (- (point) (current-column))
                 (string-to-number (match-string 1 (nth 0 lines)))))))
      (delete-file tmpfilename))))

(ac-define-source racer
  '((candidates . racer--candidates)
    (prefix . racer--prefix)
    (requires . 0)
    (cache)
    (symbol . "R")))

(provide 'racer-autocomplete)
