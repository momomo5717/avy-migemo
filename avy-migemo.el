;;; avy-migemo.el --- avy with migemo -*- lexical-binding: t -*-

;; Copyright (C) 2015 momomo5717

;; Keywords: avy, migemo
;; Version: 0.1.2
;; Package-Requires:((emacs "24.4") (avy "0.3") (migemo "1.9"))
;; URL: https://github.com/momomo5717/avy-migemo

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by

;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides the following functions:
;;
;;   + avy-migemo-goto-char
;;   + avy-migemo-goto-char-in-line
;;   + avy-migemo-goto-char-2
;;   + avy-migemo-isearch
;;   + avy-migemo-goto-word-1
;;   + avy-migemo-goto-char-timer
;;
;;  These are the same as avy's predefined functions
;;  except for adding candidates via migemo (simply using migemo as `regexp-quote').
;;
;; Setup:
;; (add-to-list 'load-path "/path/to/avy-migemo")
;; (require 'avy-migemo)
;; ;; If you override avy's predefined functions using `advice-add',
;; (avy-migemo-mode 1)

;;; Code:
(require 'avy)
(require 'migemo)

(defgroup avy-migemo nil
  "avy with migemo."
  :group  'avy
  :prefix "avy-migemo-")

(defcustom avy-migemo-get-function 'migemo-search-pattern-get
  "Getter function of migemo.
It takes a string and returns a regular expression."
  :type 'function)

(defcustom avy-migemo-function-names
  '(avy-migemo-goto-char
    avy-migemo-goto-char-in-line
    avy-migemo-goto-char-2
    avy-migemo-isearch
    avy-migemo-goto-word-1
    avy-migemo-goto-char-timer)
  "Function names for overriding avy's functions."
  :set (lambda (symbol value)
         (if (and (boundp symbol) (boundp 'avy-migemo-mode))
             (let ((avy-migemo-mode-p avy-migemo-mode))
               (ignore-errors (when avy-migemo-mode-p (avy-migemo-mode -1)))
               (set symbol value)
               (ignore-errors (when avy-migemo-mode-p (avy-migemo-mode 1))))
           (set symbol value))))

;;;###autoload
(defun avy-migemo-goto-char (char &optional arg)
  "The same as `avy-migemo-goto-char' except for the candidates via migemo."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-char
    (avy--generic-jump
     (if (= 13 char)
         "\n"
       (funcall avy-migemo-get-function (string char)))
     arg
     avy-style)))

;;;###autoload
(defun avy-migemo-goto-char-in-line (char)
  "The same as `avy-goto-char-in-line' except for the candidates via migemo."
  (interactive (list (read-char "char: " t)))
  (avy-with avy-goto-char
    (avy--generic-jump
     (funcall avy-migemo-get-function (string char))
     avy-all-windows
     avy-style
     (line-beginning-position)
     (line-end-position))))

;;;###autoload
(defun avy-migemo-goto-char-2 (char1 char2 &optional arg)
  "The same as `avy-goto-char-2' except for the candidates via migemo."
  (interactive (list (read-char "char 1: " t)
                     (read-char "char 2: " t)
                     current-prefix-arg))
  (avy-with avy-goto-char-2
    (avy--generic-jump
     (funcall avy-migemo-get-function (string char1 char2))
     arg
     avy-style)))

;;;###autoload
(defun avy-migemo-isearch ()
  "The same as `avy-isearch' except for the candidates via migemo."
  (interactive)
  (avy-with avy-isearch
    (let ((avy-background nil))
      (avy--process
       (avy--regex-candidates
        (concat isearch-string "\\|"
                (funcall avy-migemo-get-function isearch-string)))
       (avy--style-fn avy-style))
      (isearch-done))))

;;;###autoload
(defun avy-migemo-goto-word-1 (char &optional arg)
  "The same as `avy-goto-word-1' except for the candidates via migemo."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-word-1
    (let* ((str (string char))
           (regex (cond ((string= str ".")
                         "\\.")
                        ((and avy-word-punc-regexp
                              (string-match avy-word-punc-regexp str))
                         (funcall avy-migemo-get-function str))
                        (t
                         (concat
                          "\\b"
                          (funcall avy-migemo-get-function str))))))
      (avy--generic-jump regex arg avy-style))))

;;;###autoload
(defun avy-migemo-goto-char-timer (&optional arg)
  "The same as `avy-goto-char-timer' except for the candidates via migemo."
  (interactive "P")
  (let ((c1 (read-char "char 1: " t))
        (c2 (read-char "char 2: " t avy-timeout-seconds)))
    (avy-with avy-goto-char-timer
      (avy--generic-jump
       (funcall avy-migemo-get-function
                (if c2
                    (string c1 c2)
                  (string c1)))
       arg
       avy-style))))

;;;###autoload
(define-minor-mode avy-migemo-mode
  "Override avy's functions."
  :global t
  :group 'avy
  (dolist (name avy-migemo-function-names)
    (let ((predefined-name
           (intern (replace-regexp-in-string
                    "-migemo" "" (symbol-name name)))))
      (if avy-migemo-mode
          (advice-add predefined-name :override name)
        (advice-remove predefined-name name)))))

(provide 'avy-migemo)
;;; avy-migemo.el ends here
