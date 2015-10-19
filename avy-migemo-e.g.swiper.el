;;; avy-migemo-e.g.swiper.el --- A setting example of avy-migemo for swiper -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2015 momomo5717

;; URL: https://github.com/momomo5717/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by

;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a setting example of avy-migemo for swiper
;;
;; (require 'avy-migemo-e.g.swiper)
;;
;; ;; If you remove it from `avy-migemo-function-names' in a init file,
;; ;; (with-eval-after-load 'ivy--regex-migemo
;; ;;   (avy-migemo-remove-names 'ivy--regex-migemo 'ivy--format-minibuffer-line-migemo)
;; ;;   (remove-hook 'avy-migemo-mode-hook 'avy-migemo-clear-ivy--regex-hash))

;;; Code:

;; For using swiper ( `ivy--regex' ) with migemo
(with-eval-after-load "avy-migemo"
  (with-eval-after-load "ivy"
    (defun ivy--regex-migemo (str &optional greedy)
      "The same as `ivy--regex' except for using migemo."
      (let ((hashed (unless greedy
                      (gethash str ivy--regex-hash))))
        (if hashed
            (prog1 (cdr hashed)
              (setq ivy--subexps (car hashed)))
          (when (string-match "\\([^\\]\\|^\\)\\\\$" str)
            (setq str (substring str 0 -1)))
          (cdr (puthash str
                        (let ((subs
                               ;; Adapt for mgiemo
                               (mapcar #'avy-migemo-regex-concat
                                       (ivy--split str))))
                          (if (= (length subs) 1)
                              (cons
                               (setq ivy--subexps 0)
                               (car subs))
                            (cons
                             (setq ivy--subexps (length subs))
                             (mapconcat
                              (lambda (x)
                                (if (string-match "\\`\\\\(.*\\\\)\\'" x)
                                    x
                                  (format "\\(%s\\)" x)))
                              subs
                              (if greedy
                                  ".*"
                                ".*?")))))
                        ivy--regex-hash)))))
    (byte-compile 'ivy--regex-migemo)

    (defvar avy-migemo--ivy-old-re "")
    (defvar avy-migemo--ivy-old-re-depth 0)
    (defun ivy--format-minibuffer-line-migemo (str)
      "The same as `ivy--format-minibuffer-line' except adapting it for migemo's regexp."
      (let ((start 0)
            (str (copy-sequence str)))
        (when (eq ivy-display-style 'fancy)
          (unless ivy--old-re
            (setq ivy--old-re (funcall ivy--regex-function ivy-text)))
          (while (and (string-match ivy--old-re str start)
                      (> (- (match-end 0) (match-beginning 0)) 0))
            (setq start (match-end 0))
            ;; Adapt for migemo's regexp
            (let ((i 0) (i-face 0)
                  mbeg mend (l-mend 0)
                  (re-depth+1 (if (zerop ivy--subexps)
                                  1
                                (1+ (if (eq avy-migemo--ivy-old-re ivy--old-re)
                                        avy-migemo--ivy-old-re-depth
                                      (setq avy-migemo--ivy-old-re ivy--old-re
                                            avy-migemo--ivy-old-re-depth
                                            (regexp-opt-depth ivy--old-re)))))))
              (while (< i re-depth+1)
                (setq mbeg (match-beginning i)
                      mend (match-end i))
                (when (and mbeg (<= l-mend mbeg) mend)
                  (let ((face
                         (cond ((zerop ivy--subexps)
                                (cadr swiper-minibuffer-faces))
                               ((zerop i)
                                (car swiper-minibuffer-faces))
                               (t
                                (nth (1+ (mod (+ i-face 2) (1- (length swiper-minibuffer-faces))))
                                     swiper-minibuffer-faces)))))
                    (if (fboundp 'add-face-text-property)
                        (add-face-text-property
                         mbeg
                         (if (> i 0) (setq l-mend mend) mend)
                         face
                         nil
                         str)
                      (font-lock-append-text-property
                       mbeg
                       (if (> i 0) (setq l-mend mend) mend)
                       'face
                       face
                       str))
                    (cl-incf i-face)))
                (cl-incf i)))))
        str))
    (byte-compile 'ivy--format-minibuffer-line-migemo)

    (defun avy-migemo-clear-ivy--regex-hash ()
      (setq ivy--regex-hash (make-hash-table :test #'equal)))
    (byte-compile 'avy-migemo-clear-ivy--regex-hash)

    (add-hook 'avy-migemo-mode-hook 'avy-migemo-clear-ivy--regex-hash)
    (avy-migemo-add-names 'ivy--regex-migemo
                          'ivy--format-minibuffer-line-migemo)

    (provide 'ivy--regex-migemo)))

(provide 'avy-migemo-e.g.swiper)
;;; avy-migemo-e.g.swiper.el ends here
