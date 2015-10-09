;;; avy-migemo-e.g.swiper.el --- A setting example of avy-migemo for swiper -*- lexical-binding: t -*-

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
;; ;; If you remove it from `avy-migemo-function-names',
;; ;; (avy-migemo-remove-names 'ivy--regex-migemo)
;; ;; (remove-hook 'avy-migemo-mode-hook 'avy-migemo-clear-ivy--regex-hash)
;; 

;;; Code:
(require 'avy-migemo)

;; For using swiper ( `ivy--regex' ) with migemo
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
                             (mapcar
                              (lambda (str)
                                (concat (funcall avy-migemo-get-function str)
                                        "\\|" str))
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

  (avy-migemo-add-names 'ivy--regex-migemo)

  (defun avy-migemo-clear-ivy--regex-hash ()
    (setq ivy--regex-hash (make-hash-table :test #'equal)))
  
  (add-hook 'avy-migemo-mode-hook 'avy-migemo-clear-ivy--regex-hash))

(provide 'avy-migemo-e.g.swiper)
;;; avy-migemo-e.g.swiper.el ends here