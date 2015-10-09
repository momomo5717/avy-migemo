;;; avy-migemo-e.g.zzz-to-char.el --- A setting example of avy-migemo for zzz-to-char  -*- lexical-binding: t -*-

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

;; This package is a setting example of avy-migemo for zzz-to-char.
;;
;; (require 'avy-migemo-e.g.zzz-to-char)
;; ;; If you remove it from `avy-migemo-function-names',
;; ;; (avy-migemo-remove-names 'zzz-to-char--base-migemo)

;;; Code:
(require 'avy-migemo)

;; For using zzz-to-char ( `zzz-to-char--base-migemo' ) with migemo
(with-eval-after-load "zzz-to-char"
  (defun zzz-to-char--base-migemo (char n-shift)
    "The same as `zzz-to-char--base' except for using migemo."
    (let ((p (point))
          (avy-all-windows nil))
      (avy-with zzz-to-char
        (avy--generic-jump
         (if (= 13 char)
             "\n"
           ;; Adapt for migemo
           (funcall avy-migemo-get-function (string char)))
         nil
         avy-style
         (- p zzz-to-char-reach)
         (+ p zzz-to-char-reach)))
      (let ((n (point)))
        (when (/= n p)
          (cl-destructuring-bind (beg . end)
              (if (> n p)
                  (cons p (- (1+ n) n-shift))
                (cons (+ n n-shift) p))
            (goto-char end)
            (kill-region beg end))))))

  (avy-migemo-add-names 'zzz-to-char--base-migemo))

(provide 'avy-migemo-e.g.zzz-to-char)
;;; avy-migemo-e.g.zzz-to-char.el ends here