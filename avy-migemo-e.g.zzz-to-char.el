;;; avy-migemo-e.g.zzz-to-char.el --- A setting example of avy-migemo for zzz-to-char  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2015-2017 momomo5717

;; Author: momomo5717
;; URL: https://github.com/momomo5717/avy-migemo

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

;; This file is a setting example of avy-migemo for zzz-to-char.
;;
;; (require 'avy-migemo-e.g.zzz-to-char)
;;
;; ;; If you want to remove this config from `avy-migemo-function-names' in a init file,
;; (with-eval-after-load 'zzz-to-char--base-migemo
;;   (avy-migemo-remove-names 'zzz-to-char--base-migemo))

;;; Code:

;; For using zzz-to-char ( `zzz-to-char--base' ) with migemo
(with-eval-after-load "avy-migemo"
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
             (avy-migemo-regex-quote-concat (string char)))
           nil avy-style
           (max (- p zzz-to-char-reach)
                (window-start))
           (min (+ p zzz-to-char-reach)
                (window-end (selected-window) t))))
        (let ((n (point)))
          (when (/= n p)
            (cl-destructuring-bind (beg . end)
                (if (> n p)
                    (cons p (- (1+ n) n-shift))
                  (cons (+ n n-shift) p))
              (goto-char end)
              (kill-region beg end))))))
    (byte-compile 'zzz-to-char--base-migemo)

    (avy-migemo-add-names 'zzz-to-char--base-migemo)

    (provide 'zzz-to-char--base-migemo)))

(provide 'avy-migemo-e.g.zzz-to-char)
;;; avy-migemo-e.g.zzz-to-char.el ends here
