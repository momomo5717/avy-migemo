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
;; ;;   (avy-migemo-remove-names 'ivy--regex-migemo)
;; ;;   (remove-hook 'avy-migemo-mode-hook 'avy-migemo-clear-ivy--regex-hash)
;; ;;   (remove-hook 'avy-migemo-mode-hook 'avy-migemo--backup-ivy-display-style))

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

    (defun avy-migemo-clear-ivy--regex-hash ()
      (setq ivy--regex-hash (make-hash-table :test #'equal)))
    (byte-compile 'avy-migemo-clear-ivy--regex-hash)

    (defvar avy-migemo--ivy-display-style-default ivy-display-style)

    (defun avy-migemo--backup-ivy-display-style ()
      "Set `ivy-display-style' to nil, if `avy-migemo-mode' is non-nil."
      (if avy-migemo-mode
          (setq avy-migemo--ivy-display-style-default
                (or ivy-display-style avy-migemo--ivy-display-style-default)
                ivy-display-style nil)
        (setq ivy-display-style
              (or ivy-display-style
                  avy-migemo--ivy-display-style-default)
              avy-migemo--ivy-display-style-default nil)))
    (byte-compile 'avy-migemo--backup-ivy-display-style)

    (add-hook 'avy-migemo-mode-hook 'avy-migemo-clear-ivy--regex-hash)
    (add-hook 'avy-migemo-mode-hook 'avy-migemo--backup-ivy-display-style)
    (avy-migemo-add-names 'ivy--regex-migemo)

    (provide 'ivy--regex-migemo)))

(provide 'avy-migemo-e.g.swiper)
;;; avy-migemo-e.g.swiper.el ends here
