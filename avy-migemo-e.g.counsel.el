;;; avy-migemo-e.g.counsel.el --- A setting example of avy-migemo for counsel -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2015-2016 momomo5717

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

;; This file is a setting example of avy-migemo for counsel.
;;
;; (require 'avy-migemo-e.g.swiper)
;; (require 'avy-migemo-e.g.counsel)
;;
;; ;; If you want to remove this config from `avy-migemo-function-names' in a init file,
;; (with-eval-after-load 'avy-migemo-e.g.counsel-loaded
;;   (avy-migemo-remove-names 'counsel-grep-function-migemo))

;;; Code:

(with-eval-after-load 'ivy--regex-migemo
  (with-eval-after-load "counsel"

    (defun counsel-pt-function-migemo (string &optional _pred &rest _unused)
      "pt in the current directory for STRING."
      (if (< (length string) 3)
          (counsel-more-chars 3)
        (let ((default-directory counsel--git-grep-dir)
              (regex (replace-regexp-in-string
                      "\\\\|" "|"
                      (counsel-unquote-regex-parens
                       (setq ivy--old-re
                             (ivy--regex-migemo string))))))
          (counsel--async-command
           (format "pt --nocolor --nogroup -e %S" regex))
          nil)))
    (byte-compile 'counsel-pt-function-migemo)

    (defun counsel-pt-migemo (&optional initial-input initial-directory)
      "The same as `counsel-ag' except for using pt and migemo."
      (interactive)
      (setq counsel--git-grep-dir (or initial-directory default-directory))
      (ivy-read "pt: " 'counsel-pt-function-migemo
                :initial-input initial-input
                :dynamic-collection t
                :history 'counsel-git-grep-history
                :action #'counsel-git-grep-action
                :unwind (lambda ()
                          (counsel-delete-process)
                          (swiper--cleanup))))
    (byte-compile 'counsel-pt-migemo)

    (defun counsel-grep-function-migemo (string &optional _pred &rest _unused)
      "The same as `counsel-grep-function' except for using migemo."
      (if (< (length string) 3)
          (counsel-more-chars 3)
        (let ((regex (replace-regexp-in-string ; Adapt for migemo
                      "\\\\|" "|"
                      (counsel-unquote-regex-parens
                       (setq ivy--old-re
                             (ivy--regex-migemo string))))))
          (counsel--async-command
           (format "grep -nP --ignore-case '%s' %s" regex counsel--git-grep-dir))
          nil)))
    (byte-compile 'counsel-grep-function-migemo)

    (avy-migemo-add-names 'counsel-grep-function-migemo)

    ;; `counsel-locate' is incompatible with `avy-migemo-mode'.
    (advice-add 'counsel-locate :around #'avy-migemo-disable-around)
    ;; `counsel-ag' is incompatible with `avy-migemo-mode'.
    (advice-add 'counsel-ag :around #'avy-migemo-disable-around)

    (provide 'avy-migemo-e.g.counsel-loaded)))

(provide 'avy-migemo-e.g.counsel)
;;; avy-migemo-e.g.counsel.el ends here
