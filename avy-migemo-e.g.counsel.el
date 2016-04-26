;;; avy-migemo-e.g.counsel.el --- An example config of avy-migemo for counsel -*- lexical-binding: t; no-byte-compile: t -*-

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

;; This file is an example config of avy-migemo for counsel 0.8.0 or later.
;;
;; (require 'avy-migemo-e.g.counsel)
;;
;; Note: This file is not compiled for installing from MELPA.

;;; Code:
(require 'counsel)
(require 'avy-migemo-e.g.swiper)

;; Customization
(defcustom counsel-pt-migemo-base-command counsel-pt-base-command
  "Format string to use in `counsel-pt-migemo'.
The default is `counsel-pt-base-command'."
  :type 'string
  :group 'ivy)

(defcustom counsel-grep-base-command-migemo counsel-grep-base-command
  "Format string to use in `counsel-grep-function-migemo' to construct
the command. The default is `counsel-grep-base-command'."
  :type 'string
  :group 'ivy)

;; Helper function
(defun counsel-unquote-regex-parens-migemo (str)
  "`counsel-unquote-regex-parens' for migemo."
  (replace-regexp-in-string "\\\\|" "|" (counsel-unquote-regex-parens str)))
(byte-compile 'counsel-unquote-regex-parens-migemo)

;; counsel-pt-migemo
(counsel-set-async-exit-code 'counsel-pt-migemo 1 "No matches found")
(ivy-set-occur 'counsel-pt-migemo 'counsel-pt-migemo-occur)
(ivy-set-display-transformer 'counsel-pt-migemo 'counsel-git-grep-transformer)

(defun counsel-pt-function-migemo (string)
  "pt in the current directory for STRING."
  (if (< (length string) 3)
      (counsel-more-chars 3)
    (let ((default-directory counsel--git-grep-dir)
          (regex (counsel-unquote-regex-parens-migemo ; Adapt for migemo
                  (setq ivy--old-re
                        (ivy--regex-migemo string)))))
      (counsel--async-command
       (format counsel-pt-migemo-base-command (shell-quote-argument regex)))
      nil)))
(byte-compile 'counsel-pt-function-migemo)

(defun counsel-pt-migemo (&optional initial-input initial-directory)
  "The same as `counsel-ag' except for using pt and migemo."
  (interactive
   (list nil
         (when current-prefix-arg
           (read-directory-name "pt in directory: "))))
  (setq counsel--git-grep-dir (or initial-directory default-directory))
  (ivy-read "pt: " 'counsel-pt-function-migemo
            :initial-input initial-input
            :dynamic-collection t
            :keymap counsel-ag-map
            :history 'counsel-git-grep-history
            :action #'counsel-git-grep-action
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))
            :caller 'counsel-pt-migemo))
(byte-compile 'counsel-pt-migemo)

(defun counsel-pt-migemo-occur ()
  "The same as `counsel-ag-occur' except for using pt and migemo."
  (unless (eq major-mode 'ivy-occur-grep-mode)
    (ivy-occur-grep-mode))
  (setq default-directory counsel--git-grep-dir)
  (let* ((regex (counsel-unquote-regex-parens-migemo ; Adapt for migemo
                 (setq ivy--old-re
                       (ivy--regex-migemo
                        (progn (string-match "\"\\(.*\\)\"" (buffer-name))
                               (match-string 1 (buffer-name)))))))
         (cands (split-string
                 (shell-command-to-string
                  (format counsel-pt-migemo-base-command (shell-quote-argument regex)))
                 "\n"
                 t)))
    ;; Need precise number of header lines for `wgrep' to work.
    (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
                    default-directory))
    (insert (format "%d candidates:\n" (length cands)))
    (ivy--occur-insert-lines
     (mapcar
      (lambda (cand) (concat "./" cand))
      cands))))
(byte-compile 'counsel-pt-migemo-occur)

;; counsel-grep
(defun counsel-grep-function-migemo (string)
  "The same as `counsel-grep-function' except for using migemo."
  (if (< (length string) 3)
      (counsel-more-chars 3)
    (let ((regex (counsel-unquote-regex-parens-migemo ; Adapt for migemo
                  (setq ivy--old-re
                        (ivy--regex-migemo string)))))
      (counsel--async-command
       (format counsel-grep-base-command-migemo regex counsel--git-grep-dir))
      nil)))
(byte-compile 'counsel-grep-function-migemo)

(defun counsel-grep-occur-migemo ()
  "Generate a custom occur buffer for `counsel-grep'."
  (unless (eq major-mode 'ivy-occur-grep-mode)
    (ivy-occur-grep-mode))
  (let ((cands
         (split-string
          (shell-command-to-string
           (format counsel-grep-base-command-migemo ; Adapt for migemo
                   (counsel-unquote-regex-parens-migemo
                    (setq ivy--old-re
                          (ivy--regex-migemo
                           (progn (string-match "\"\\(.*\\)\"" (buffer-name))
                                  (match-string 1 (buffer-name))))))
                   counsel--git-grep-dir))
          "\n" t))
        (file (file-name-nondirectory counsel--git-grep-dir)))
    ;; Need precise number of header lines for `wgrep' to work.
    (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
                    default-directory))
    (insert (format "%d candidates:\n" (length cands)))
    (ivy--occur-insert-lines
     (mapcar
      (lambda (cand) (concat "./" file ":" cand))
      cands))))
(byte-compile 'counsel-grep-occur-migemo)

;; For using with `avy-migemo-mode'
(avy-migemo-add-names 'counsel-grep-function-migemo
                      'counsel-grep-occur-migemo)

;; `counsel-locate' is incompatible with `avy-migemo-mode'.
(advice-add 'counsel-locate :around #'avy-migemo-disable-around)
;; `counsel-ag' is incompatible with `avy-migemo-mode'.
(advice-add 'counsel-ag :around #'avy-migemo-disable-around)

(provide 'avy-migemo-e.g.counsel)
;;; avy-migemo-e.g.counsel.el ends here
