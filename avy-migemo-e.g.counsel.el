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

(defcustom counsel-rg-migemo-base-command counsel-rg-base-command
  "Format string to use in `counsel-rg-migemo'.
The default is `counsel-rg-base-command'."
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

(defun counsel-pt-function-migemo (string base-cmd extra-pt-args)
  "The same as `counsel-ag-function' except for using migemo."
  (when (null extra-pt-args)
    (setq extra-pt-args ""))
  (if (< (length string) 3)
      (counsel-more-chars 3)
    (let ((default-directory counsel--git-grep-dir)
          (regex (counsel-unquote-regex-parens-migemo ; Adapt for migemo
                  (setq ivy--old-re
                        (ivy--regex string)))))
      (let ((pt-cmd (format base-cmd
                            (concat extra-pt-args
                                    " -- "
                                    (shell-quote-argument regex)))))
        (if (file-remote-p default-directory)
            (split-string (shell-command-to-string pt-cmd) "\n" t)
          (counsel--async-command pt-cmd)
          nil)))))
(byte-compile 'counsel-pt-function-migemo)

(defun counsel-pt-migemo-arg-descriptor (base-cmd)
  "ARG-DESCRIPTOR of `interactive' for counsel-pt-migemo \(BASE-CMD)."
  (list nil
        (when current-prefix-arg
          (read-directory-name (concat
                                (car (split-string base-cmd))
                                " in directory: ")))))
(byte-compile 'counsel-pt-migemo-arg-descriptor)

(defun counsel-pt-migemo (&optional initial-input initial-directory extra-pt-args pt-prompt)
  "The same as `counsel-ag' except for using migemo and pt."
  (interactive (counsel-pt-migemo-arg-descriptor counsel-pt-migemo-base-command))
  (ivy-set-prompt 'counsel-pt-migemo counsel-prompt-function)
  (setq counsel--git-grep-dir (or initial-directory default-directory))
  (ivy-read (or pt-prompt (car (split-string counsel-pt-migemo-base-command)))
            (lambda (string)
              (counsel-pt-function-migemo string counsel-pt-migemo-base-command extra-pt-args))
            :initial-input initial-input
            :dynamic-collection t
            :keymap counsel-ag-map
            :history 'counsel-git-grep-history
            :action #'counsel-git-grep-action
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))
            :caller 'counsel-pt-migemo))
;; (byte-compile 'counsel-pt-migemo) ;Suppress a warning message for `counsel-prompt-function'

(defun counsel-pt-migemo-occur (&optional base-cmd)
  "The same as `counsel-ag-occur' except for using pt, migemo and BASE-CMD.
If BASE-CMD is nil, `counsel-pt-migemo-base-command' will be used."
  (setq base-cmd (or base-cmd counsel-pt-migemo-base-command))
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
                  (format base-cmd (shell-quote-argument regex)))
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

;; counsel-rg-migemo
(counsel-set-async-exit-code 'counsel-rg-migemo 1 "No matches found")
(ivy-set-occur 'counsel-rg-migemo 'counsel-rg-migemo-occur)
(ivy-set-display-transformer 'counsel-rg-migemo 'counsel-git-grep-transformer)

(defun counsel-rg-migemo (&optional initial-input initial-directory extra-rg-args rg-prompt)
  "The same as `counsel-rg' except for using migemo"
  (interactive (counsel-pt-migemo-arg-descriptor counsel-rg-migemo-base-command))
  (ivy-set-prompt 'counsel-rg-migemo counsel-prompt-function)
  (setq counsel--git-grep-dir (or initial-directory default-directory))
  (ivy-read (or rg-prompt (car (split-string counsel-rg-migemo-base-command)))
            (lambda (string)
              (counsel-pt-function-migemo string counsel-rg-migemo-base-command extra-rg-args))
            :initial-input initial-input
            :dynamic-collection t
            :keymap counsel-ag-map
            :history 'counsel-git-grep-history
            :action #'counsel-git-grep-action
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))
            :caller 'counsel-rg-migemo))
;; (byte-compile 'counsel-rg-migemo) ;Suppress a warning message for `counsel-prompt-function'

(defun counsel-rg-migemo-occur ()
  "Generate a custom occur buffer for `counsel-rg-migemo'."
  (counsel-pt-migemo-occur counsel-rg-migemo-base-command))
(byte-compile 'counsel-rg-migemo-occur)

;; counsel-grep
(defun counsel-grep-function-migemo (string)
  "The same as `counsel-grep-function' except for using migemo."
  (if (< (length string) 2)
      (counsel-more-chars 2)
    (let ((regex (counsel-unquote-regex-parens-migemo ; Adapt for migemo
                  (setq ivy--old-re
                        (ivy--regex-migemo string)))))
      (counsel--async-command
       (format counsel-grep-base-command-migemo regex
               (shell-quote-argument counsel--git-grep-dir)))
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
                   (shell-quote-argument counsel--git-grep-dir)))
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

(dolist (fn '(counsel-ag
              counsel-rg
              counsel-locate
              counsel-describe-variable
              counsel-describe-function
              counsel-descbinds
              counsel-M-x
              counsel-dpkg
              counsel-rpm))
  (add-to-list 'ivy-migemo-ignore-functions fn))

(add-to-list 'ivy-migemo-ignore-prompts
             (concat "^"
                     (regexp-opt (list "Load library" ; counsel-load-library
                                       "Load custom theme" ; counsel-load-theme
                                       "Find library" ; counsel-find-library
                                       ))))

(avy-migemo-add-names '(counsel-clj :around avy-migemo-disable-around))

(provide 'avy-migemo-e.g.counsel)
;;; avy-migemo-e.g.counsel.el ends here
