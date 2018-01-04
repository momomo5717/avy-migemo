;;; avy-migemo-e.g.counsel.el --- An example config of avy-migemo for counsel -*- lexical-binding: t; no-byte-compile: t -*-

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

(defcustom counsel-cmd-to-dired-migemo-grep-command "grep %s -i -P %s"
  "Format string ot use in `counsel-find-file-occur-migemo-around'.
The first fomat specification will be used for the --invert-matching option."
  :type 'string
  :group 'ivy)

(defcustom counsel-cmd-to-dired-migemo-grep-invert-matching "-v"
  "This option will be used for the first fomat specification of `counsel-cmd-to-dired-migemo-grep-command'."
  :type 'string
  :group 'ivy)

(defcustom counsel-find-file-occur-migemo-ls-cmd "ls -a"
  "This command will be used for `counsel-find-file-occur-migemo-around'.
This is \"ls\" command part of `counsel-find-file-occur-cmd'."
  :type 'string
  :group 'ivy)

(defcustom counsel-find-file-occur-migemo-ls-xargs-cmd
  "tr '\\n' '\\0' | xargs -0 ls -d --group-directories-first"
  "This command will be used for `counsel-find-file-occur-migemo-around'.
This is \"xargs\" command part of `counsel-find-file-occur-cmd'."
  :type 'string
  :group 'ivy)

(defcustom counsel-find-file-occur-migemo-find-cmd "find . -maxdepth 1"
  "This command will be used for `counsel-find-file-occur-migemo-around'.
This is \"find\" command part of `counsel--occur-cmd-find'."
  :type 'string
  :group 'ivy)

(defcustom counsel-find-file-occur-migemo-find-xargs-cmd
  "xargs -I {} find {} -maxdepth 0 -ls"
  "This command will be used for `counsel-find-file-occur-migemo-around'.
This is \"xargs\" command part of `counsel--occur-cmd-find'."
  :type 'string
  :group 'ivy)

(defcustom counsel-unquote-regex-parens-migemo-function
  'counsel-unquote-regex-parens-migemo-default
  "Funciton for migemo instead of `counsel-unquote-regex-parens'.
This function will be used for `counsel-unquote-regex-parens-migemo'."
  :type 'function
  :group 'ivy)

;; Helper function
(defun counsel-unquote-regex-parens-migemo-default (str)
  "Replace the backslash of \\\\|  with empty string in STR.
after `counsel-unquote-regex-parens'."
  (replace-regexp-in-string "\\(\\\\\\)|" ""
                            (counsel-unquote-regex-parens str) nil nil 1))
(byte-compile 'counsel-unquote-regex-parens-migemo-default)

(defun counsel-unquote-regex-parens-migemo (str)
  "`counsel-unquote-regex-parens' for migemo.
`counsel-unquote-regex-parens-migemo-function' will be used."
  (funcall counsel-unquote-regex-parens-migemo-function str))
(byte-compile 'counsel-unquote-regex-parens-migemo)

(defun counsel-grep-like-occur-migemo (cmd-template)
  "The same as `counsel-grep-like-occur' except for using migemo."
  (unless (eq major-mode 'ivy-occur-grep-mode)
    (ivy-occur-grep-mode)
    (setq default-directory (ivy-state-directory ivy-last)))
  (setq ivy-text
        (and (string-match "\"\\(.*\\)\"" (buffer-name))
             (match-string 1 (buffer-name))))
  ;; Adapt for migemo
  (let* ((regex
          (funcall (if (string-match-p "\\(\"%s\"\\|'%s'\\)" cmd-template)
                       #'identity
                     #'shell-quote-argument)
                   (counsel-unquote-regex-parens-migemo
                    (ivy--regex-migemo ivy-text))))
         (cmd (format cmd-template regex))
         (cands (split-string (shell-command-to-string cmd) "\n" t)))
    ;; Need precise number of header lines for `wgrep' to work.
    (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
                    default-directory))
    (insert (format "%d candidates:\n" (length cands)))
    (ivy--occur-insert-lines
     (mapcar
      (lambda (cand) (concat "./" cand))
      cands))))
(byte-compile 'counsel-grep-like-occur-migemo)

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
    (let ((default-directory (ivy-state-directory ivy-last))
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
  (let ((default-directory (or initial-directory default-directory)))
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
              :caller 'counsel-pt-migemo)))
;; (byte-compile 'counsel-pt-migemo) ;Suppress a warning message for `counsel-prompt-function'

(defun counsel-pt-migemo-occur ()
  "The same as `counsel-ag-occur' except for using `counsel-pt-migemo-base-command'."
  (counsel-grep-like-occur-migemo counsel-pt-migemo-base-command))
(byte-compile 'counsel-pt-migemo-occur)

;; counsel-rg-migemo
(counsel-set-async-exit-code 'counsel-rg-migemo 1 "No matches found")
(ivy-set-occur 'counsel-rg-migemo 'counsel-rg-migemo-occur)
(ivy-set-display-transformer 'counsel-rg-migemo 'counsel-git-grep-transformer)

(defun counsel-rg-migemo (&optional initial-input initial-directory extra-rg-args rg-prompt)
  "The same as `counsel-rg' except for using migemo"
  (interactive (counsel-pt-migemo-arg-descriptor counsel-rg-migemo-base-command))
  (ivy-set-prompt 'counsel-rg-migemo counsel-prompt-function)
  (let ((default-directory (or initial-directory default-directory)))
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
              :caller 'counsel-rg-migemo)))
;; (byte-compile 'counsel-rg-migemo) ;Suppress a warning message for `counsel-prompt-function'

(defun counsel-rg-migemo-occur ()
  "Generate a custom occur buffer for `counsel-rg-migemo'."
  (counsel-grep-like-occur-migemo
   (replace-regexp-in-string
    (rx (and (or "-M" "--max-columns") (*? " ") (1+ digit)))
    ""
    counsel-rg-migemo-base-command)))
(byte-compile 'counsel-rg-migemo-occur)

;; counsel-grep
(defun counsel-grep-migemo-around (fn &rest args)
  "Around advice for `counsel-grep'."
  (let ((counsel-grep-base-command counsel-grep-base-command-migemo))
    (apply fn args)))
(byte-compile 'counsel-grep-migemo-around)

(defun counsel-grep-function-migemo (string)
  "The same as `counsel-grep-function' except for using migemo."
  (if (< (length string) 2)
      (counsel-more-chars 2)
    (let ((regex (counsel-unquote-regex-parens-migemo ; Adapt for migemo
                  (setq ivy--old-re
                        (ivy--regex-migemo string))))
          (maybe-shell-quote-arg
           (if (string-match-p "\\(\"%s\"\\|'%s'\\)" counsel-grep-command)
               #'identity
             #'shell-quote-argument)))
      (counsel--async-command
       (format counsel-grep-command (funcall maybe-shell-quote-arg regex)))
      nil)))
(byte-compile 'counsel-grep-function-migemo)

(defun counsel-grep-occur-migemo ()
  "The same as `counsel-grep' except for using migemo."
  (counsel-grep-like-occur-migemo
   (mapconcat #'identity
              (list (replace-regexp-in-string
                     "%s$" "" counsel-grep-base-command-migemo)
                    (shell-quote-argument
                     (file-name-nondirectory
                      (buffer-file-name
                       (ivy-state-buffer ivy-last))))
                    "/dev/null")
              " ")))
(byte-compile 'counsel-grep-occur-migemo)

;; counsel-cmd-to-dired
(defun counsel-migemo--compose-grep-cmd-to-dired (re)
  "Compose grep command from RE for using `counsel-cmd-to-dired'."
  (cl-labels ((quote-re (re) (shell-quote-argument (counsel-unquote-regex-parens-migemo re))))
    (let ((grep-format counsel-cmd-to-dired-migemo-grep-command)
          (invert-opt counsel-cmd-to-dired-migemo-grep-invert-matching))
      (mapconcat (lambda (pair)
                   (format grep-format
                           (if (cdr pair) "" invert-opt) (quote-re (car pair))))
                 (if (listp re) re `((,re . t)))
                 " | "))))
(byte-compile 'counsel-migemo--compose-grep-cmd-to-dired)

(defun counsel-git-occur-migemo-around (fn &rest args)
  "Around advice for `counsel-git-occur'."
  (if (ivy-migemo-ignore-p)
      (apply fn args)
    (cd (ivy-state-directory ivy-last))
    (counsel-cmd-to-dired
     (counsel--expand-ls
      (concat counsel-git-cmd " | "
              (counsel-migemo--compose-grep-cmd-to-dired ivy--old-re)
              " | xargs ls")))))
(byte-compile 'counsel-git-occur-migemo-around)

(defun counsel--occur-cmd-find-migemo ()
  "The same as `counsel--occur-cmd-find' except for using migemo."
  (let* ((find counsel-find-file-occur-migemo-find-cmd)
         (grep-v-dot (if (string-match-p "^\\." ivy-text) "" "grep -v '/\\.' | "))
         (grep (counsel-migemo--compose-grep-cmd-to-dired ivy--old-re))
         (xargs counsel-find-file-occur-migemo-find-xargs-cmd)
         (grep-xargs (concat " | " grep-v-dot grep  " | " xargs)))
    (concat find " -type d " grep-xargs " && " find " -type f " grep-xargs)))
(byte-compile 'counsel--occur-cmd-find-migemo)

(defun counsel-find-file-occur-migemo-around (fn &rest args)
  "Around advice for `counsel-find-file-occur'."
  (if (ivy-migemo-ignore-p)
      (apply fn args)
    (cd ivy--directory)
    (if (and counsel-find-file-occur-use-find
             (require 'find-dired))
        (counsel-cmd-to-dired
         (counsel--occur-cmd-find-migemo)
         'find-dired-filter)
      (let ((ls counsel-find-file-occur-migemo-ls-cmd)
            (xargs counsel-find-file-occur-migemo-ls-xargs-cmd)
            (grep (counsel-migemo--compose-grep-cmd-to-dired ivy--old-re)))
        (counsel-cmd-to-dired
         (counsel--expand-ls (concat ls " | " grep " | " xargs)))))))
(byte-compile 'counsel-find-file-occur-migemo-around)

;; For using with `avy-migemo-mode'
(avy-migemo-add-names '(counsel-grep :around counsel-grep-migemo-around)
                      'counsel-grep-function-migemo
                      'counsel-grep-occur-migemo
                      '(counsel-git-occur
                        :around
                        counsel-git-occur-migemo-around)
                      '(counsel-find-file-occur
                        :around
                        counsel-find-file-occur-migemo-around))

(dolist (fn '(counsel-ag
              counsel-rg
              counsel-locate
              counsel-describe-variable
              counsel-describe-function
              counsel-descbinds
              counsel-M-x
              counsel-dpkg
              counsel-rpm
              counsel-irony
              counsel-git-grep
              counsel-fzf))
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
