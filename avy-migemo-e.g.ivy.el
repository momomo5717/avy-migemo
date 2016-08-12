;;; avy-migemo-e.g.ivy.el --- An example config of avy-migemo for ivy -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2016 momomo5717

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

;; This file is an example config of avy-migemo for ivy 0.8.0 or later.
;;
;; (require 'avy-migemo-e.g.ivy)
;;
;; Note: This file is not compiled for installing from MELPA.

;;; Code:
(require 'ivy)
(require 'avy-migemo)

(defcustom ivy-migemo-ignore-functions nil
  "List of function names.
If `this-command' or caller of `ivy-last' is included,
`ivy--regex-migemo-around' will not use migemo."
  :group 'ivy
  :type '(repeat symbol))

(defcustom ivy-migemo-ignore-prompts
  (list (regexp-opt '("symbol" "function" "variable" "binding" "face")))
  "List of regexps.
If `ivy-state-prompt' of `ivy-last' is matched by a regexp,
`ivy--regex-migemo-around' will not use migemo.
The case of the text is ignored."
  :group 'ivy
  :type '(repeat regexp))

(defun ivy--migemo-ignore-p ()
  "Retrun t, if `ivy-last' state is included in `ivy-migemo-ignore-functions' or `ivy-migemo-ignore-prompts'."
  (let ((collection (ivy-state-collection ivy-last))
        (caller (ivy-state-caller ivy-last))
        (prompt (ivy-state-prompt ivy-last))
        (case-fold-search t))
    (or (and (functionp collection)
             (member collection ivy-migemo-ignore-functions))
        (and caller
             (member caller ivy-migemo-ignore-functions))
        (member this-command ivy-migemo-ignore-functions)
        (cl-loop for re in ivy-migemo-ignore-prompts
                 thereis (string-match-p re prompt)))))
(byte-compile 'ivy--migemo-ignore-p)

(defvar avy-migemo--ivy--regex-hash
  (make-hash-table :test #'equal)
  "avy-migemo's `ivy--regex-hash'.")

(defun avy-migemo--ivy--regex-hash-clear ()
  "Clear `avy-migemo--ivy--regex-hash'."
  (clrhash avy-migemo--ivy--regex-hash))
(byte-compile 'avy-migemo--ivy--regex-hash-clear)

(defun ivy--regex-migemo (str &optional greedy)
  "The same as `ivy--regex' except for using migemo."
  (let ((hashed (unless greedy
                  (gethash str avy-migemo--ivy--regex-hash))))
    (if hashed
        (prog1 (cdr hashed)
          (setq ivy--subexps (car hashed)))
      (when (string-match "\\([^\\]\\|^\\)\\\\$" str)
        (setq str (substring str 0 -1)))
      (cdr (puthash str
                    (let ((subs
                           ;; Adapt for migemo
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
                            (if (string-match "\\`\\\\([^?].*\\\\)\\'" x)
                                x
                              (format "\\(%s\\)" x)))
                          subs
                          (if greedy
                              ".*"
                            ".*?")))))
                    avy-migemo--ivy--regex-hash)))))
(byte-compile 'ivy--regex-migemo)

(defun ivy--regex-migemo-around (fn &rest args)
  "Around advice function for `ivy--regex'."
  (if (ivy--migemo-ignore-p)
      (apply fn args)
    (apply #'ivy--regex-migemo args)))
(byte-compile 'ivy--regex-migemo-around)

(defun ivy--regex-ignore-order--part-migemo (str &optional discard)
  "The same as `ivy--regex-ignore-order--part' except for using migemo."
  (let* ((subs (split-string str " +" t))
         (len (length subs)))
    (cl-case len
      (0
       "")
      (t
       (mapcar (lambda (x) (cons (avy-migemo-regex-concat x) (not discard)))
               subs)))))
(byte-compile 'ivy--regex-ignore-order--part-migemo)

(defun ivy--regex-ignore-order--part-migemo-around (fn &rest args)
  "Around advice function for `ivy--regex-ignore-order--part'."
  (if (ivy--migemo-ignore-p) (apply fn args)
    (apply #'ivy--regex-ignore-order--part-migemo args)))
(byte-compile 'ivy--regex-ignore-order--part-migemo-around)

(defun ivy--regex-plus-migemo (str)
  "The same as `ivy--regex-plus' except for using migemo."
  (let ((parts (split-string str "!" t)))
    (cl-case (length parts)
      (0
       "")
      (1
       (if (string= (substring str 0 1) "!")
           (list (cons "" t)
                 (list (ivy--regex-migemo (car parts))))
         (ivy--regex-migemo (car parts))))
      (2
       (cons
        (cons (ivy--regex-migemo (car parts)) t)
        (cl-loop for str in (split-string (cadr parts) " " t)
                 collect (list (avy-migemo-regex-concat str)))))
      (t (error "Unexpected: use only one !")))))
(byte-compile 'ivy--regex-plus-migemo)

(defun ivy--regex-plus-migemo-around (fn &rest args)
  "Around advice function for `ivy--regex-plus'."
  (if (ivy--migemo-ignore-p) (apply fn args)
    (apply #'ivy--regex-plus-migemo args)))
(byte-compile 'ivy--regex-plus-migemo-around)

(defun ivy--format-minibuffer-line-migemo (str)
  "The same as `ivy--format-minibuffer-line' except adapting it for migemo's regexp."
  (let ((start
         (if (and (memq (ivy-state-caller ivy-last)
                        '(counsel-git-grep counsel-ag counsel-pt counsel-pt-migemo))
                  (string-match "^[^:]+:[^:]+:" str))
             (match-end 0)
           0))
        (str (copy-sequence str)))
    (cond ((eq ivy--regex-function 'ivy--regex-ignore-order)
           (when (consp ivy--old-re)
             (let ((i 1))
               (dolist (re ivy--old-re)
                 (when (string-match (car re) str)
                   (ivy-add-face-text-property
                    (match-beginning 0) (match-end 0)
                    (nth (1+ (mod (+ i 2) (1- (length ivy-minibuffer-faces))))
                         ivy-minibuffer-faces)
                    str))
                 (cl-incf i)))))
          ((and (eq ivy-display-style 'fancy)
                (not (eq ivy--regex-function 'ivy--regex-fuzzy)))
           (unless ivy--old-re
             (setq ivy--old-re (funcall ivy--regex-function ivy-text)))
           (ignore-errors
             (while (and (string-match ivy--old-re str start)
                         (> (- (match-end 0) (match-beginning 0)) 0))
               (setq start (match-end 0))
               ;; Adapt for migemo's regexp
               (cl-loop
                with i-face = 0
                with l-mend = 0
                for i from 0 below (if (zerop ivy--subexps) 1
                                     (cl-loop for _ in (match-data) by #'cddr sum 1))
                when (>= l-mend start) return nil
                for mbeg = (match-beginning i)
                for mend = (match-end i)
                when (and mbeg (<= l-mend mbeg)) do
                (let ((face
                       (cond ((zerop ivy--subexps)
                              (cadr ivy-minibuffer-faces))
                             ((zerop i)
                              (car ivy-minibuffer-faces))
                             (t
                              (nth (1+ (mod (+ i-face 2) (1- (length ivy-minibuffer-faces))))
                                   ivy-minibuffer-faces)))))
                  (ivy-add-face-text-property
                   mbeg (if (> i 0) (setq l-mend mend) mend) face str)
                  (cl-incf i-face)))))))
    str))
(byte-compile 'ivy--format-minibuffer-line-migemo)

;; For using with avy-migemo-mode
(avy-migemo-remove-names 'ivy--regex-migemo)
(avy-migemo-add-names '(ivy--regex :around ivy--regex-migemo-around)
                      '(ivy--regex-ignore-order--part
                        :around
                        ivy--regex-ignore-order--part-migemo-around)
                      '(ivy--regex-plus :around ivy--regex-plus-migemo-around)
                      'ivy--format-minibuffer-line-migemo)
(add-hook 'avy-migemo-regex-cache-clear-hook
          'avy-migemo--ivy--regex-hash-clear)

(provide 'avy-migemo-e.g.ivy)
;;; avy-migemo-e.g.ivy.el ends here
