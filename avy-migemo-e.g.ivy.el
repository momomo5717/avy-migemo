;;; avy-migemo-e.g.ivy.el --- An example config of avy-migemo for ivy -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2016-2017 momomo5717

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

(defcustom ivy-migemo-get-function 'avy-migemo-regex-concat-nnl
  "Function which takes a string and returns a regular expression.
This variable will be used for `ivy--regex-migemo', `ivy--regex-plus-migemo'
and `ivy--regex-ignore-order--part-migemo'."
  :group 'ivy
  :type 'function)

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

(defcustom ivy-migemo-preferred-functions nil
  "List of function names.
If `this-command' or caller of `ivy-last' is included,
`ivy--regex-migemo-around' will use migemo.
If non-nil, `ivy-migemo-ignore-functions' will be ignored."
  :group 'ivy
  :type '(repeat symbol))

(defun ivy-migemo--include-caller-p (names)
  "Return t, if NAMES include `this-command' or caller/collection of `ivy-last'."
  (let ((caller (ivy-state-caller ivy-last))
        (collection (ivy-state-collection ivy-last)))
    (or (and caller
             (memq caller names))
        (and (functionp collection)
             (member collection names))
        (memq this-command names))))
(byte-compile 'ivy-migemo--include-caller-p)

(defun ivy-migemo-ignore-p ()
  "Retrun t, if `ivy-migemo-ignore-functions'/`ivy-migemo-ignore-prompts' has caller/collection/prompt of `ivy-last.
Return nil if `ivy-migemo-preferred-functions' has caller/collection/prompt of `ivy-last.
If `ivy-migemo-preferred-functions' is non-nil,
`ivy-migemo-ignore-functions'/`ivy-migemo-ignore-prompts' will be ignored."
  (let ((prompt (ivy-state-prompt ivy-last))
        (case-fold-search t))
    (if ivy-migemo-preferred-functions
        (not (ivy-migemo--include-caller-p ivy-migemo-preferred-functions))
      (or (ivy-migemo--include-caller-p ivy-migemo-ignore-functions)
          (cl-loop for re in ivy-migemo-ignore-prompts
                   thereis (string-match-p re prompt))))))
(byte-compile 'ivy-migemo-ignore-p)

(define-obsolete-function-alias 'ivy--migemo-ignore-p
  'ivy-migemo-ignore-p
  "20171218")

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
                           (mapcar ivy-migemo-get-function
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
  (if (ivy-migemo-ignore-p)
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
       (mapcar (lambda (x) (cons (funcall ivy-migemo-get-function x) (not discard)))
               subs)))))
(byte-compile 'ivy--regex-ignore-order--part-migemo)

(defun ivy--regex-ignore-order--part-migemo-around (fn &rest args)
  "Around advice function for `ivy--regex-ignore-order--part'."
  (if (ivy-migemo-ignore-p) (apply fn args)
    (apply #'ivy--regex-ignore-order--part-migemo args)))
(byte-compile 'ivy--regex-ignore-order--part-migemo-around)

(make-obsolete 'ivy--regex-ignore-order--part-migemo-around
               "This will be removed in the future."
               "20170908")

(defun ivy--regex-or-literal-migemo-around (fn &rest args)
  "Around advice function for `ivy--regex-or-literal'."
  (if (ivy-migemo-ignore-p) (apply fn args)
    (apply ivy-migemo-get-function args)))
(byte-compile 'ivy--regex-or-literal-migemo-around)

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
                 collect (list (funcall ivy-migemo-get-function str)))))
      (t (error "Unexpected: use only one !")))))
(byte-compile 'ivy--regex-plus-migemo)

(defun ivy--regex-plus-migemo-around (fn &rest args)
  "Around advice function for `ivy--regex-plus'."
  (if (ivy-migemo-ignore-p) (apply fn args)
    (apply #'ivy--regex-plus-migemo args)))
(byte-compile 'ivy--regex-plus-migemo-around)

(defun ivy--format-minibuffer-line-migemo (str)
  "The same as `ivy--format-minibuffer-line' except adapting it for migemo's regexp."
  (let ((start
         (if (and (memq (ivy-state-caller ivy-last)
                        '(counsel-git-grep counsel-ag counsel-pt counsel-rg
                          counsel-pt-migemo counsel-rg-migemo))
                  (string-match "^[^:]+:[^:]+:" str))
             (match-end 0)
           0))
        (str (copy-sequence str))
        fuzzy-p)
    (when (eq ivy-display-style 'fancy)
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
            ((and (setq fuzzy-p
                        (or (eq ivy--regex-function 'ivy--regex-fuzzy)
                            (and (eq ivy--regex-function 'swiper--re-builder)
                                 (let ((caller (ivy-state-caller ivy-last)))
                                   (eq (or (and caller
                                                (cdr (assoc caller ivy-re-builders-alist)))
                                           (cdr (assoc t ivy-re-builders-alist)))
                                       'ivy--regex-fuzzy)))))
                  ivy--flx-featurep)
             (let ((flx-name (if (string-match "^\\^" ivy-text)
                                 (substring ivy-text 1)
                               ivy-text)))
               (setq str
                     (ivy--flx-propertize
                      (cons (flx-score str flx-name ivy--flx-cache) str)))))
            (t
             (unless ivy--old-re
               (setq ivy--old-re (funcall ivy--regex-function ivy-text)))
             (ignore-errors
               (while (and (string-match ivy--old-re str start)
                           (> (- (match-end 0) (match-beginning 0)) 0))
                 (setq start (match-end 0))
                 ;; Adapt for migemo's regexp
                 (cl-loop
                  with i-face = (if fuzzy-p 1 0)
                  with c-mbeg = nil
                  with l-mend = 0
                  for i from (if fuzzy-p 1 0) below (if (zerop ivy--subexps) 1
                                                      (/ (length (match-data)) 2))
                  when (>= l-mend start) return nil
                  for mbeg = (or c-mbeg (match-beginning i))
                  for mend = (match-end i)
                  when (and mbeg mend (<= l-mend mbeg)) do
                  (setq c-mbeg (or c-mbeg mbeg))
                  (unless (and (match-beginning (1+ i))
                               (= mend (match-beginning (1+ i))))
                    (let ((face
                           (cond ((zerop ivy--subexps)
                                  (cadr ivy-minibuffer-faces))
                                 ((zerop i)
                                  (car ivy-minibuffer-faces))
                                 (t
                                  (nth (1+ (mod (+ i-face 2)
                                                (1- (length ivy-minibuffer-faces))))
                                       ivy-minibuffer-faces)))))
                      (ivy-add-face-text-property
                       mbeg (if (> i 0) (setq l-mend mend) mend) face str)
                      (setq c-mbeg nil)
                      (cl-incf i-face)))))))))
    str))
(byte-compile 'ivy--format-minibuffer-line-migemo)
(make-obsolete 'ivy--format-minibuffer-line-migemo
               "This will be removed in the future."
               "20161229")

(defun ivy--highlight-default-migemo (str)
  "The same as `ivy--highlight-default' except for using migemo."
  (unless ivy--old-re
    (setq ivy--old-re (funcall ivy--regex-function ivy-text)))
  (let ((fuzzy-p (eq ivy--highlight-function 'ivy--highlight-fuzzy))
        (start
         (if (and (memq (ivy-state-caller ivy-last)
                        '(counsel-git-grep counsel-ag counsel-rg counsel-pt
                          ;; Add migemo version
                          counsel-pt-migemo counsel-rg-migemo))
                  (string-match "^[^:]+:[^:]+:" str))
             (match-end 0)
           0)))
    (ignore-errors
      (while (and (string-match ivy--old-re str start)
                  (> (- (match-end 0) (match-beginning 0)) 0))
        (setq start (match-end 0))
        ;; Adapt for migemo's regexp
        (cl-loop
         with i-face = (if fuzzy-p 1 0)
         with c-mbeg = nil
         with l-mend = 0
         for i from (if fuzzy-p 1 0) below (if (zerop ivy--subexps) 1
                                             (/ (length (match-data)) 2))
         when (>= l-mend start) return nil
         for mbeg = (or c-mbeg (match-beginning i))
         for mend = (match-end i)
         when (and mbeg mend (<= l-mend mbeg)) do
         (setq c-mbeg (or c-mbeg mbeg))
         (unless (and (match-beginning (1+ i))
                      (= mend (match-beginning (1+ i))))
           (let ((face
                  (cond ((zerop ivy--subexps)
                         (cadr ivy-minibuffer-faces))
                        ((zerop i)
                         (car ivy-minibuffer-faces))
                        (t
                         (nth (1+ (mod (+ i-face 2)
                                       (1- (length ivy-minibuffer-faces))))
                              ivy-minibuffer-faces)))))
             (ivy-add-face-text-property
              mbeg (if (> i 0) (setq l-mend mend) mend) face str)
             (setq c-mbeg nil)
             (cl-incf i-face)))))))
  str)
(byte-compile 'ivy--highlight-default-migemo)

(defun ivy-occur-revert-buffer-migemo ()
  "The same as `ivy-occur-revert-buffer'.
except for adding counsel-pt-migemo, counsel-rg-migemo."
  (interactive)
  (let ((caller (ivy-state-caller ivy-occur-last))
        (ivy-last ivy-occur-last))
    (cond ((eq caller 'swiper)
           (let ((buffer (ivy-state-buffer ivy-occur-last)))
             (unless (buffer-live-p buffer)
               (error "Buffer was killed"))
             (let ((inhibit-read-only t))
               (erase-buffer)
               (funcall (plist-get ivy--occurs-list caller) t)
               (ivy-occur-grep-mode))))
          ((memq caller '(counsel-git-grep counsel-grep counsel-ag counsel-rg
                                           ;; Add migemo version
                                           counsel-pt-migemo counsel-rg-migemo))
           (let ((inhibit-read-only t))
             (erase-buffer)
             (funcall (plist-get ivy--occurs-list caller)))))
    (setq ivy-occur-last ivy-last)))
(byte-compile 'ivy-occur-revert-buffer-migemo)

(defun ivy-occur-press-migemo ()
  "The same as `ivy-occur-press' except for adding counsel-pt-migemo, counsel-rg-migemo."
  (interactive)
  (when (save-excursion
          (beginning-of-line)
          (looking-at "\\(?:./\\|    \\)\\(.*\\)$"))
    (when (memq (ivy-state-caller ivy-occur-last)
                '(swiper counsel-git-grep counsel-grep counsel-ag counsel-rg
                  counsel-describe-function counsel-describe-variable
                  ;; Add migemo version
                  counsel-pt-migemo counsel-rg-migemo))
      (let ((window (ivy-state-window ivy-occur-last)))
        (when (or (null (window-live-p window))
                  (equal window (selected-window)))
          (save-selected-window
            (setf (ivy-state-window ivy-occur-last)
                  (display-buffer (ivy-state-buffer ivy-occur-last)
                                  'display-buffer-pop-up-window))))))
    (let* ((ivy-last ivy-occur-last)
           (ivy-text (ivy-state-text ivy-last))
           (str (buffer-substring
                 (match-beginning 1)
                 (match-end 1)))
           (coll (ivy-state-collection ivy-last))
           (action (ivy--get-action ivy-last))
           (ivy-exit 'done))
      (with-ivy-window
        (when (boundp 'counsel-grep-last-line)
          (setq counsel-grep-last-line nil))
        (with-current-buffer (ivy-state-buffer ivy-last)
          (funcall action
                   (if (and (consp coll)
                            (consp (car coll)))
                       (assoc str coll)
                     str)))
        (if (memq (ivy-state-caller ivy-last)
                  '(swiper counsel-git-grep counsel-grep))
            (with-current-buffer (window-buffer (selected-window))
              (swiper--cleanup)
              (swiper--add-overlays
               (ivy--regex ivy-text)
               (line-beginning-position)
               (line-end-position)
               (selected-window))
              (when (timerp ivy-occur-timer)
                (cancel-timer ivy-occur-timer))
              (setq ivy-occur-timer (run-at-time 1.0 nil 'swiper--cleanup))))))))
(byte-compile 'ivy-occur-press-migemo)

;; For using with avy-migemo-mode
(avy-migemo-remove-names 'ivy--regex-migemo ; Obsolete
                         'ivy--format-minibuffer-line-migemo
                         '(ivy--regex-ignore-order--part
                           :around
                           ivy--regex-ignore-order--part-migemo-around))
(avy-migemo-add-names '(ivy--regex :around ivy--regex-migemo-around)
                      '(ivy--regex-or-literal
                        :around
                        ivy--regex-or-literal-migemo-around)
                      '(ivy--regex-plus :around ivy--regex-plus-migemo-around)
                      'ivy--highlight-default-migemo
                      'ivy-occur-revert-buffer-migemo
                      'ivy-occur-press-migemo)
(add-hook 'avy-migemo-regex-cache-clear-hook
          'avy-migemo--ivy--regex-hash-clear)

(provide 'avy-migemo-e.g.ivy)
;;; avy-migemo-e.g.ivy.el ends here
