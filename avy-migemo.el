;;; avy-migemo.el --- avy with migemo -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 momomo5717

;; Keywords: avy, migemo
;; Version: 0.3.2
;; Package-Requires:((emacs "24.4") (avy "0.4.0") (migemo "1.9"))
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

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a minor mode of avy for using migemo.
;;
;; Migemo which is a library for incremental search has Japanese dictionaries.
;; It could be used as abbreviation matching for other languages
;; by preparing user's migemo dictionaries or customizing `avy-migemo-get-function'.
;;
;; `avy-migemo-get-function' can also use `char-fold-to-regexp' as below.
;; (setq avy-migemo-get-function 'char-fold-to-regexp)
;;
;;
;; The following functions are provided:
;;
;;   + avy-migemo-goto-char
;;   + avy-migemo-goto-char-2
;;   + avy-migemo-goto-char-in-line
;;   + avy-migemo-goto-char-timer
;;   + avy-migemo-goto-subword-1
;;   + avy-migemo-goto-word-1
;;   + avy-migemo-isearch
;;   + avy-migemo-org-goto-heading-timer
;;   + avy-migemo--overlay-at
;;   + avy-migemo--overlay-at-full
;;   + avy-migemo--read-candidates
;;
;;  These are the same as avy's predefined functions
;;  except for adding candidates via migemo (simply using migemo instead of `regexp-quote').
;;
;; The following extensions are available:
;;
;;   + avy-migemo-e.g.zzz-to-char.el
;;   + avy-migemo-e.g.ivy.el
;;   + avy-migemo-e.g.swiper.el
;;   + avy-migemo-e.g.counsel.el
;;
;; Further information is available from:
;; https://github.com/momomo5717/avy-migemo  (README.org or README.jp.org)

;; Setup:
;; (add-to-list 'load-path "/path/to/avy-migemo")
;; (require 'avy-migemo)
;; ;; `avy-migemo-mode' overrides avy's predefined functions using `advice-add'.
;; (avy-migemo-mode 1)
;; (global-set-key (kbd "M-g m m") 'avy-migemo-mode)
;;
;; ;; If you would like to restrict the length of displayed keys within 2
;; ;; for `avy-style' of at-full, `avy-migemo-at-full-max' provides it.
;; (custom-set-variables '(avy-migemo-at-full-max 2))

;;; Code:
(require 'avy)
(require 'migemo)
(require 'cl-lib)

(defgroup avy-migemo nil
  "avy with migemo."
  :group  'avy
  :prefix "avy-migemo-")

(defcustom avy-migemo-lighter nil
  "Lighter for `avy-migemo-mode'."
  :type '(choice (const :tag "Not displayed." nil)
                 string))

(defcustom avy-migemo-get-function 'migemo-get-pattern
  "Function which takes a string and returns a regular expression."
  :type '(choice (const :tag "Use `migemo-get-pattern'" migemo-get-pattern)
                 (const :tag "Use `avy-migemo-get-pattern-non-capturing'"
                        avy-migemo-get-pattern-non-capturing)
                 (const :tag "Use `char-fold-to-regexp'" char-fold-to-regexp)
                 function))

(defcustom avy-migemo-regex-concat-use-non-capturing nil
  "Use non-capturing group for `avy-migemo-regex-concat'/`avy-migemo-regex-quote-concat'."
  :type 'boolean)

(defcustom avy-migemo-function-names
  '(avy-migemo-goto-char
    avy-migemo-goto-char-2
    avy-migemo-goto-char-in-line
    avy-migemo-goto-char-timer
    avy-migemo-goto-subword-1
    avy-migemo-goto-word-1
    avy-migemo-isearch
    avy-migemo-org-goto-heading-timer
    avy-migemo--overlay-at
    avy-migemo--overlay-at-full)
  "Function names for overriding avy's functions.
\(orig-fn where advice-fn) like args of `advice-add' is also available."
  :type '(repeat (choice symbol
                         (list symbol symbol symbol)))
  :set (lambda (symbol value)
         (if (and (boundp symbol) (boundp 'avy-migemo-mode))
             (let ((avy-migemo-mode-p avy-migemo-mode))
               (ignore-errors (when avy-migemo-mode-p (avy-migemo-mode -1)))
               (set symbol value)
               (ignore-errors (when avy-migemo-mode-p (avy-migemo-mode 1))))
           (set symbol value))))

(defcustom avy-migemo-regex-cache-clear-hook nil
  "Normal hook run at the end of `avy-migemo-regex-cache-clear'."
  :type 'hook)

;;;###autoload
(defun avy-migemo-add-names (&rest names)
  "Add NAMES to the front of `avy-migemo-function-names'."
  (let ((names (nconc (cl-loop for name in (cl-delete-duplicates names)
                               unless (member name avy-migemo-function-names)
                               collect name)
                      avy-migemo-function-names)))
    (custom-set-variables `(avy-migemo-function-names ',names))
    avy-migemo-function-names))

;;;###autoload
(defun avy-migemo-remove-names (&rest names)
  "Remove NAMES from `avy-migemo-function-names'."
  (let ((names (cl-loop for name in avy-migemo-function-names
                        unless (member name names)
                        collect name)))
    (custom-set-variables `(avy-migemo-function-names ',names))
    avy-migemo-function-names))

;;;###autoload
(define-minor-mode avy-migemo-mode
  "Override avy's functions."
  :global t
  :lighter avy-migemo-lighter
  (cl-loop for e in avy-migemo-function-names
           for name = (if (listp e) (cl-first e) e)
           for where = (if (listp e) (cl-second e) :override)
           for function = (when (listp e) (cl-third e))
           for predefined-name = (if function name
                                   (intern (replace-regexp-in-string
                                            "-migemo" "" (symbol-name name))))
           do
           (when (and function (consp function)) (setq function (cadr function)))
           (if avy-migemo-mode
               (unless (if function (eq predefined-name function)
                         (eq predefined-name name))
                 (advice-add predefined-name where (or function name)))
             (advice-remove predefined-name (or function name)))))

;;;###autoload
(defun avy-migemo-disable-around (orig-f &rest orig-args)
  "Advice for a function incompatible with `avy-migemo-mode'.
e.g. \(advice-add 'counsel-clj :around #'avy-migemo-disable-around\)"
  (if (not avy-migemo-mode)
      (apply orig-f orig-args)
    (avy-migemo-mode -1)
    (unwind-protect
        (apply orig-f orig-args)
      (avy-migemo-mode 1))))

(defvar avy-migemo--regex-cache
  (make-hash-table :test #'equal)
  "Migemo's regexp cache.")

(defvar avy-migemo--regex-quote-cache
  (make-hash-table :test #'equal)
  "Migemo's regexp quote cache.")

(defvar avy-migemo--regex-nnl-cache
  (make-hash-table :test #'equal)
  "Migemo's regexp cache for nonnewline.")

(defvar avy-migemo--regex-quote-nnl-cache
  (make-hash-table :test #'equal)
  "Migemo's regexp quote cache for nonnewline.")

;;;###autoload
(defun avy-migemo-regex-cache-clear ()
  "Clear `avy-migemo--regex-cache'."
  (interactive)
  (clrhash avy-migemo--regex-cache)
  (clrhash avy-migemo--regex-quote-cache)
  (clrhash avy-migemo--regex-nnl-cache)
  (clrhash avy-migemo--regex-quote-nnl-cache)
  (run-hooks 'avy-migemo-regex-cache-clear-hook))

;;;###autoload
(defun avy-migemo-regex-p (regex)
  "Retrun nil if REGEX is invalid."
  (ignore-errors
    (string-match regex "")
    regex))

(defun avy-migemo--rep-wspace-re (regexp)
  "Replace \\s-* with empty string in REGEXP."
  (replace-regexp-in-string "\\\\s-\\*" "" regexp))

(defun avy-migemo--rep-group (regexp)
  "Replace group with non-captureing group in REGEXP."
  (replace-regexp-in-string "\\\\\\((\\)[^?]" "(?:" regexp nil nil 1))

(defun avy-migemo-get-pattern-non-capturing (word)
  "Retrun a regexp from WORD via `migemo-get-pattern'.
The regxp's group will be repcaled with non-captureing group."
  (avy-migemo--rep-group (migemo-get-pattern word)))

;;;###autoload
(defun avy-migemo-regex-concat (pattern &optional nnl-p)
  "Return migemo's regexp which includes PATTERN in last place.
Return PATTERN if migemo's regexp is invalid.
Return quoted PATTERN if PATTERN is invalid.
If NNL-P is non-nil, replace \\s-* on migemo's regexp with empty string."
  (let ((cache (unless nnl-p (gethash pattern avy-migemo--regex-cache))))
    (if cache cache
      (let ((re (let* ((non-cap-p (and avy-migemo-regex-concat-use-non-capturing "?:"))
                       (mre (funcall avy-migemo-get-function pattern))
                       (regex (if nnl-p (avy-migemo--rep-wspace-re mre) mre))
                       (regex-p (avy-migemo-regex-p regex))
                       (pattern-p (avy-migemo-regex-p pattern)))
                  (cond ((and regex-p pattern-p)
                         (concat "\\(" non-cap-p regex "\\|" pattern "\\)"))
                        (regex-p
                         (concat "\\(" non-cap-p (regexp-quote pattern) "\\|" regex "\\)"))
                        (pattern-p pattern)
                        (t (regexp-quote pattern))))))
        (if nnl-p re (puthash pattern re avy-migemo--regex-cache))))))

;;;###autoload
(defun avy-migemo-regex-quote-concat (pattern &optional nnl-p)
  "Return migemo's regexp which includes quoted PATTERN in last place.
Return quoted PATTERN if migemo's regexp is invalid.
If NNL-P is non-nil, replace \\s-* on migemo's regexp with empty string."
  (let ((cache (unless nnl-p (gethash pattern avy-migemo--regex-quote-cache))))
    (if cache cache
      (let ((re (let* ((non-cap-p (and avy-migemo-regex-concat-use-non-capturing "?:"))
                       (mre (funcall avy-migemo-get-function pattern))
                       (regex (if nnl-p (avy-migemo--rep-wspace-re mre) mre)))
                  (if (avy-migemo-regex-p regex)
                      (concat "\\(" non-cap-p regex "\\|" (regexp-quote pattern) "\\)")
                    (regexp-quote pattern)))))
        (if nnl-p re (puthash pattern re avy-migemo--regex-quote-cache))))))

;;;###autoload
(defun avy-migemo-regex-concat-nnl (pattern)
  "Return migemo's regexp which includes PATTERN with nonnewline.
Replace \\s-* on migemo's regexp with empty string."
  (let ((cache (gethash pattern avy-migemo--regex-nnl-cache)))
    (if cache cache
      (puthash pattern (avy-migemo-regex-concat pattern t)
               avy-migemo--regex-nnl-cache))))

;;;###autoload
(defun avy-migemo-regex-quote-concat-nnl (pattern)
  "Return migemo's regexp which includes quoted PATTERN with nonnewline.
Replace \\s-* on migemo's regexp with empty string."
  (let ((cache (gethash pattern avy-migemo--regex-quote-nnl-cache)))
    (if cache cache
      (puthash pattern (avy-migemo-regex-quote-concat pattern t)
               avy-migemo--regex-quote-nnl-cache))))

;; avy functions for migemo

(defcustom avy-migemo-at-full-max nil
  "Max length of keys."
  :type '(choice (integer :tag "Restrict the length of displayed keys for `avy-style' of at-full.")
                 (const :tag "Disabled" nil)))

(defcustom avy-migemo-pad-char (string-to-char " ")
  "Pad char."
  :type 'character)

(defcustom avy-migemo-pad-char-visual-line-mode ?_
  "Pad char for `visual-line-mode'."
  :type 'character)

(defvar avy-migemo--pad-style nil)

(defun avy-migemo--pad-char (&optional style)
  "Return a pad character of STYLE."
  (cl-case (or style avy-migemo--pad-style)
    (visual-line-mode avy-migemo-pad-char-visual-line-mode)
    (otherwise avy-migemo-pad-char)))

(defun avy-migemo--pad-string (c n)
  "Retrun a pad string of C of length N."
  (let ((pad-str (make-string n c)))
    (if (or avy-background (not (eq c ? )))
        (propertize pad-str 'face 'avy-background-face)
      pad-str)))

(defun avy-mgiemo--truncate-str-width (str width)
  "Truncate STR to WIDTH."
  (substring str 0
             (cl-loop with s-to = 0
                      for c across str
                      if (> width 0) do
                      (cl-decf width (char-width c))
                      (cl-incf s-to)
                      else return s-to
                      finally return s-to)))

(defun avy-migemo--rest-old-str (old-str+ len)
  "Return the padded string of a part of OLD-STR+.
LEN is compared with string width of OLD-STR+."
  (cl-loop
   with old-ls = (string-to-list old-str+)
   with pre-width  = 0
   with char-count = 0
   until (zerop len) do
   (cond
    ((and (zerop char-count) (car old-ls))
     (setq pre-width (max (char-width (pop old-ls)) 1))
     (cl-decf len)
     (cl-incf char-count))
    ((> pre-width char-count)
     (cl-decf len)
     (cl-incf char-count))
    ((null old-ls)
     (setq len 0 pre-width 0 char-count 0))
    (t (setq char-count 0)))
   finally return
   (concat (avy-migemo--pad-string (avy-migemo--pad-char)
                                   (- pre-width char-count))
           old-ls)))

(defun avy-migemo--overlay-at (path leaf)
  "The same as `avy--overlay-at' except adapting it for migemo."
  (let* ((path (mapcar #'avy--key-to-char path))
         (str (propertize
               (string (car (last path)))
               'face 'avy-lead-face))
         (avy-migemo--pad-style
          (when (with-selected-window (avy-candidate-wnd leaf)
                  (bound-and-true-p visual-line-mode))
            'visual-line-mode)))
    (avy--overlay
     str
     (avy-candidate-beg leaf) nil
     (avy-candidate-wnd leaf)
     (lambda (str old-str)
       (cond ((string= old-str "\n")
              (concat str "\n"))
             ;; Adapt for migemo
             ((= (max (string-width old-str) 1) 1)
              str)
             (t
              (concat str (avy-migemo--rest-old-str old-str 1))))))))

;; Dynamically bound in `avy-migemo--overlay-at-full'.
(defvar avy-migemo--vbeg)
(defvar avy-migemo--vend)
(defvar avy-migemo--vend-pre)
(defvar avy-migemo--vwbeg)
(defvar avy-migemo--visual-line-mode-p)

(defun avy-migemo--ovl-vbeg (ovl)
  "Return the beginning position of the visual line of OVL."
  (overlay-get ovl 'avy-migemo--vbeg))

(defun avy-migemo--ovl-vend (ovl)
  "Return the end position of the visual line of OVL."
  (overlay-get ovl 'avy-migemo--vend))

(defun avy-migemo--ovl-vwnd (ovl)
  "Return the window of OVL."
  (overlay-get ovl 'window))

(defun avy-migemo--ovl-vwbeg (ovl)
  "Return the beginning position of the window of OVL."
  (overlay-get ovl 'avy-migemo--vwbeg))

(defun avy-migemo--overlay-at-full-vend-position (beg)
  "Return the point of the end of the visual line for `visual-line-mode'.

Set variables for distinguish the beginning position of the visual line."
  (let* ((ovl (car-safe avy--overlays-lead))
         (vbeg (when ovl (avy-migemo--ovl-vbeg ovl)))
         (vend (when ovl (avy-migemo--ovl-vend ovl)))
         (vwnd (when ovl (avy-migemo--ovl-vwnd ovl)))
         (vwbeg (when ovl (avy-migemo--ovl-vwbeg ovl))))
    (setq avy-migemo--visual-line-mode-p t)
    (setq avy-migemo--vend-pre nil)
    (save-excursion
      (unless (eq beg (point)) (goto-char beg))
      (prog2
          (end-of-visual-line)
          (setq avy-migemo--vend
                (cond
                 ((<= beg (point)) (point))
                 ((and vbeg vend
                       (eq vwnd (selected-window))
                       (<= vbeg (point) vend))
                  vend)
                 (t (line-end-position))))
        (if (and (eq (selected-window) vwnd)
                 (eq avy-migemo--vend vend))
            (setq avy-migemo--vbeg vbeg
                  avy-migemo--vwbeg vwbeg)
          (goto-char beg)
          (beginning-of-visual-line)
          (setq avy-migemo--vbeg (point)
                avy-migemo--vwbeg (window-start)))
        (when (and (eq beg avy-migemo--vbeg)
                   (> beg avy-migemo--vwbeg))
          (goto-char (1- beg))
          (end-of-visual-line)
          (setq avy-migemo--vend-pre (point)))))))

(defun avy-migemo--overlay-at-full-vpre-space (beg)
  "Return one space for `visual-line-mode'.

if BEG is equal to `avy-migemo--vend-pre'."
  (if (and (eq avy-style 'at-full)
           avy-migemo--vend-pre
           (eq beg avy-migemo--vend-pre))
      " " ""))

(defun avy-migemo--overlay-at-full-vlen (beg len str)
  "Restrict len for `visual-line-mode' via BEG LEN STR.

Also restrict LEN if `avy-migemo-at-full-max' is an integer,"
  (if (eq avy-style 'at-full)
      (let ((len (cond ((and avy-migemo--visual-line-mode-p
                             (eq beg avy-migemo--vend))
                        (char-width (aref str 0)))
                       ((and avy-migemo--visual-line-mode-p
                             (> beg avy-migemo--vend))
                        (min len (- beg avy-migemo--vend)))
                       (t len))))
        (if (integerp avy-migemo-at-full-max)
            (min len avy-migemo-at-full-max) len))
    len))

(defun avy-migemo--overlay-at-full-concat (str old-str+ beg len)
  "Return a string.

STR / OLD-STR+ is a string.
BEG / LEN is an integer."
  (let* ((other-char-p
          (unless (eq avy-style 'words)
            (cl-loop for c across str
                     for i from 0
                     unless (or (memq c avy-keys) (eq c ? ))
                     return i)))
         (str (if other-char-p
                  (substring str 0 other-char-p) str))
         (len (if other-char-p (length str) len))
         (vlen (avy-migemo--overlay-at-full-vlen beg len str))
         (vstr (if (eq len vlen) str (substring str 0 vlen)))
         (avy-migemo--pad-style
          (when avy-migemo--visual-line-mode-p 'visual-line-mode)))
    (concat vstr (avy-migemo--rest-old-str old-str+ vlen))))

(defun avy-migemo--overlay-at-full-add-vpoint (ovl-list)
  "Add point for distinguish the beg/end of visual line."
  (when avy-migemo--visual-line-mode-p
    (let* ((ovl (car ovl-list))
           (pre-space (avy-migemo--overlay-at-full-vpre-space (overlay-start ovl))))
      (overlay-put ovl 'avy-migemo--vbeg avy-migemo--vbeg)
      (overlay-put ovl 'avy-migemo--vend avy-migemo--vend)
      (overlay-put ovl 'avy-migemo--vwbeg avy-migemo--vwbeg)
      (unless (string= pre-space "")
        ;; One space is added at the beginning position of the visual line
        ;; for keeping layout as much as possible.
        (overlay-put ovl 'before-string pre-space))))
  ovl-list)

(defun avy-migemo--overlay-at-full (path leaf)
  "The same as `avy--overlay-at-full' except adapting it for migemo."
  (let* ((path (mapcar #'avy--key-to-char path))
         (str (propertize
               (apply #'string (reverse path))
               'face 'avy-lead-face))
         (len (length path))
         (beg (avy-candidate-beg leaf))
         (wnd (cdr leaf))
         end
         ;; Adapt for migemo
         avy-migemo--old-str+
         avy-migemo--visual-line-mode-p
         avy-migemo--vbeg  avy-migemo--vend
         avy-migemo--vwbeg)
    (dotimes (i len)
      (set-text-properties (- len i 1) (- len i)
                           `(face ,(nth i avy-lead-faces))
                           str))
    (when (eq avy-style 'de-bruijn)
      (setq str (concat
                 (propertize avy-current-path
                             'face 'avy-lead-face-1)
                 str))
      (setq len (length str)))
    (with-selected-window wnd
      (save-excursion
        (goto-char beg)
        (let* ((lep (if (bound-and-true-p visual-line-mode)
                        (avy-migemo--overlay-at-full-vend-position beg)
                      (line-end-position)))
               (len-and-str (avy--update-offset-and-str len str lep)))
          (setq len (car len-and-str))
          (setq str (cdr len-and-str))
          (setq end (if (= beg lep)
                        (1+ beg)
                      (min (+ beg
                              (if (eq (char-after) ?\t)
                                  1
                                len))
                           lep)))
          (setq avy-migemo--old-str+
                (avy-mgiemo--truncate-str-width
                 (buffer-substring beg (min end (point-max)))
                 (length str)))
          (when (and (bound-and-true-p visual-line-mode)
                     (> len (- end beg))
                     (not (eq lep beg)))
            (setq len (- end beg))
            (let ((old-str (apply #'string (reverse path))))
              (setq str
                    (substring
                     (propertize
                      old-str
                      'face
                      (if (= (length old-str) 1)
                          'avy-lead-face
                        'avy-lead-face-0))
                     0 len)))))))
    (setq end (min (+ beg (length avy-migemo--old-str+)) end))
    (avy-migemo--overlay-at-full-add-vpoint
     (avy--overlay
      str beg end wnd
      (lambda (str old-str)
        (cond ((string= old-str "\n")
               (concat str "\n"))
              ((string= old-str "\t")
               (concat str (make-string (max (- tab-width len) 0) ?\ )))
              (t
               ;; Adapt for migemo
               (avy-migemo--overlay-at-full-concat str avy-migemo--old-str+ beg len))))))))

;;;###autoload
(defun avy-migemo-goto-char (char &optional arg)
  "The same as `avy-migemo-goto-char' except for the candidates via migemo."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-char
    (avy--generic-jump
     (if (= 13 char)
         "\n"
       ;; Adapt for migemo
       (avy-migemo-regex-quote-concat (string char)))
     arg
     avy-style)))

;;;###autoload
(defun avy-migemo-goto-char-2 (char1 char2 &optional arg beg end)
  "The same as `avy-goto-char-2' except for the candidates via migemo."
  (interactive (list (read-char "char 1: " t)
                     (read-char "char 2: " t)
                     current-prefix-arg
                     nil nil))
  (when (eq char1 ?)
    (setq char1 ?\n))
  (when (eq char2 ?)
    (setq char2 ?\n))
  (avy-with avy-goto-char-2
    (avy--generic-jump
     ;; Adapt for migemo
     (if (eq char1 ?\n)
         (concat (string char1) (avy-migemo-regex-quote-concat (string char2)))
       (avy-migemo-regex-quote-concat (string char1 char2)))
     arg
     avy-style
     beg end)))

;;;###autoload
(defun avy-migemo-goto-char-in-line (char)
  "The same as `avy-goto-char-in-line' except for the candidates via migemo."
  (interactive (list (read-char "char: " t)))
  (avy-with avy-goto-char
    (avy--generic-jump
     ;; Adapt for migemo
     (avy-migemo-regex-quote-concat (string char))
     avy-all-windows
     avy-style
     (line-beginning-position)
     (line-end-position))))

(defun avy-migemo--read-candidates (&optional re-builder group)
  "The same as `avy--read-candidates' except for the candidates via migemo.
Default RE-BUILDER is `avy-migemo-regex-quote-concat'.
If GROUP is no-nil, the group will be highlighted. Default value is 0."
  (let ((str "")
        (re-builder (or re-builder #'avy-migemo-regex-quote-concat))
        (group (or group 0))
        char break overlays regex)
    (unwind-protect
        (progn
          (while (and (not break)
                      (setq char
                            (read-char (format "char%s: "
                                               (if (string= str "")
                                                   str
                                                 (format " (%s)" str)))
                                       t
                                       (and (not (string= str ""))
                                            avy-timeout-seconds))))
            ;; Unhighlight
            (dolist (ov overlays)
              (delete-overlay ov))
            (setq overlays nil)
            (cond
             ;; Handle RET
             ((= char 13)
              (if avy-enter-times-out
                  (setq break t)
                (setq str (concat str (list ?\n)))))
             ;; Handle C-h, DEL
             ((memq char '(8 127))
              (let ((l (length str)))
                (when (>= l 1)
                  (setq str (substring str 0 (1- l))))))
             (t
              (setq str (concat str (list char)))))
            ;; Highlight
            (when (>= (length str) 1)
              ;; Adapt for migemo
              (setq regex (funcall re-builder str))
              (let ((case-fold-search
                     (or avy-case-fold-search (string= str (downcase str))))
                    found)
                (avy-dowindows current-prefix-arg
                  (dolist (pair (avy--find-visible-regions
                                 (window-start)
                                 (window-end (selected-window) t)))
                    (save-excursion
                      (goto-char (car pair))
                      (while (re-search-forward regex (cdr pair) t)
                        (unless (get-char-property (1- (point)) 'invisible)
                          (let ((ov (make-overlay
                                     (match-beginning group)
                                     (match-end group))))
                            (setq found t)
                            (push ov overlays)
                            (overlay-put ov 'window (selected-window))
                            (overlay-put ov 'face 'avy-goto-char-timer-face)))))))
                ;; No matches at all, so there's surely a typo in the input.
                (unless found (beep)))))
          (nreverse (mapcar (lambda (ov)
                              (cons (cons (overlay-start ov)
                                          (overlay-end ov))
                                    (overlay-get ov 'window)))
                            overlays)))
      (dolist (ov overlays)
        (delete-overlay ov)))))

;;;###autoload
(defun avy-migemo-goto-char-timer (&optional arg)
  "The same as `avy-goto-char-timer' except for the candidates via migemo."
  (interactive "P")
  (let ((avy-all-windows (if arg
                             (not avy-all-windows)
                           avy-all-windows)))
    (avy-with avy-goto-char-timer
      (avy--process
       ;; Adapt for migemo
       (avy-migemo--read-candidates)
       (avy--style-fn avy-style)))))

;;;###autoload
(defun avy-migemo-goto-subword-1 (char &optional arg)
  "The same as `avy-goto-subword-1' except for the candidates via migemo."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-subword-1
    (let ((char (downcase char))
          ;; Adapt for migemo
          (regex (avy-migemo-regex-quote-concat (string char))))
      (avy-goto-subword-0
       arg (lambda ()
             (let ((char-after (char-after))) ; Adapt for migemo
               (and char-after
                    (or (eq (downcase char-after) char)
                        (string-match-p regex (string char-after))))))))))

;;;###autoload
(defun avy-migemo-goto-word-1 (char &optional arg beg end symbol)
  "The same as `avy-goto-word-1' except for the candidates via migemo."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-word-1
    (let* ((str (string char))
           (regex (cond ((string= str ".")
                         "\\.")
                        ((and avy-word-punc-regexp
                              (string-match avy-word-punc-regexp str))
                         ;; Adapt for migemo
                         (avy-migemo-regex-quote-concat str))
                        ((<= char 26) str)
                        (symbol (concat "\\_<" str))
                        (t ;; Adapt for migemo
                         (concat
                          "\\b"
                          (avy-migemo-regex-concat str))))))
      (avy--generic-jump regex arg avy-style beg end))))

(defcustom avy-migemo-use-isearch-search-fun nil
  "If non-nil, `avy-migemo-isearch' uses `isearch-search-fun'."
  :type 'boolean)

(defun avy-migemo--isearch-migemo-enabled-p ()
  "Return non-nil if migemo is enabled on isearch."
  (and migemo-isearch-enable-p
       (not (or (bound-and-true-p isearch-regexp-function)
                (bound-and-true-p isearch-word)))
       (not isearch-regexp)))

(defun avy-migemo--isearch-candidates (string &optional beg end pred group)
  "The same as `avy--regex-candidates' except for using `isearch-search-fun'."
  (setq group (or group 0))
  (unless (string= string "")
    (let* ((case-fold-search isearch-case-fold-search)
           (use-migemo-p (avy-migemo--isearch-migemo-enabled-p))
           (isearch-regexp (if use-migemo-p t isearch-regexp))
           (string (if use-migemo-p (migemo-search-pattern-get string) string))
           (search-fun (isearch-search-fun))
           candidates cpt)
      (avy-dowindows current-prefix-arg
        (cl-loop
         for (vbeg . vend)
         in (funcall (if isearch-forward #'identity #'nreverse)
                     (avy--find-visible-regions
                      (or beg (window-start))
                      (or end (window-end (selected-window) t))))
         for vstart = (if isearch-forward vbeg vend)
         for vbound = (if isearch-forward vend vbeg) do
         (save-excursion
           (setq cpt (goto-char vstart))
           (while (funcall search-fun string vbound t)
             (unless (get-char-property (- (point) (if isearch-forward 1 0)) 'invisible)
               (when (or (null pred) (funcall pred))
                 (push (cons (cons (match-beginning group)
                                   (match-end group))
                             wnd) candidates)))
             (when (eq cpt (point))
               (error "Point doesn't move: %s" search-fun))
             (setq cpt (point))))))
      (funcall (if isearch-forward #'nreverse #'identity) candidates))))

;;;###autoload
(defun avy-migemo-isearch ()
  "The same as `avy-isearch' except for the candidates via migemo."
  (interactive)
  (avy-with avy-isearch
    (let ((avy-background nil))
      ;; Adapt for migemo
      (avy--process (if avy-migemo-use-isearch-search-fun
                        (avy-migemo--isearch-candidates isearch-string)
                      (avy--regex-candidates
                       (avy-migemo-regex-concat isearch-string)))
                    (avy--style-fn avy-style))
      (isearch-done))))


(declare-function org-back-to-heading "org")

;;;###autoload
(defun avy-migemo-org-goto-heading-timer (&optional arg)
  "The same as `avy-org-goto-heading-timer' except for the candidates via migemo."
  (interactive "P")
  (let ((avy-all-windows (if arg
                             (not avy-all-windows)
                           avy-all-windows)))
    (avy-with avy-goto-char-timer
      (avy--process
       ;; Addapt for migemo
       (avy-migemo--read-candidates
        (lambda (input)
          (format "^\\*+ .*\\(%s\\)" (avy-migemo-regex-concat input)))
        1)
       (avy--style-fn avy-style))
      (org-back-to-heading))))

(provide 'avy-migemo)
;;; avy-migemo.el ends here
