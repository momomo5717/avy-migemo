;;; avy-migemo.el --- avy with migemo -*- lexical-binding: t -*-

;; Copyright (C) 2015 momomo5717

;; Keywords: avy, migemo
;; Version: 0.2.11
;; Package-Requires:((emacs "24.4") (avy "0.3") (migemo "1.9"))
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
;; For example, if url is defined in a migemo dictionary as ftp, http, and so on,
;; these words also can be added to avy's candidates.
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
;; (global-set-key (kbd "M-g m") 'avy-migemo-mode)
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

(defcustom avy-migemo-get-function 'migemo-search-pattern-get
  "Getter function of migemo.
It takes a string and returns a regular expression."
  :type 'function)

(defcustom avy-migemo-function-names
  '(avy-migemo-goto-char
    avy-migemo-goto-char-2
    avy-migemo-goto-char-in-line
    avy-migemo-goto-char-timer
    avy-migemo-goto-subword-1
    avy-migemo-goto-word-1
    avy-migemo-isearch
    avy-migemo--overlay-at
    avy-migemo--overlay-at-full)
  "Function names for overriding avy's functions."
  :set (lambda (symbol value)
         (if (and (boundp symbol) (boundp 'avy-migemo-mode))
             (let ((avy-migemo-mode-p avy-migemo-mode))
               (ignore-errors (when avy-migemo-mode-p (avy-migemo-mode -1)))
               (set symbol value)
               (ignore-errors (when avy-migemo-mode-p (avy-migemo-mode 1))))
           (set symbol value))))

;;;###autoload
(defun avy-migemo-add-names (&rest names)
  "Add NAMES to the front of `avy-migemo-function-names'."
  (let ((names (nconc (cl-loop for name in (cl-delete-duplicates names)
                               unless (memq name avy-migemo-function-names)
                               collect name)
                      avy-migemo-function-names)))
    (custom-set-variables `(avy-migemo-function-names ',names))
    avy-migemo-function-names))

;;;###autoload
(defun avy-migemo-remove-names (&rest names)
  "Remove NAMES from `avy-migemo-function-names'."
  (let ((names (cl-loop for name in avy-migemo-function-names
                        unless (memq name names)
                        collect name)))
    (custom-set-variables `(avy-migemo-function-names ',names))
    avy-migemo-function-names))

;;;###autoload
(define-minor-mode avy-migemo-mode
  "Override avy's functions."
  :global t
  (dolist (name avy-migemo-function-names)
    (let ((predefined-name
           (intern (replace-regexp-in-string
                    "-migemo" "" (symbol-name name)))))
      (if avy-migemo-mode
          (unless (eq predefined-name name)
           (advice-add predefined-name :override name))
        (advice-remove predefined-name name)))))

;;;###autoload
(defun avy-migemo-disable-around (orig-f &rest orig-args)
  "Advice for a function incompatible with `avy-migemo-mode'.
e.g. \(advice-add 'counsel-locate :around #'avy-migemo-disable-around\)"
  (if (not avy-migemo-mode)
      (apply orig-f orig-args)
    (avy-migemo-mode -1)
    (unwind-protect
        (apply orig-f orig-args)
      (avy-migemo-mode 1))))

(defvar avy-migemo--regex-cache
  (make-hash-table :test #'equal)
  "Migemo's regexp cache.")

;;;###autoload
(defun avy-migemo-regex-cache-clear ()
  "Clear `avy-migemo--regex-cache'."
  (interactive)
  (clrhash avy-migemo--regex-cache))

;;;###autoload
(defun avy-migemo-regex-p (regex)
  "Retrun nil if REGEX is invalid."
  (ignore-errors
    (string-match regex "")
    regex))

;;;###autoload
(defun avy-migemo-regex-concat (pattern)
  "Return migemo's regexp which includes PATTERN in last place.
Return PATTERN if migemo's regexp is invalid."
  (let ((cache (gethash pattern avy-migemo--regex-cache)))
    (if cache cache
      (puthash pattern
               (let ((regex (funcall avy-migemo-get-function pattern)))
                 (if (avy-migemo-regex-p regex)
                     (concat regex "\\|" pattern)
                   pattern))
               avy-migemo--regex-cache))))

;;;###autoload
(defun avy-migemo-regex-quote-concat (pattern)
  "Return migemo's regexp which includes quoted PATTERN in last place.
Return quoted PATTERN if migemo's regexp is invalid."
  (let* ((quoted-pattern (regexp-quote pattern))
         (cache (gethash quoted-pattern avy-migemo--regex-cache)))
    (if cache cache
      (puthash quoted-pattern
               (let ((regex (funcall avy-migemo-get-function pattern)))
                 (if (avy-migemo-regex-p regex)
                     (concat regex "\\|" quoted-pattern)
                   quoted-pattern))
               avy-migemo--regex-cache))))

;; avy functions for migemo

(defcustom avy-migemo-at-full-max nil
  "Max length of keys."
  :type '(choice (integer :tag "Restrict the length of displayed keys for `avy-style' of at-full.")
                 boolean))

(defcustom avy-migemo-padding-char (string-to-char " ")
  "Padding char."
  :type 'character)

(defcustom avy-migemo-padding-char-visual-line-mode ?_
  "Padding char for `visual-line-mode'."
  :type 'character)

(defvar avy-migemo--padding-style nil)

(defun avy-migemo--padding-char (&optional style)
  "Return a padding character of STYLE."
  (cl-case (or style avy-migemo--padding-style)
    (visual-line-mode avy-migemo-padding-char-visual-line-mode)
    (otherwise avy-migemo-padding-char)))

(defun avy-migemo--rest-old-str (old-str+ len)
  "Return a new character list which is a part of OLD-STR+.
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
   (nconc (make-list (- pre-width char-count) (avy-migemo--padding-char)) old-ls)))

(defun avy-migemo--overlay-at (path leaf)
  "The same as `avy--overlay-at' except adapting it for migemo."
  (let* ((path (mapcar #'avy--key-to-char path))
         (str (propertize
               (string (car (last path)))
               'face 'avy-lead-face))
         (avy-migemo--padding-style
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
          (setq avy-migemo--vend (point))
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
          (cl-loop for c across str
                   for i from 0
                   unless (or (memq c avy-keys) (eq c ? ))
                   return i))
         (str (if other-char-p
                  (substring str 0 other-char-p) str))
         (len (if other-char-p (length str) len))
         (vlen (avy-migemo--overlay-at-full-vlen beg len str))
         (vstr (if (eq len vlen) str (substring str 0 vlen)))
         (avy-migemo--padding-style
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
          (setq avy-migemo--old-str+ (buffer-substring beg end))
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
(defun avy-migemo-goto-char-2 (char1 char2 &optional arg)
  "The same as `avy-goto-char-2' except for the candidates via migemo."
  (interactive (list (read-char "char 1: " t)
                     (read-char "char 2: " t)
                     current-prefix-arg))
  (avy-with avy-goto-char-2
    (avy--generic-jump
     ;; Adapt for migemo
     (avy-migemo-regex-quote-concat (string char1 char2))
     arg
     avy-style)))

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

(defun avy-migemo--read-candidates ()
  "The same as `avy--read-candidates' except for the candidates via migemo."
  (let ((str "") char break overlays regex)
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
              (setq break t))
             ;; Handle DEL
             ((= char 127)
              (let ((l (length str)))
                (when (>= l 1)
                  (setq str (substring str 0 (1- l))))))
             (t
              (setq str (concat str (list char)))))
            ;; Highlight
            (when (>= (length str) 1)
              ;; Adapt for migemo
              (setq regex (avy-migemo-regex-quote-concat str))
              (let ((case-fold-search
                     (or avy-case-fold-search (string= str (downcase str))))
                    found)
                (avy-dowindows nil
                  (dolist (pair (avy--find-visible-regions
                                 (window-start)
                                 (window-end (selected-window) t)))
                    (save-excursion
                      (goto-char (car pair))
                      (while (re-search-forward regex (cdr pair) t)
                        (unless (get-char-property (1- (point)) 'invisible)
                          (let ((ov (make-overlay
                                     (match-beginning 0)
                                     (match-end 0))))
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
       arg (lambda () (let ((char-after (char-after))) ; Adapt for migemo
                    (or (eq (downcase char-after) char)
                        (string-match-p regex (string char-after)))))))))

;;;###autoload
(defun avy-migemo-goto-word-1 (char &optional arg)
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
                        (t ;; Adapt for migemo
                         (concat
                          "\\b"
                          (avy-migemo-regex-concat str))))))
      (avy--generic-jump regex arg avy-style))))

;;;###autoload
(defun avy-migemo-isearch ()
  "The same as `avy-isearch' except for the candidates via migemo."
  (interactive)
  (avy-with avy-isearch
    (let ((avy-background nil))
      (avy--process
       (avy--regex-candidates
        ;; Adapt for migemo
        (avy-migemo-regex-concat isearch-string))
       (avy--style-fn avy-style))
      (isearch-done))))

(define-obsolete-function-alias 'avy-migemo-check-regex
  'avy-migemo-regex-p "0.2.5")

(define-obsolete-function-alias 'avy-migemo--read-string-timer
  'avy-migemo--read-candidates "0.2.9")
(provide 'avy-migemo)
;;; avy-migemo.el ends here
