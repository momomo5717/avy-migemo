;;; avy-migemo.el --- avy with migemo -*- lexical-binding: t -*-

;; Copyright (C) 2015 momomo5717

;; Keywords: avy, migemo
;; Version: 0.2.2
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
;; Migemo which is a library for incremental search has only Japanese dictionaries.
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
;;   + avy-migemo--read-string-timer
;;
;;  These are the same as avy's predefined functions
;;  except for adding candidates via migemo (simply using migemo instead of `regexp-quote').
;;
;; The following extensions are available:
;;   + avy-migemo-e.g.zzz-to-char.el
;;   + avy-migemo-e.g.swiper.el
;;
;; Further information is available from:
;; https://github.com/momomo5717/avy-migemo  (README.org or README.jp.org)

;; Setup:
;; (add-to-list 'load-path "/path/to/avy-migemo")
;; (require 'avy-migemo)
;; ;; If you override avy's predefined functions using `advice-add',
;; (avy-migemo-mode 1)
;;
;; ;; You can toggle `avy-migemo-mode' by M-x avy-migemo-mode.

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
    avy-migemo--overlay-at-full
    avy-migemo--read-string-timer)
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


;; avy functions for migemo

(defun avy-migemo--rest-old-str (old-str+ len)
  "Retrun a new character list which is a part of OLD-STR+.
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
   (nconc (make-list (- pre-width char-count) ? ) old-ls)))

(defun avy-migemo--overlay-at (path leaf)
  "The same as `avy--overlay-at' except adapting it for migemo."
  (let* ((path (mapcar #'avy--key-to-char path))
         (str (propertize
               (string (car (last path)))
               'face 'avy-lead-face))
         (pt (+ (if (consp (car leaf))
                    (caar leaf)
                  (car leaf))
                avy--overlay-offset))
         (wnd (cdr leaf))
         (ol (make-overlay pt (1+ pt)
                           (window-buffer wnd)))
         (old-str (with-selected-window wnd
                    (buffer-substring pt (1+ pt)))))
    (when avy-background
      (setq old-str (propertize
                     old-str 'face 'avy-background-face)))
    (overlay-put ol 'window wnd)
    (overlay-put ol 'display
                 (if (string= old-str "\n")
                     (concat str "\n")
                   ;; Adapt for migemo
                   (if (= (max (string-width old-str) 1) 1)
                       str
                     (concat str (avy-migemo--rest-old-str old-str 1)))))
    (push ol avy--overlays-lead)))

(defun avy-migemo--overlay-at-full (path leaf)
  "The same as `avy--overlay-at-full' except adapting it for migemo."
  (let* ((path (mapcar #'avy--key-to-char path))
         (str (propertize
               (apply #'string (reverse path))
               'face 'avy-lead-face))
         (len (length path))
         (beg (if (consp (car leaf))
                  (caar leaf)
                (car leaf)))
         (wnd (cdr leaf))
         oov)
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
        (when (setq oov
                    (delq nil
                          (mapcar
                           (lambda (o)
                             (and (eq (overlay-get o 'category) 'avy)
                                  (eq (overlay-get o 'window) wnd)
                                  (overlay-start o)))
                           (overlays-in (point) (min (+ (point) len)
                                                     (line-end-position))))))
          (setq len (- (apply #'min oov) beg))
          (setq str (substring str 0 len)))
        (let ((other-ov (cl-find-if
                         (lambda (o)
                           (and (eq (overlay-get o 'category) 'avy)
                                (eq (overlay-start o) beg)
                                (not (eq (overlay-get o 'window) wnd))))
                         (overlays-in (point) (min (+ (point) len)
                                                   (line-end-position))))))
          (when (and other-ov
                     (> (overlay-end other-ov)
                        (+ beg len)))
            (setq str (concat str (buffer-substring
                                   (+ beg len)
                                   (overlay-end other-ov))))
            (setq len (- (overlay-end other-ov)
                         beg))))
        (let* ((end (if (= beg (line-end-position))
                        (1+ beg)
                      (min (+ beg
                              (if (eq (char-after) ?\t)
                                  1
                                len))
                           (line-end-position))))
               (ol (make-overlay
                    beg end
                    (current-buffer)))
               (old-str (buffer-substring beg (1+ beg))))
          (overlay-put ol 'window wnd)
          (overlay-put ol 'category 'avy)
          (overlay-put ol 'display
                       (cond ((string= old-str "\n")
                              (concat str "\n"))
                             ((string= old-str "\t")
                              (concat str (make-string (max (- tab-width len) 0) ?\ )))
                             (t ;; Adapt for migemo
                              (let* ((other-char-p
                                      (cl-loop for c across str
                                               for i from 0
                                               unless (or (memq c avy-keys) (eq c ? ))
                                               return i))
                                     (str (if other-char-p
                                              (substring str 0 other-char-p) str))
                                     (len (if other-char-p (length str) len))
                                     (old-str+ (buffer-substring beg end)))
                                (if (= (string-width old-str+) len)
                                    str
                                  (concat str (avy-migemo--rest-old-str old-str+ len)))))))
          (push ol avy--overlays-lead))))))

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
       (funcall avy-migemo-get-function (string char)))
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
     (funcall avy-migemo-get-function (string char1 char2))
     arg
     avy-style)))

;;;###autoload
(defun avy-migemo-goto-char-in-line (char)
  "The same as `avy-goto-char-in-line' except for the candidates via migemo."
  (interactive (list (read-char "char: " t)))
  (avy-with avy-goto-char
    (avy--generic-jump
     ;; Adapt for migemo
     (funcall avy-migemo-get-function (string char))
     avy-all-windows
     avy-style
     (line-beginning-position)
     (line-end-position))))

(defun avy-migemo--read-string-timer ()
  "The same as `avy--read-string-timer' except for candidates via migemo."
  (let* ((str "")
         char break overlays regex we)
    (unwind-protect
        (progn
          (while (and (not break)
                      (setq char (read-char (format "char%s: "
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
              (setq regex (funcall avy-migemo-get-function str))
              (dolist (win (if avy-all-windows
                               (window-list)
                             (list (selected-window))))
                (with-selected-window win
                  (save-excursion
                    (setq we (window-end win t))
                    (goto-char (window-start))
                    (while (re-search-forward regex we t)
                      (unless (get-char-property (point) 'invisible)
                        (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                          (push ov overlays)
                          (overlay-put ov 'window win)
                          (overlay-put ov 'face 'avy-goto-char-timer-face)))))))))
          str)
      (dolist (ov overlays)
        (delete-overlay ov)))))

;;;###autoload
(defun avy-migemo-goto-char-timer (&optional arg)
  "The same as `avy-goto-char-timer' except for the candidates via migemo."
  (interactive "P")
  (let ((str (avy--read-string-timer)))
    (avy-with avy-goto-char-timer
      (avy--generic-jump
       ;; Adapt for migemo
       (funcall avy-migemo-get-function str)
       arg
       avy-style))))

;;;###autoload
(defun avy-migemo-goto-subword-1 (char &optional arg)
  "The same as `avy-goto-subword-1' except for the candidates via migemo."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-subword-1
    (let ((char (downcase char))
          ;; Adapt for migemo
          (regex (funcall avy-migemo-get-function (string char))))
      (avy-goto-subword-0
       arg (lambda () (let ((char-after (char-after)))
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
                         (funcall avy-migemo-get-function str))
                        (t ;; Adapt for migemo
                         (concat
                          "\\b"
                          (funcall avy-migemo-get-function str))))))
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
        (concat isearch-string "\\|"
                (funcall avy-migemo-get-function isearch-string)))
       (avy--style-fn avy-style))
      (isearch-done))))

(provide 'avy-migemo)
;;; avy-migemo.el ends here
