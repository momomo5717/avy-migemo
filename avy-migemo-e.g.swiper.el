;;; avy-migemo-e.g.swiper.el --- A setting example of avy-migemo for swiper -*- lexical-binding: t; no-byte-compile: t -*-

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

;; This file is a setting example of avy-migemo for swiper
;;
;; (require 'avy-migemo-e.g.swiper)
;;
;; ;; If you want to remove this config from `avy-migemo-function-names' in a init file,
;; (with-eval-after-load 'ivy--regex-migemo
;;   (avy-migemo-remove-names 'ivy--regex-migemo
;;                            'ivy--format-minibuffer-line-migemo))
;; (with-eval-after-load 'swiper--add-overlays-migemo
;;   (avy-migemo-remove-names 'swiper--add-overlays-migemo))

;;; Code:

;; For using swiper ( `ivy--regex' ) with migemo
(with-eval-after-load "avy-migemo"
  (with-eval-after-load "ivy"
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

    (defun ivy--format-minibuffer-line-migemo (str)
      "The same as `ivy--format-minibuffer-line' except adapting it for migemo's regexp."
      (let ((start 0)
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
                    (cl-incf i-face))))))
        str))
    (byte-compile 'ivy--format-minibuffer-line-migemo)

    (avy-migemo-add-names 'ivy--regex-migemo
                          'ivy--format-minibuffer-line-migemo)
    (add-hook 'avy-migemo-regex-cache-clear-hook
              'avy-migemo--ivy--regex-hash-clear)

    (provide 'ivy--regex-migemo))

  (with-eval-after-load "swiper"
    (defun swiper--add-overlays-migemo (re &optional beg end)
      "The same as `swiper--add-overlays' except adapting it for migemo's regexp."
      (let ((ov (if visual-line-mode
                    (make-overlay
                     (save-excursion
                       (beginning-of-visual-line)
                       (point))
                     (save-excursion
                       (end-of-visual-line)
                       (point)))
                  (make-overlay
                   (line-beginning-position)
                   (1+ (line-end-position))))))
        (overlay-put ov 'face 'swiper-line-face)
        (overlay-put ov 'window (ivy-state-window ivy-last))
        (push ov swiper--overlays)
        (let* ((wh (window-height))
               (beg (or beg (save-excursion
                              (forward-line (- wh))
                              (point))))
               (end (or end (save-excursion
                              (forward-line wh)
                              (point)))))
          (when (>= (length re) swiper-min-highlight)
            (save-excursion
              (goto-char beg)
              ;; RE can become an invalid regexp
              (while (and (ignore-errors (re-search-forward re end t))
                          (> (- (match-end 0) (match-beginning 0)) 0))
                ;; Adapt for migemo's regexp.
                (cl-loop
                 with i-face = 0
                 with l-mend = 0
                 with mend-0 = (match-end 0)
                 for i from 0 below (if (zerop ivy--subexps) 1
                                      (cl-loop for _ in (match-data) by #'cddr sum 1))
                 when (>= l-mend mend-0) return nil
                 for mbeg = (match-beginning i)
                 for mend = (match-end i)
                 when (and mbeg (<= l-mend mbeg)) do
                 (let ((overlay (make-overlay
                                 mbeg
                                 (if (> i 0) (setq l-mend mend) mend)))
                       (face
                        (cond ((zerop ivy--subexps)
                               (cadr swiper-faces))
                              ((zerop i)
                               (car swiper-faces))
                              (t
                               (nth (1+ (mod (+ i-face 2) (1- (length swiper-faces))))
                                    swiper-faces)))))
                   (push overlay swiper--overlays)
                   (overlay-put overlay 'face face)
                   (overlay-put overlay 'window (ivy-state-window ivy-last))
                   (overlay-put overlay 'priority i-face)
                   (cl-incf i-face)))))))))

    (byte-compile 'swiper--add-overlays-migemo)

    (avy-migemo-add-names 'swiper--add-overlays-migemo)

    (provide 'swiper--add-overlays-migemo)))

(provide 'avy-migemo-e.g.swiper)
;;; avy-migemo-e.g.swiper.el ends here
