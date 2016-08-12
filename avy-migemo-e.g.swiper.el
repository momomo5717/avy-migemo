;;; avy-migemo-e.g.swiper.el --- An example config of avy-migemo for swiper -*- lexical-binding: t; no-byte-compile: t -*-

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

;; This file is an example config of avy-migemo for swiper 0.8.0 or later.
;;
;; (require 'avy-migemo-e.g.swiper)
;;
;; Note: This file is not compiled for installing from MELPA.

;;; Code:
(require 'swiper)
(require 'avy-migemo-e.g.ivy)

(defun swiper--add-overlays-migemo-ignore-order (re-seq &optional beg end wnd)
  "Add overlays at lines matched by RE-SEQ from BEG to END on WND.
RE-SEQ is a list of \(regex . boolean)."
  (setq wnd (or wnd (ivy-state-window ivy-last)))
  (cl-macrolet ((lbeg-pos () '(if visual-line-mode
                                  (save-excursion
                                    (beginning-of-visual-line)
                                    (point))
                                (line-beginning-position)))
                (lend-pos () '(if visual-line-mode
                                  (save-excursion
                                    (end-of-visible-line)
                                    (point))
                                (line-end-position))))
    (when (and re-seq
               (cl-loop for (re . _) in re-seq
                        always (ignore-errors (string-match re "") t)))
      (let* ((wh nil)
             (beg (or beg (save-excursion
                            (forward-line (- (setq wh (window-height wnd))))
                            (point))))
             (end (or end (save-excursion
                            (forward-line (or wh (window-height wnd)))
                            (point))))
             (re (caar re-seq))
             lbeg lend)
        (save-excursion
          (goto-char beg)
          (while (re-search-forward re end t)
            (when (ivy-re-match
                   re-seq (funcall (if (memq major-mode '(org-mode dired-mode))
                                       #'buffer-substring-no-properties
                                     #'buffer-substring)
                                   (setq lbeg (max (lbeg-pos) beg))
                                   (setq lend (min (lend-pos) end))))
              (cl-loop
               with i-face = 1
               for (re . match-p) in re-seq
               for face = (nth (1+ (mod (+ i-face 2) (1- (length swiper-faces))))
                               swiper-faces) do
               (goto-char lbeg)
               (while (and match-p
                           (re-search-forward re lend t))
                 (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                   (push ov swiper--overlays)
                   (overlay-put ov 'face face)
                   (overlay-put ov 'window wnd)
                   (overlay-put ov 'priority i-face)))
               (when match-p (cl-incf i-face))))
            (goto-char lend)))))))
(byte-compile 'swiper--add-overlays-migemo-ignore-order)

(defun swiper--add-overlays-migemo (re &optional beg end wnd)
  "The same as `swiper--add-overlays' except adapting it for migemo's regexp."
  (setq wnd (or wnd (ivy-state-window ivy-last)))
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
    (overlay-put ov 'window wnd)
    (push ov swiper--overlays)
    (let* ((wh (window-height wnd))
           (beg (or beg (save-excursion
                          (forward-line (- wh))
                          (point))))
           (end (or end (save-excursion
                          (forward-line wh)
                          (point)))))
      (when (>= (length re) swiper-min-highlight)
        (if (eq ivy--regex-function 'ivy--regex-ignore-order)
            (swiper--add-overlays-migemo-ignore-order
             (ivy--regex-ignore-order ivy-text) beg end wnd)
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
                                    (/ (length (match-data)) 2))
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
                 (overlay-put overlay 'window wnd)
                 (overlay-put overlay 'priority i-face)
                 (cl-incf i-face))))))))))
(byte-compile 'swiper--add-overlays-migemo)

;; For using with avy-migemo-mode
(avy-migemo-add-names 'swiper--add-overlays-migemo)

(provide 'avy-migemo-e.g.swiper)
;;; avy-migemo-e.g.swiper.el ends here
