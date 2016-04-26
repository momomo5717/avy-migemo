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

;; For using with avy-migemo-mode
(avy-migemo-add-names 'swiper--add-overlays-migemo)

(provide 'avy-migemo-e.g.swiper)
;;; avy-migemo-e.g.swiper.el ends here
