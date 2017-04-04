;;; avy-migemo-e.g.swiper.el --- An example config of avy-migemo for swiper -*- lexical-binding: t; no-byte-compile: t -*-

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

;; This file is an example config of avy-migemo for swiper 0.8.0 or later.
;;
;; (require 'avy-migemo-e.g.swiper)
;;
;; Note: This file is not compiled for installing from MELPA.

;;; Code:
(require 'swiper)
(require 'avy-migemo-e.g.ivy)

(defcustom swiper-migemo-min-highlight 2
  "Highlight matches if `ivy-text' is at least this long.
If nil, `swiper-min-highlight' will be used."
  :group 'swiper
  :type '(choice integer
                 (const :tag "Use `swiper-min-highlight'." nil)))

(defun swiper--add-overlays-migemo-visible-regions (beg end)
  "Return visible regions between BEG and END."
  (let ((pt beg)
        npt
        regions)
    (while (< pt end)
      (setq npt (next-single-char-property-change pt 'invisible nil end))
      (unless (invisible-p pt) (push (cons pt npt) regions))
      (setq pt npt))
    (nreverse regions)))
(byte-compile 'swiper--add-overlays-migemo-visible-regions)

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
               for (lbeg . lend) in (swiper--add-overlays-migemo-visible-regions lbeg lend) do
               (cl-loop
                with i-face = 1
                for (re . match-p) in re-seq do
                (goto-char lbeg)
                (while (and match-p
                            (re-search-forward re lend t))
                  (swiper--add-overlay
                   (match-beginning 0) (match-end 0)
                   (nth (1+ (mod (+ i-face 2) (1- (length swiper-faces))))
                        swiper-faces)
                   wnd i-face))
                (when match-p (cl-incf i-face)))))
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
      (when (if swiper-migemo-min-highlight
                (>= (length ivy-text) swiper-migemo-min-highlight)
              (>= (length re) swiper-min-highlight))
        (if (eq ivy--regex-function 'ivy--regex-ignore-order)
            (swiper--add-overlays-migemo-ignore-order
             (ivy--regex-ignore-order ivy-text) beg end wnd)
          (save-excursion
            (cl-loop
             for (beg . end) in (swiper--add-overlays-migemo-visible-regions beg end) do
             (goto-char beg)
             ;; RE can become an invalid regexp
             (while (and (ignore-errors (re-search-forward re end t))
                         (> (- (match-end 0) (match-beginning 0)) 0))

               (swiper--add-overlay (match-beginning 0) (match-end 0)
                                    (if (zerop ivy--subexps)
                                        (cadr swiper-faces)
                                      (car swiper-faces))
                                    wnd 0)
               ;; Adapt for migemo's regexp.
               (cl-loop
                with i-face = 1
                with c-mbeg = nil
                with l-mend = 0
                with mend-0 = (match-end 0)
                for i from 1 below (if (zerop ivy--subexps) 1
                                     (/ (length (match-data)) 2))
                when (>= l-mend mend-0) return nil
                for mbeg = (or c-mbeg (match-beginning i))
                for mend = (match-end i)
                when (and mbeg mend (>= mbeg l-mend)) do
                (setq c-mbeg (or c-mbeg mbeg))
                (unless (and (match-beginning (1+ i))
                             (= mend (match-beginning (1+ i))))
                  (swiper--add-overlay
                   mbeg (setq l-mend mend)
                   (nth (1+ (mod (+ i-face 2) (1- (length swiper-faces))))
                        swiper-faces)
                   wnd i-face)
                  (setq c-mbeg nil)
                  (cl-incf i-face)))))))))))
(byte-compile 'swiper--add-overlays-migemo)

(defvar search-default-mode)
(defun swiper--re-builder-migemo-around (fn &rest args)
  "Around advice function for `swiper--re-builder'."
  (let ((search-default-mode nil))
    (apply fn args)))
(byte-compile 'swiper--re-builder-migemo-around)

;; For using with avy-migemo-mode
(avy-migemo-add-names 'swiper--add-overlays-migemo
                      '(swiper--re-builder
                        :around swiper--re-builder-migemo-around))

(provide 'avy-migemo-e.g.swiper)
;;; avy-migemo-e.g.swiper.el ends here
