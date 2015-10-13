;;; helm-flx.el --- Sort helm candidates by flx score -*- lexical-binding: t -*-

;; Copyright (C) 2014 PythonNut

;; Author: PythonNut <pythonnut@pythonnut.com>
;; Keywords: convenience, helm, fuzzy, flx
;; Version: 20151013
;; URL: https://github.com/PythonNut/helm-flx
;; Package-Requires: ((emacs "24") (helm "20151013.958") (flx "20140921.739"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(eval-when-compile
  (with-demoted-errors "Byte-compile: %s"
    (require 'helm)))

(defgroup helm-flx nil
  "Sort helm candidates by flx score"
  :group 'convenience
  :prefix "helm-flx-")

(defvar helm-flx-cache nil)

(with-eval-after-load 'flx
  (setq helm-flx-cache (flx-make-string-cache #'flx-get-heatmap-file)))

(defun helm-flx-fuzzy-matching-sort-fn (candidates _source &optional use-real)
  (require 'flx)
  (if (string= helm-pattern "")
      candidates
    (mapcar #'car
            (sort (mapcar
                   (lambda (cand)
                     (cons cand (or
                                 (car (flx-score
                                       (if (consp cand)
                                           (if use-real
                                               (cdr cand)
                                             (car cand))
                                         cand)
                                       helm-pattern helm-flx-cache))
                                 0)))
                   candidates)
                  (lambda (s1 s2)
                    (> (cdr s1)
                       (cdr s2)))))))

(defun helm-flx-fuzzy-highlight-match (candidate)
  (require 'flx)
  (let* ((pair (and (consp candidate) candidate))
         (display (if pair (car pair) candidate))
         (real (cdr pair)))
    (with-temp-buffer
      (insert display)
      (goto-char (point-min))
      (if (string-match-p " " helm-pattern)
          (dolist (p (split-string helm-pattern))
            (when (search-forward p nil t)
              (add-text-properties
               (match-beginning 0) (match-end 0) '(face helm-match))))
        (dolist (index (cdr (flx-score
                             (substring-no-properties display)
                             helm-pattern helm-flx-cache)))
          (with-demoted-errors
              (add-text-properties
               (1+ index) (+ 2 index) '(face helm-match)))))
      (setq display (buffer-string)))
    (if real (cons display real) display)))

(provide 'helm-flx)
;;; helm-flx.el ends here
