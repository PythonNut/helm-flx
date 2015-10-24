;;; helm-flx.el --- Sort helm candidates by flx score -*- lexical-binding: t -*-

;; Copyright (C) 2014, 2015 PythonNut

;; Author: PythonNut <pythonnut@pythonnut.com>
;; Keywords: convenience, helm, fuzzy, flx
;; Version: 20151013
;; URL: https://github.com/PythonNut/helm-flx
;; Package-Requires: ((emacs "24.4") (helm "1.7.9") (flx "0.5"))

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

;; This package implements intelligent helm fuzzy sorting, provided by flx.

;; You can install the package by either cloning it yourself, or by doing M-x package-install RET helm-flx RET.

;; After that, you can enable it by putting the following in your init file:

;;     ;; For best results, load this before you load helm.
;;     (helm-flx-mode +1)

;; See the README for more info.

;;; Code:

(eval-when-compile
  (with-demoted-errors "Byte-compile: %s"
    (require 'helm)
    (require 'flx)))

(defgroup helm-flx nil
  "Sort helm candidates by flx score"
  :group 'convenience
  :prefix "helm-flx-")

(defcustom helm-flx-limit 5000
  "The maximum number of helm candidates (N) to sort. If the number of
candidates is greater than this number, only sort the first N (presorted by length). Set to nil to sort all candidates."
  :type 'number
  :group 'flx-isearch)

(defvar helm-flx-cache nil
  "Stores the current flx cache for helm-flx.")

(defvar helm-flx-old-helm-fuzzy-sort-fn nil
  "Stores the old value of helm-fuzzy-sort-fn")
(defvar helm-flx-old-helm-fuzzy-matching-highlight-fn nil
  "Stored the old value of helm-fuzzy-matching-highlight-fn")

(with-eval-after-load 'flx
  (setq helm-flx-cache (flx-make-filename-cache)))

(defun helm-flx-sort (candidates pattern display-string-fn &optional score-fn)
  (require 'flx)
  (let ((num-cands (length candidates)))
    (mapcar #'car
            (sort (mapcar
                   (or (and score-fn
                            (funcall score-fn pattern display-string-fn))
                       (lambda (cand)
                         (cons cand
                               (or (car (flx-score (funcall display-string-fn
                                                            cand)
                                                   pattern
                                                   helm-flx-cache))
                                   most-negative-fixnum))))
                   (if (or (not helm-flx-limit)
                           (> helm-flx-limit helm-candidate-number-limit)
                           (< num-cands helm-flx-limit))
                       candidates
                     (let* ((seq (sort candidates
                                       (lambda (c1 c2)
                                         (< (length (funcall display-string-fn
                                                             c1))
                                            (length (funcall display-string-fn
                                                             c2))))))
                            (end (min helm-flx-limit
                                      num-cands
                                      (length seq)))
                            (result nil))
                       (dotimes (_ end result)
                         (push (pop seq) result)))))
                  (lambda (c1 c2)
                    (> (cdr c1)
                       (cdr c2)))))))

(defun helm-flx-helm-ff-sort-candidates (candidates _source)
  "Sort function for `helm-source-find-files'.
Return candidates prefixed with basename of `helm-input' first."
  (require 'flx)
  (if (string= helm-input "")
      candidates
    (helm-flx-sort candidates helm-input
                   (lambda (cand)
                     (substring-no-properties
                      (if (consp cand)
                          (cdr cand)
                        cand)))
                   (lambda (pattern display-string-fn)
                     (lambda (cand)
                       (cons cand
                             (if (string-match-p
                                  "^\\[\\?\\]"
                                  (funcall display-string-fn cand))
                                 most-positive-fixnum
                               (or (car (flx-score
                                         (funcall display-string-fn cand)
                                         pattern
                                         helm-flx-cache))
                                   most-negative-fixnum))))))))

(defun helm-flx-fuzzy-matching-sort (candidates _source &optional use-real)
  (if (string= helm-pattern "")
      candidates
    (helm-flx-sort candidates
                   helm-pattern
                   (if use-real
                       (lambda (cand)
                         (if (consp cand)
                             (cdr cand)
                           cand))
                     (lambda (cand)
                       (if (consp cand)
                           (car cand)
                         cand))))))

(defun helm-flx-candidate-string (candidate)
  (cond
   ((symbolp candidate) (symbol-name candidate))
   (t candidate)))

(defun helm-flx-fuzzy-highlight-match (candidate)
  (require 'flx)
  (let* ((candidate (helm-flx-candidate-string candidate))
         (pair (and (consp candidate) candidate))
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
          (with-demoted-errors "helm-fx error: %s"
            (add-text-properties
             (1+ index) (+ 2 index) '(face helm-match)))))
      (setq display (buffer-string)))
    (if real (cons display real) display)))

;;;###autoload
(define-minor-mode helm-flx-mode
  "helm-flx minor mode"
  :init-value nil
  :group 'helm-flx
  :global t
  (if helm-flx-mode
      (progn (setq helm-flx-old-helm-fuzzy-sort-fn
                   (bound-and-true-p helm-fuzzy-sort-fn))
             (setq helm-flx-old-helm-fuzzy-matching-highlight-fn
                   (bound-and-true-p helm-fuzzy-matching-highlight-fn))
             (setq helm-fuzzy-sort-fn
                   #'helm-flx-fuzzy-matching-sort)
             (setq helm-fuzzy-matching-highlight-fn
                   #'helm-flx-fuzzy-highlight-match)
             (advice-add 'helm-ff-sort-candidates :override
                         #'helm-flx-helm-ff-sort-candidates))

    (setq helm-fuzzy-sort-fn
          (or helm-flx-old-helm-fuzzy-sort-fn
              #'helm-fuzzy-matching-default-sort-fn))
    (setq helm-fuzzy-matching-highlight-fn
          (or helm-flx-old-helm-fuzzy-matching-highlight-fn
              #'helm-fuzzy-default-highlight-match))
    (advice-remove 'helm-ff-sort-candidates
                   #'helm-flx-helm-ff-sort-candidates)))

(provide 'helm-flx)

;;; helm-flx.el ends here
