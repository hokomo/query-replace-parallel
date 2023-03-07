;;; query-replace-parallel.el --- Parallel replacements for query-replace  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 hokomo
;; Copyright (C) 2023 Valentino Picotti

;; Author: hokomo <hokomo@airmail.cc>
;;         Valentino Picotti <valentino.picotti@gmail.com>
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2"))
;; Keywords: tools, convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'pcre2el)
(require 'rx)

(defun query-replace-parallel--prompt ()
  (concat "Query replace parallel"
		  (and current-prefix-arg
		       (if (eq current-prefix-arg '-) " backward" " word"))
		  (and (use-region-p) " in region")))

(defun query-replace-parallel--read-args (regexp-flag)
  (cl-loop for (from to delim backward) = (query-replace-read-args
                                           (query-replace-parallel--prompt)
                                           regexp-flag)
           for pair = (cons from to)
           until (member pair pairs)
           collect pair into pairs
           finally (cl-return (list pairs delim backward))))

(defun query-replace-parallel--matcher (regexps)
  (rx-to-string `(or ,@(mapcar (lambda (r) `(group (regexp ,r))) regexps))))

(defun query-replace-parallel--flatten (regexp)
  (let ((i 1)
        (groups '()))
    (cl-labels ((walk (root)
                  (if (atom root)
                      root
                    (pcase root
                      (`(submatch . ,rest)
                       (push i groups)
                       (cl-incf i)
                       `(submatch ,@(walk rest)))
                      (`(submatch-n ,n . ,rest)
                       (push n groups)
                       (setf i (max i (1+ n)))
                       `(submatch ,@(walk rest)))
                      (_
                       (mapcar #'walk root))))))
      (let ((form (walk (ignore-errors (rxt-elisp-to-rx regexp)))))
        (list (rx-to-string form) (nreverse groups))))))

(defun query-replace-parallel--table (pairs)
  (cl-loop with i = 1
           for (from . to) in pairs
           for (nfrom groups) = (query-replace-parallel--flatten from)
           collect (cons i (list from to nfrom groups))
           do (cl-incf i (1+ (length groups)))))

(defun query-replace-parallel--match-data (base groups)
  (let* ((n (if groups (apply #'max groups) 0))
         (data (make-vector (* 2 (1+ n)) nil)))
    (setf (aref data 0) (match-beginning base)
          (aref data 1) (match-end base))
    (cl-loop for i from 1
             for j in groups
             when (match-beginning (+ base i))
               do (setf (aref data (* 2 j)) (match-beginning (+ base i))
                        (aref data (1+ (* 2 j))) (match-end (+ base i))))
    (cl-coerce data 'list)))

(defun query-replace-parallel--quote (string)
  (string-replace "\\\\?" "\\?" (string-replace "\\" "\\\\" string)))

(defun query-replace-parallel--patch-noedit (args)
  (cl-destructuring-bind (newtext fixedcase literal _noedit match-data
                          &optional backward)
      args
    (list newtext fixedcase literal nil match-data backward)))

(defvar query-replace-parallel--description '())

(defun query-replace-parallel--patch-description (oldfun string)
  (funcall oldfun
           (if (get-text-property 0 'query-replace-parallel--tag string)
               (car query-replace-parallel--description)
             string)))

(defun query-replace-parallel--replace (table count)
  (cl-destructuring-bind (base . (from to _nfrom groups))
      (cl-find-if (pcase-lambda (`(,base . ,_))
                    (match-beginning base))
                  table)
    (let ((original (match-data)))
      (setf (car query-replace-parallel--description) from)
      (set-match-data (query-replace-parallel--match-data base groups))
      (unwind-protect
          (cl-etypecase to
            (string
             (query-replace-parallel--quote
              (match-substitute-replacement
               to
               (not (and case-replace case-fold-search))
               nil
               ;; TODO: Refactor into `perform-replace-parallel'
               ;; with a `regexp-flag'.
               ;;
               ;; (or (not regexp-flag) (eq regexp-flag 'literal))
               )))
            (cons (funcall (car to) (cdr to) count)))
        (set-match-data original)))))

(defun query-replace-parallel-regexp
    (pairs &optional delimited start end backward region-noncontiguous-p)
  (interactive (let ((common (query-replace-parallel--read-args :regexp)))
                 (list (nth 0 common)
                       (nth 1 common)
                       (and (use-region-p) (region-beginning))
                       (and (use-region-p) (region-end))
                       (nth 2 common)
                       (and (use-region-p) (region-noncontiguous-p)))))
  (let* ((table (query-replace-parallel--table pairs))
         (regexp (query-replace-parallel--matcher
                  (mapcar #'cadddr table)))
         (query-replace-parallel--description
          (cons nil query-replace-parallel--description)))
    (advice-add #'replace-match-maybe-edit :filter-args
                #'query-replace-parallel--patch-noedit)
    (advice-add #'query-replace-descr :around
                #'query-replace-parallel--patch-description)
    (unwind-protect
        (perform-replace
         (propertize regexp 'query-replace-parallel--tag t)
         (cons #'query-replace-parallel--replace table) :query
         :regexp delimited nil nil start end backward region-noncontiguous-p)
      (advice-remove #'replace-match-maybe-edit
                     #'query-replace-parallel--patch-noedit)
      (advice-remove #'query-replace-descr
                     #'query-replace-parallel--patch-description))))

(provide 'query-replace-parallel)
;;; query-replace-parallel.el ends here
