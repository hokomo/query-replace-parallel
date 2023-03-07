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

(defun query-replace-parallel--prompt (regexp-flag)
  (concat "Query replace parallel"
          (and regexp-flag " regexp")
		  (and current-prefix-arg
               (if (eq current-prefix-arg '-) " backward" " word"))
		  (and (use-region-p) " in region")))

(defun query-replace-parallel--read-args (regexp-flag)
  (cl-loop for (from to delim backward)
             = (query-replace-read-args
                (query-replace-parallel--prompt regexp-flag) regexp-flag)
           for pair = (cons from to)
           ;; NOTE: `query-replace-read-args' will return the last pair from
           ;; history in case of empty input. That's our signal to stop reading.
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
      (let ((form (walk (rxt-elisp-to-rx regexp))))
        (list (rx-to-string form) (nreverse groups))))))

(defun query-replace-parallel--table (pairs regexp-flag)
  (cl-loop with i = 1
           for (from . to) in pairs
           for (nfrom groups) = (if regexp-flag
                                    (query-replace-parallel--flatten from)
                                  (list (regexp-quote from) '()))
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
  (propertize
   (funcall oldfun
            (if (get-text-property 0 'query-replace-parallel--tag string)
                (caar query-replace-parallel--description)
              string))
   'query-replace-parallel--tag t))

(defun query-replace-parallel--patch-message (args)
  (cl-destructuring-bind (format &optional arg &rest rest) args
    (if (and (stringp arg)
             (get-text-property 0 'query-replace-parallel--tag arg))
        (let ((nformat (apply
                        #'propertize
                        (replace-regexp-in-string
                         (rx "Query replacing" (group (* nonl)) "regexp %s")
                         (concat "Query replacing parallel\\1"
                                 (and (cdar query-replace-parallel--description)
                                      "regexp ")
                                 "%s")
                         format)
                        (text-properties-at 0 format))))
          (cl-list* nformat arg rest))
      args)))

(defun query-replace-parallel--replacer (table regexp-flag)
  (lambda (_arg count)
    (cl-destructuring-bind (base . (from to _nfrom groups))
        (cl-find-if #'match-beginning table :key #'car)
      (setf (caar query-replace-parallel--description) from)
      (if (not regexp-flag)
          (replace-quote to)
        (let ((original (match-data)))
          (set-match-data (query-replace-parallel--match-data base groups))
          (unwind-protect
              (cl-etypecase to
                (string
                 (query-replace-parallel--quote
                  (match-substitute-replacement
                   to (not (and case-replace case-fold-search)))))
                (cons (funcall (car to) (cdr to) count)))
            (set-match-data original)))))))

(defun query-replace-parallel-perform-replace
    (pairs query-flag regexp-flag delimited
     &optional repeat-count map start end backward region-noncontiguous-p)
  (let* ((table (query-replace-parallel--table pairs regexp-flag))
         (regexp (query-replace-parallel--matcher (mapcar #'cadddr table)))
         (query-replace-parallel--description
          (cons (cons nil regexp-flag) query-replace-parallel--description)))
    (advice-add #'replace-match-maybe-edit :filter-args
                #'query-replace-parallel--patch-noedit)
    (advice-add #'query-replace-descr :around
                #'query-replace-parallel--patch-description)
    (advice-add #'message :filter-args
                #'query-replace-parallel--patch-message)
    (unwind-protect
        (perform-replace
         (propertize regexp 'query-replace-parallel--tag t)
         (cons (query-replace-parallel--replacer table regexp-flag) nil)
         query-flag :regexp delimited repeat-count map start end backward
         region-noncontiguous-p)
      (advice-remove #'replace-match-maybe-edit
                     #'query-replace-parallel--patch-noedit)
      (advice-remove #'query-replace-descr
                     #'query-replace-parallel--patch-description)
      (advice-remove #'message
                     #'query-replace-parallel--patch-message))))

(defun query-replace-parallel--args (regexp-flag)
  (cl-destructuring-bind (pairs delimited backward)
      (query-replace-parallel--read-args regexp-flag)
    (list pairs
          delimited
          (and (use-region-p) (region-beginning))
          (and (use-region-p) (region-end))
          backward
          (and (use-region-p) (region-noncontiguous-p)))))

(defun query-replace-parallel
    (pairs &optional delimited start end backward region-noncontiguous-p)
  (interactive (query-replace-parallel--args nil))
  (query-replace-parallel-perform-replace
   pairs :query nil delimited nil nil start end backward
   region-noncontiguous-p))

(defun query-replace-parallel-regexp
    (pairs &optional delimited start end backward region-noncontiguous-p)
  (interactive (query-replace-parallel--args :regexp))
  (query-replace-parallel-perform-replace
   pairs :query :regexp delimited nil nil start end backward
   region-noncontiguous-p))

(provide 'query-replace-parallel)
;;; query-replace-parallel.el ends here
