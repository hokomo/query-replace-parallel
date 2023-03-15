;;; query-replace-parallel.el --- Parallel replacements for query-replace  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 hokomo
;; Copyright (C) 2023 Valentino Picotti

;; Author: hokomo <hokomo@airmail.cc>
;;         Valentino Picotti <valentino.picotti@gmail.com>
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2") (pcre2el "20161120.2103"))
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

;;; Commentary:

;; * About
;;
;; This package provides a "parallel" version of `query-replace', capable of
;; performing multiple different replacements with just a single pass, while
;; eliminating the possibility of erroneously replacing a previous replacement.
;; E.g. one can safely replace "foo" with "bar" and "bar" with "foo" at the same
;; time.
;;
;; We provide the full functionality of `query-replace', including regexps,
;; capture references (`\N', `\&'), Lisp expressions (`\,') and query edits
;; (`\?'). See `query-replace-regexp' for the details.
;;
;; Use `query-replace-parallel' if you want to replace literal strings. This is
;; the parallel counterpart to `query-replace'.
;;
;; Use `query-replace-parallel-regexp' if you want to replace regexps and use
;; the various regexp replacement features. This is the parallel counterpart to
;; `query-replace-regexp'.

;; * Implementation
;;
;; The core of the package is `query-replace-parallel-perform-replace',
;; implemented on top of the existing interactive replacement machinery, namely,
;; `perform-replace'. Figuring out a way to do all of the replacements in a
;; single pass while preserving all of its sophisticated functionality is
;; tricky, but can be done. Just like Olin Shivers, we attempt a 100% solution
;; (https://www.ccs.neu.edu/home/shivers/papers/sre.txt). :-)
;;
;; We describe the most general case of performing a series of regexp
;; replacements, as the literal case can be implemented trivially on top.
;;
;; First, the user provides a series of replacement pairs (FROM . TO). FROM is a
;; regexp, while TO can be a replacement string or a cons specifying the
;; function to invoke to retrieve the replacement string (just like in
;; `perform-replace').
;;
;; We invoke `perform-replace' once, with a "matcher regexp" that we construct
;; by putting all of the FROM regexps into one big alternative construct, while
;; wrapping each alternative in a separate capture group:
;;
;;   \(FROM-1\)\|...\|\(FROM-N\)
;;
;; This regexp allows us to match any of the given replacement pairs (those
;; appearing earlier in the list have priority), while also being able to tell
;; which particular pair matched. However, any of the FROM regexps might contain
;; their own capture groups, so we can't immediately tell what are the indices
;; of our top-level groups.
;;
;; More generally, we want the user to be able to treat each replacement in
;; isolation and use advanced features such as explicitly numbered groups
;; without having to manually adjust the replacement by, e.g., shifting group
;; numbers around. Similarly, if TO is a function, we want it to have available
;; the match data from the perspective of just the one particular FROM regexp
;; that matched.
;;
;; To solve this properly, we preprocess each FROM regexp into a list (FLAT
;; GROUPS). FLAT is the "flattened" version of FROM where any explicitly
;; numbered groups have been replaced with implicit ones. GROUPS is a list of
;; indices, each related to an implicit group in FLAT, in order from left to
;; right. The index is the number of the corresponding group in FROM, which
;; gives us a 1-to-1 mapping between the groups of FROM and FLAT and allows us
;; to reconstruct the "local" information from our "global" matcher information.
;; The matcher actually becomes:
;;
;;   \(FLAT-1\)\|...\|\(FLAT-N\)
;;
;; The replacement function we use in our call to `perform-replace' must then
;; determine which of the pairs matched and activate the appropriate match data.
;; For this purpose, we construct a "replacement table", which is an alist where
;; each pair has a corresponding entry (BASE . (FROM TO FLAT GROUPS)).
;;
;; The key BASE is the index of the group wrapped around the alternative for the
;; particular pair in the matcher regexp. Because the matcher regexp consists
;; only of implicit groups, we can be sure that zero or more successive groups
;; starting at BASE + 1 in the matcher regexp all correspond to the groups
;; specified by the user for that pair. The match data is then constructed by
;; mapping each such successive group to the corresponding group in the original
;; FROM regexp, given by GROUPS. If GROUPS contains duplicates, the rightmost
;; mapping wins.
;;
;; Whenever called, our replacement function can scan the table to determine
;; which of the pairs matched, use BASE and GROUPS to construct the match data
;; appropriate for the pair and then perform the replacement. The order of the
;; entries in the table is significant and matches the order of the given
;; replacement pairs.

;; * Explicitly Numbered Groups
;;
;; Even though the Emacs manual claims "There is no particular restriction on
;; the numbering" in the paragraph on explicitly numbered groups, the regexp
;; engine doesn't allow them to use the same index as any of their enclosing
;; (parent) groups. E.g. the following is invalid:
;;
;;   \(hello \(?1:there\)\)
;;
;; Our approach actually ends up fixing this limitation for any regexp specified
;; by the user, while following the same rightmost-match-wins semantics.
;;
;; See (elisp) Regexp Backslash in the Emacs Lisp manual and
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2012-01/msg00283.html for a
;; discussion on the Emacs mailing list.

;; * `noedit' Flag
;;
;; The `replace-match-maybe-edit' function is in charge of processing the query
;; edit (`\?') feature in the replacement. It is given a `noedit' parameter
;; maintained by the body of `perform-replace', which is used as a flag to tell
;; whether the replacement contains a query edit or not. If it doesn't, no query
;; edit processing is done.
;;
;; From the looks of it, this is supposed to be an optimization. However, until
;; the flag becomes non-nil, it is set every time again and again upon each
;; replacement. Once it becomes non-nil, `perform-replace' stops processing
;; query edits in replacements.
;;
;; While this is fine for replacements that are constant, it cannot work for
;; cases where the replacement string is dynamically constructed by a function.
;; In those cases, query edits will work until a replacement with no query edit
;; is encountered, after which they stop working.
;;
;; This actually uncovered a bug in `map-query-replace-regexp' which ends up
;; making use of the replacement function feature. Using "hello\? there world\?"
;; as the replacement string showcases the bug.
;;
;; To fix this, we advise `replace-match-maybe-edit' to always treat `noedit'
;; as nil.

;; * Messages
;;
;; We advise a few functions in order to provide the user with nice messages
;; during a replacement session, instead of the big and ugly matcher regexp.
;;
;; Luckily we are able to make the advice quite specific with the use of a
;; string property (`query-replace-parallel--tag') to detect when to fire. See
;; also the description of `query-replace-parallel--description'.

;; * Related Work
;;
;; There have been multiple previous attempts in solving the same problem, which
;; we take some inspiration from. However, they all stop short of a full
;; solution in one form or another:
;;
;; (1) They only work with literals instead of arbitrary regexps. Even if they
;;     do mention the necessary `query-replace-regexp' workflow to achieve a
;;     parallel replace, they don't attempt to generalize and automate it, which
;;     requires the user to effectively construct the matcher regexp and detect
;;     the matched pairs by hand.
;;
;; (2) They don't integrate with `query-replace'. This loses the commonly useful
;;     interactive features such as skipping matches, going backward, undoing,
;;     etc., but also the more advanced ones such as performing a recursive
;;     edit, or using Lisp expressions (`\,') and query edits (`\?') in
;;     replacements.
;;
;; (3) They don't provide a reusable programmatic interface for the implemented
;;     functionality.
;;
;; See e.g.:
;;
;; - https://www.emacswiki.org/emacs/SwappingText
;; - https://stackoverflow.com/questions/2588277/how-can-i-swap-or-replace-multiple-strings-in-code-at-the-same-time/2592685#2592685
;; - https://www.masteringemacs.org/article/evaluating-lisp-forms-regular-expressions
;; - https://tony-zorman.com/posts/query-replace-many.html

;; * TODO
;;
;; - Report the mentioned `map-query-replace-regexp' `noedit' bug.
;;
;; - Aside from the one we patched, there remains one other "Query replacing
;;   ..." message. However, this one is printed into the help buffer of query
;;   replace and not at all that visible, so maybe this isn't so important.
;;   Also, fixing it would mean advising `concat' or `princ' which doesn't sound
;;   good (but then again, neither does advising `message' which we already do
;;   :-)).
;;
;; - Add assumption checks/assertions/warnings for our advice, since we're
;;   meddling with the internals of `replace.el'. Luckily this is all purely
;;   aesthetical, but it would be nice to get a warning if anything ever changes
;;   unexpectedly.

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
  "Interactively read replacement pairs for a parallel query
replace by invoking `query-replace-read-args' multiple times.

Reading stops when a replacement pair is repeated. Return the
list (PAIRS DELIM BACKWARD).

PAIRS is a list of conses (FROM . TO). FROM is the source string
read from the user. If REGEXP-FLAG is nil, TO is the replacement
string read from the user. Otherwise, TO can also be a cons
depending on whether the read replacement string uses the Lisp
expression `\\,' feature or not.

DELIM and BACKWARD are taken from the return value of the last
call to `query-replace-read-args' and should be forwarded as the
arguments to the query replacement functions."
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
  "Given a list of regexps, return a regexp that matches either of
them (in order), but with a capture group wrapped around each
one."
  (rx-to-string `(or ,@(mapcar (lambda (r) `(group (regexp ,r))) regexps))))

(defun query-replace-parallel--flatten (regexp)
  "Given a regexp, return the list (FLAT GROUPS).

FLAT is the \"flattened\" version of REGEXP, where each
explicitly numbered group is replaced with an implicit one.

GROUPS is a list of indices, each related to an implicit group in
FLAT, in order from left to right. The index is the number of the
corresponding group in REGEXP, which gives us a 1-to-1 mapping
between the groups of REGEXP and FLAT."
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
  "Given the list of replacement pairs, construct the replacement
table.

Each pair is of the form (FROM . TO) and has a corresponding
entry in the replacement table, which is an alist containing
entries of the form (BASE . (FROM TO FLAT GROUPS)).

The key BASE is the index of the group wrapped around the
alternative for the particular pair in the matcher regexp. If
REGEXP-FLAG is non-nil, FLAT and GROUPS are the result of
flattening FROM. Otherwise, FLAT is a regexp-quoted version of
FROM and GROUPS is an empty list."
  (cl-loop with i = 1
           for (from . to) in pairs
           for (flat groups) = (if regexp-flag
                                   (query-replace-parallel--flatten from)
                                 (list (regexp-quote from) '()))
           collect (cons i (list from to flat groups))
           do (cl-incf i (1+ (length groups)))))

(defun query-replace-parallel--match-data (base groups)
  "With the match data of the matcher regexp active, return the
match data for the specific alternative corresponding to BASE.

BASE is the index of the group wrapped around the alternative in
the matcher regexp. GROUPS is a list of indices mapping the
successive groups (starting at BASE + 1) of the matcher regexp to
the respective groups in the original non-flat regexp of the
alternative."
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
  "Return STRING but with each backslash sequence except for `\\?'
escaped."
  (string-replace "\\\\?" "\\?" (string-replace "\\" "\\\\" string)))

(defvar query-replace-parallel--description '()
  "A list of cons cells of the form (DESC . REGEXP-FLAG),
used by our advice in order to patch the messages displayed by
`perform-replace'.

We use a list in order to support multiple reentrant invocations
of `query-replace-parallel-perform-replace', e.g. as a result of
a recursive edit initiated by the user. Each invocation binds the
variable and adds a new cons to the front.

DESC is initially nil but is mutated by each subsequent
invocation of our replacement function to be the original FROM
string provided by the user.

REGEXP-FLAG is the value of the flag passed to the invocation of
`query-replace-parallel-perform-replace'.")

(defun query-replace-parallel--replacer (table regexp-flag)
  "Construct a replacement function suitable for a call to
`perform-replace', which returns the replacement according to the
given replacement table.

If REGEXP-FLAG is nil, the replacement is taken literally.
Otherwise, the replacement can use all of the features of
`perform-replace' replacement."
  (lambda (_arg count)
    (cl-destructuring-bind (base . (from to _flat groups))
        (cl-find-if #'match-beginning table :key #'car)
      (setf (caar query-replace-parallel--description) from)
      ;; TO can either be a string or a cons. We handle the case where it's a
      ;; literal string specially to avoid computing and setting the match data.
      (if (and (stringp to) (not regexp-flag))
          ;; Escape TO so that the calling `perform-replace' takes it literally.
          (replace-quote to)
        (let ((original (match-data)))
          (set-match-data (query-replace-parallel--match-data base groups))
          (unwind-protect
              (let ((nto (cl-etypecase to
                           (string to)
                           (cons (funcall (car to) (cdr to) count)))))
                ;; We first do what `perform-replace' would normally do, i.e.
                ;; substitute any references to captured groups, but while our
                ;; custom match data is active. Then, we escape all of the
                ;; backslash sequences so that they don't get interpreted again
                ;; by the calling `perform-replace', except for `\\?' which we
                ;; leave for the caller to handle.
                (query-replace-parallel--quote
                 (match-substitute-replacement
                  nto (not (and case-replace case-fold-search)))))
            (set-match-data original)))))))

(defun query-replace-parallel--patch-noedit (args)
  (cl-destructuring-bind (newtext fixedcase literal _noedit match-data
                          &optional backward)
      args
    (list newtext fixedcase literal nil match-data backward)))

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

(defun query-replace-parallel-perform-replace
    (pairs query-flag regexp-flag delimited
     &optional map start end backward region-noncontiguous-p)
  "Perform multiple replacements given by PAIRS as if by
`perform-replace', except in parallel. That is, the replacements
are performed in a single pass and cannot erroneously replace a
previous replacement.

Each element of PAIRS has to be a cons (FROM . TO), and specifies
that occurrences of the regexp FROM should be replaced with TO.
TO can either be a string or a cons, which have the same meaning
as in `perform-replace'. Unlike `perform-replace' however, it
cannot be a list of strings, and this function omits the
`replace-count' argument.

Arguments QUERY-FLAG, REGEXP-FLAG, DELIMITED, MAP, START, END,
BACKWARD AND REGION-NONCONTIGUOUS-P are as in `perform-replace',
which see."
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
         query-flag :regexp delimited nil map start end backward
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

(defun query-replace-parallel (pairs &optional delimited start end
                                       backward region-noncontiguous-p)
  "Perform multiple replacements given by PAIRS as if by
`query-replace', except in parallel. That is, the replacements
are performed in a single pass and cannot erroneously replace a
previous replacement.

Each element of PAIRS has to be a cons (FROM . TO), and specifies
that occurrences of the string FROM should be replaced with the
string TO.

Arguments DELIMITED, START, END, BACKWARD and
REGION-NONCONTIGUOUS-P are passed to
`query-replace-parallel-perform-replace' (which see)."
  (interactive (query-replace-parallel--args nil))
  (query-replace-parallel-perform-replace
   pairs :query nil delimited nil start end backward
   region-noncontiguous-p))

(defun query-replace-parallel-regexp (pairs &optional delimited start end
                                              backward region-noncontiguous-p)
  "Perform multiple replacements given by PAIRS as if by
`query-replace-regexp', except in parallel. That is, the
replacements are performed in a single pass and cannot
erroneously replace a previous replacement.

Each element of PAIRS has to be a cons (FROM . TO), and specifies
that matches of the regexp FROM should be replaced with the
string TO, which is interpreted the same as the replacement
string in `query-replace-regexp'.

If more than one FROM regexp matches, the one appearing earlier
in the list has priority.

Arguments DELIMITED, START, END, BACKWARD and
REGION-NONCONTIGUOUS-P are passed to
`query-replace-parallel-perform-replace' (which see)."
  (interactive (query-replace-parallel--args :regexp))
  (query-replace-parallel-perform-replace
   pairs :query :regexp delimited nil start end backward
   region-noncontiguous-p))

(provide 'query-replace-parallel)
;;; query-replace-parallel.el ends here
