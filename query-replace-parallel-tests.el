;;; query-replace-parallel.el --- Tests for query-replace-parallel  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 hokomo
;; Copyright (C) 2023 Valentino Picotti

;; Author: hokomo <hokomo@disroot.org>
;;         Valentino Picotti <valentino.picotti@gmail.com>
;; Version: 0.1-pre

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

(require 'query-replace-parallel)

(require 'cl-lib)
(require 'ert)
(require 'rx)

(cl-defun query-replace-parallel--test (pairs text newtext &key (regexp-flag t))
  (let* ((table (query-replace-parallel--table pairs regexp-flag))
         (regexp (query-replace-parallel--matcher (mapcar #'cadddr table)))
         (replacer (query-replace-parallel--replacer table regexp-flag))
         ;; NOTE: Since we're testing the internals, we must also set up this
         ;; variable the way our replacer expects. We might want to think about
         ;; refactoring the implementation to get rid of this, but it's not that
         ;; big of a deal.
         (query-replace-parallel--state (list (cons nil nil))))
    (message "Pairs: %S" pairs)
    (message "Matcher: %s" regexp)
    (message "Table: %S" table)
    (message "Text: %S" text)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (cl-loop for i from 1
               while (re-search-forward regexp nil t)
               for rep = (funcall replacer nil (1- i))
               do (message "Occurrence %d: replacing %S with %S"
                           i (match-string 0) rep)
                  (replace-match rep t))
      (should (string= newtext (buffer-string))))))

(ert-deftest query-replace-parallel--swap ()
  "Swap two strings with each other."
  (query-replace-parallel--test
   '(("foo" . "bar")
     ("bar" . "foo"))
   "foo goes to bar, bar goes to foo"
   "bar goes to foo, foo goes to bar"))

(ert-deftest query-replace-parallel--cycle ()
  "Swap multiple strings in a cycle."
  (query-replace-parallel--test
   '(("foo" . "bar")
     ("bar" . "baz")
     ("baz" . "quux")
     ("quux" . "foo"))
   "foo goes to bar goes to baz goes to quux goes to foo"
   "bar goes to baz goes to quux goes to foo goes to bar"))

(ert-deftest query-replace-parallel--prefix-1 ()
  "Swap two strings where the former is a prefix of the latter."
  (query-replace-parallel--test
   '(("foo" . "baz")
     ("foobar" . "quux"))
   "foo goes to baz, foobar goes to quux"
   "baz goes to baz, bazbar goes to quux"))

(ert-deftest query-replace-parallel--prefix-2 ()
  "Swap two strings where the latter is a prefix of the former."
  (query-replace-parallel--test
   '(("foobar" . "quux")
     ("foo" . "baz"))
   "foobar goes to quux, foo goes to baz"
   "quux goes to quux, baz goes to baz"))

(ert-deftest query-replace-parallel--suffix-1 ()
  "Swap two strings where the former is a suffix of the latter."
  (query-replace-parallel--test
   '(("watch" . "wristwatch")
     ("stopwatch" . "timer"))
   "watch goes to wristwatch, stopwatch goes to timer"
   "wristwatch goes to wristwristwatch, timer goes to timer"))

(ert-deftest query-replace-parallel--suffix-2 ()
  "Swap two strings where the latter is a suffix of the former."
  (query-replace-parallel--test
   '(("stopwatch" . "timer")
     ("watch" . "wristwatch"))
   "stopwatch goes to timer, watch goes to wristwatch"
   "timer goes to timer, wristwatch goes to wristwristwatch"))

(ert-deftest query-replace-parallel--isolated ()
  "Use groups with the same number in multiple replacements."
  (query-replace-parallel--test
   `((,(rx "a" (group digit) (group digit)) . "a\\1\\2")
     (,(rx "b" (group digit) (group digit)) . "b\\2\\1"))
   "a12 b21 a12 b21 b21 a12"
   "a12 b12 a12 b12 b12 a12"))

(ert-deftest query-replace-parallel--numbered ()
  "Use explicitly numbered groups."
  (query-replace-parallel--test
   `((,(rx "a" (group-n 4 digit) (group-n 6 digit)) . "a\\6\\4")
     (,(rx "b" (group-n 6 digit) (group-n 4 digit)) . "b\\4\\6"))
   "a12 b21 a12 b21 b21 a12"
   "a21 b12 a21 b12 b12 a21"))

(ert-deftest query-replace-parallel--numbered-duplicate ()
  "Use explicitly numbered groups with duplicate numbers."
  (query-replace-parallel--test
   `((,(rx "a" (group-n 4 digit) (group-n 4 digit)) . "a\\4")
     (,(rx "b" (group-n 3 digit) (group-n 4 (group-n 3 digit))) . "b\\3"))
   "a12 b21 a12 b21 b21 a12"
   "a2 b1 a2 b1 b1 a2"))

(ert-deftest query-replace-parallel--numbered-overlapped-duplicate ()
  "Use an explicitly numbered group whose number is a duplicate of
its parent group.

The Emacs regexp engine will normally treat such a regexp as
invalid, but our implementation actually fixes the issue. See the
implementation notes in the commentary."
  (query-replace-parallel--test
   `((,(rx "a" (group digit (group-n 1 digit))) . "a\\1")
     (,(rx "b" (group (group-n 4 digit (group-n 4 digit)))) . "b\\4"))
   "a12 b21 a12 b21 b21 a12"
   "a2 b1 a2 b1 b1 a2"))

(ert-deftest query-replace-parallel--expr ()
  "Use a Lisp expression in the replacement."
  (query-replace-parallel--test
   `(,(cons (rx "1" (group (+ alnum)))
            (query-replace-compile-replacement "\\,(downcase \\1)" :regexp))
     ,(cons (rx "2" (group (+ alnum)))
            (query-replace-compile-replacement "\\,(upcase \\1)" :regexp)))
   "1foo 2bar 1baz 2quux 2bar 2foo 1baz 1quux"
   "foo BAR baz QUUX BAR FOO baz quux"))

(ert-deftest query-replace-parallel--func ()
  "Use a function that constructs the replacement."
  (query-replace-parallel--test
   `(,(cons (rx "1" (group (+ (not blank))))
            (cons (lambda (_arg _count) (match-string 1)) nil))
     ,(cons (rx "2" (group (+ (not blank))))
            (cons (lambda (_arg _count) (replace-quote (match-string 1))) nil))
     ,(cons (rx "3" (group (+ (not blank))))
            (cons (lambda (_arg _count) "\\1") nil))
     ,(cons (rx "4" (group (+ (not blank))))
            (cons (lambda (_arg _count) (replace-quote "\\1")) nil)))
   "1foo 2bar 4baz 3foo 2quux 4bar 3baz 2bar\\1 1foo\\1 4quux 3quux"
   "foo bar \\1 foo quux \\1 baz bar\\1 foofoo\\1 \\1 quux"))

(provide 'query-replace-parallel-tests)
;;; query-replace-parallel-tests.el ends here
