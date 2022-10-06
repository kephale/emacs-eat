;;; eat-tests.el --- Tests for Eat -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Keywords: tests

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains all tests for Eat.  This includes tests for the
;; terminal emulator itself.

;;; Code:

(require 'ert)
(require 'eat)
(require 'cl-lib)


;;;; Helpers.

(defun eat--tests-parse-text-properties (string)
  "Parse and modify the text properties of STRING.

Modify the text properties of STRING so that for two strings with
identical contents and similar appearance `equal-including-properties'
will return t."
  (let ((pos 0))
    (while (< pos (length string))
      (let ((next-pos (or (next-single-property-change
                           pos 'face string)
                          (length string)))
            (face (get-text-property pos 'face string)))
        (set-text-properties
         pos next-pos
         `( :foreground ,(plist-get face :foreground)
            :background ,(plist-get face :background)
            :underline-type ,(plist-get (plist-get face :underline)
                                        :style)
            :underline-color ,(plist-get (plist-get face :underline)
                                         :color)
            :crossed ,(plist-get face :strike-through)
            :intensity ,(let ((list (plist-get face :inherit)))
                          (cond ((member 'eat-term-faint list) 'faint)
                                ((member 'eat-term-bold list) 'bold)))
            :italic ,(not (not (member 'eat-term-italic
                                       (plist-get face :inherit))))
            :blink ,(let ((list (plist-get face :inherit)))
                      (cond
                       ((member 'eat-term-slow-blink list) 'slow)
                       ((member 'eat-term-fast-blink list) 'fast)))
            :font ,(let ((list (plist-get face :inherit)))
                     (cond ((member 'eat-term-font-0 list) 0)
                           ((member 'eat-term-font-1 list) 1)
                           ((member 'eat-term-font-2 list) 2)
                           ((member 'eat-term-font-3 list) 3)
                           ((member 'eat-term-font-4 list) 4)
                           ((member 'eat-term-font-5 list) 5)
                           ((member 'eat-term-font-6 list) 6)
                           ((member 'eat-term-font-7 list) 7)
                           ((member 'eat-term-font-8 list) 8)
                           ((member 'eat-term-font-9 list) 9)
                           (t 0))))
         string)
        (setq pos next-pos)))
    string))

(defun eat--tests-sanitize-expected-text (string)
  "Sanitize the text properties of expected string STRING."
  (let ((pos 0))
    (while (< pos (length string))
      (let ((next-pos (or (next-property-change pos string)
                          (length string))))
        (set-text-properties
         pos next-pos
         (mapcan
          (lambda (prop)
            (list (car prop)
                  (or (get-text-property pos (car prop) string)
                      (cdr prop))))
          '((:foreground . nil)
            (:background . nil)
            (:underline-type . nil)
            (:underline-color . nil)
            (:crossed . nil)
            (:intensity . nil)
            (:italic . nil)
            (:blink . nil)
            (:font . 0)))
         string)
        (setq pos next-pos)))
    string))

(defun eat--tests-compare-lines (actual expected)
  "Compare ACTUAL and EXPECTED."
  (let ((a (eat--tests-parse-text-properties actual))
        (b (eat--tests-sanitize-expected-text expected)))
    (cl-flet ((visually-equal (a b)
                (if (> emacs-major-version 28)
                    (equal-including-properties a b)
                  ;; On Emacs versions less than 29,
                  ;; `equal-including-properties' returns t only if
                  ;; the properties of a and b are `eq', so we compare
                  ;; the strings ourselves.
                  (and
                   (string= a b)
                   (cl-every
                    (lambda (i)
                      (cl-flet ((plist-to-alist (plist)
                                  (let ((alist nil))
                                    (while plist
                                      (push (cons (pop plist)
                                                  (pop plist))
                                            alist))
                                    (sort
                                     alist
                                     (lambda (a b)
                                       (string<
                                        (symbol-name (car a))
                                        (symbol-name (car b))))))))
                        (equal
                         (plist-to-alist (text-properties-at i a))
                         (plist-to-alist (text-properties-at i b)))))
                    (number-sequence 0 (1- (length a))))))))
      (cond ((= (length a) (length b))
             (visually-equal a b))
            ((< (length a) (length b))
             (and (visually-equal
                   a (substring b 0 (length a)))
                  (let ((str (substring b (length a))))
                    (and (string-blank-p str)
                         (visually-equal
                          str (eat--tests-parse-text-properties
                               (substring-no-properties str)))))))
            ((> (length a) (length b))
             (and (visually-equal
                   (substring a 0 (length b)) b)
                  (let ((str (substring a (length b))))
                    (and (string-blank-p str)
                         (visually-equal
                          str (eat--tests-parse-text-properties
                               (substring-no-properties str)))))))))))

(defun eat--tests-compare-scrollback (terminal lines)
  "Compare TERMINAL's scrollback buffer with LINES."
  (let ((scrollback
         (nbutlast
          (split-string
           (buffer-substring
            (point-min) (eat-term-display-beginning terminal))
           "\n" nil nil))))
    (and (or (= (eat-term-display-beginning terminal) (point-min))
             (= (char-before (eat-term-display-beginning terminal))
                ?\n))
         (= (length scrollback) (length lines))
         (cl-every (lambda (pair)
                     (eat--tests-compare-lines
                      (car pair) (cdr pair)))
                   (cl-mapcar #'cons scrollback lines)))))

(defun eat--tests-compare-display (terminal lines)
  "Compare TERMINAL's scrollback buffer with LINES."
  (let ((display
         (split-string
          (buffer-substring
           (eat-term-display-beginning terminal) (point-max))
          "\n" nil nil)))
    (and (<= (length display) (cdr (eat-term-size terminal)))
         (cl-every
          (lambda (i)
            (let ((actual (or (nth i display) ""))
                  (expected (or (nth i lines) "")))
              (and (<= (length actual) (car (eat-term-size terminal)))
                   (eat--tests-compare-lines actual expected))))
          (number-sequence 0 (1- (cdr (eat-term-size terminal))))))))

(defun eat--tests-compare-cursor-pos (terminal cursor-pos)
  "Compare TERMINAL's cursor position with CURSOR-POS.

CURSOR-POS should be a cons cell of form (Y . X)."
  (let* ((prev-lines (split-string
                      (buffer-substring
                       (eat-term-display-beginning terminal)
                       (eat-term-display-cursor terminal))
                      "\n" nil nil))
         (y (length prev-lines))
         (x (1+ (length (car (last prev-lines))))))
    (and (= (car cursor-pos) y)
         (= (cdr cursor-pos) x))))

(defun eat--tests-add-properties (string &rest intervals)
  "Add properties to STRING according to INTERVALS.

INTERVALS is a list.  Each element of it is a list of form
\((BEGIN . END) PLIST...).  For each element in INTERVALS, add PLIST
to string from BEGIN to END."
  (dolist (interval intervals)
    (set-text-properties (caar interval) (cdar interval)
                         (cdr interval) string))
  string)

(defmacro eat--tests-with-term (spec &rest body)
  "Make a temporary terminal with SPEC and run BODY with it.

SPEC is a form that should evaluate to a plist.  The plist can have
any of the following properties:

  `:width'        Width of terminal.  Defaults to 20.
  `:height'       Height of terminal.  Defaults to 6."
  (declare (indent 1))
  (let ((term (make-symbol "term"))
        (inputs (make-symbol "inputs")))
    `(with-temp-buffer
       (let ((,term (eat-term-make (current-buffer) (point)))
             (,inputs nil))
         (unwind-protect
             (progn
               (cl-destructuring-bind
                   (&key (width 20) (height 6)) ,spec
                 (eat-term-resize ,term width height))
               (setf (eat-term-input-function ,term)
                     (lambda (_ str)
                       (setq ,inputs (append ,inputs (list str)))))
               (cl-labels
                   ((terminal ()
                      ,term)
                    (output (&rest args)
                      (dolist (str args)
                        (eat-term-process-output ,term str)))
                    (pop-input ()
                      (pop ,inputs))
                    (input-empty-p ()
                      (not ,inputs))
                    (match-term (&key scrollback display cursor)
                      (and (eat--tests-compare-scrollback
                            ,term scrollback)
                           (eat--tests-compare-display ,term display)
                           (eat--tests-compare-cursor-pos
                            ,term cursor)))
                    (add-props (str &rest intervals)
                      (apply #'eat--tests-add-properties
                             str intervals)))
                 ,@body))
           (eat-term-delete ,term))))))


;;;; Tests.

;;;;; Plain Text Tests.

(ert-deftest eat-test-plain-text ()
  "Test plain text handling.

Send only plain text (i.e. no control sequences, not even newline) and
compare the output with the expected output.  Don't do test automatic
margin."
  (eat--tests-with-term '()
    (output "some test string")
    (should (match-term :display '("some test string    ")
                        :cursor '(1 . 17)))))

(ert-deftest eat-test-auto-margin ()
  "Test automatic margin.

Send only plain text (i.e. no control sequences, not even newline)
longer than the terminal width and compare the output with
the expected output."
  (eat--tests-with-term '()
    (should (match-term :cursor '(1 . 1)))
    (output "some test string... and some more")
    (should (match-term :display '("some test string..."
                                   "and some more")
                        :cursor '(2 . 14)))))


;;;;; Cursor Motion Tests.

(ert-deftest eat-test-character-tabulation ()
  "Test character tabulation control function."
  (eat--tests-with-term '()
    (output "\t")
    (should (match-term :cursor '(1 . 9)))
    (output "\t")
    (should (match-term :cursor '(1 . 17)))
    (output "\t")
    (should (match-term :cursor '(1 . 20)))
    (output "\n ")
    (should (match-term :cursor '(2 . 2)))
    (output "\t")
    (should (match-term :cursor '(2 . 9)))))

(ert-deftest eat-test-cursor-backward-tabulation ()
  "Test cursor backward tabulation control function."
  (eat--tests-with-term '()
    (output "\t")
    (should (match-term :cursor '(1 . 9)))
    (output "\t")
    (should (match-term :cursor '(1 . 17)))
    (output "\t")
    (should (match-term :cursor '(1 . 20)))
    (output "\n ")
    (should (match-term :cursor '(2 . 2)))
    (output "\t")
    (should (match-term :cursor '(2 . 9)))))

(ert-deftest eat-test-line-tabulation ()
  "Test line tabulation control function."
  (eat--tests-with-term '()
    (output "\v")
    (should (match-term :cursor '(2 . 1)))
    (output "  ")
    (should (match-term :cursor '(2 . 3)))
    (output "\v")
    (should (match-term :cursor '(3 . 3)))
    (output "\v\v\v")
    (should (match-term :cursor '(6 . 3)))
    (output "\v")
    (should (match-term :scrollback '("")
                        :cursor '(6 . 3)))))

(ert-deftest eat-test-form-feed ()
  "Test form feed."
  (eat--tests-with-term '()
    (output "\f")
    (should (match-term :cursor '(2 . 1)))
    (output "  ")
    (should (match-term :cursor '(2 . 3)))
    (output "\f")
    (should (match-term :cursor '(3 . 3)))
    (output "\f\f\f")
    (should (match-term :cursor '(6 . 3)))
    (output "\f")
    (should (match-term :scrollback '("")
                        :cursor '(6 . 3)))))

(ert-deftest eat-test-line-feed ()
  "Test line feed control function."
  (eat--tests-with-term '()
    (output "\n")
    (should (match-term :cursor '(2 . 1)))
    (output "    \n")
    (should (match-term :cursor '(3 . 1)))
    (output "\eE")
    (should (match-term :cursor '(4 . 1)))
    (output "    \eE")
    (should (match-term :cursor '(5 . 1)))))

(ert-deftest eat-test-reverse-line-feed ()
  "Test reverse line feed control function.

Use newlines to move to an initial position from where the control
function is to be invoked."
  (eat--tests-with-term '()
    (output "\n\n")
    (should (match-term :cursor '(3 . 1)))
    (output "\eM")
    (should (match-term :cursor '(2 . 1)))
    (output "    \eM")
    (should (match-term :cursor '(1 . 5)))))

(ert-deftest eat-test-backspace ()
  "Test backspace control function.

Use spaces to move to an initial position from where the control
function is to be invoked."
  (eat--tests-with-term '()
    (output " ")
    (should (match-term :cursor '(1 . 2)))
    (output "\b")
    (should (match-term :cursor '(1 . 1)))))

(ert-deftest eat-test-carriage-return ()
  "Test carriage return control function.

Use spaces to move to an initial position from where the control
function is to be invoked."
  (eat--tests-with-term '()
    (output "\r")
    (should (match-term :cursor '(1 . 1)))
    (output "    ")
    (should (match-term :cursor '(1 . 5)))
    (output "\r")
    (should (match-term :cursor '(1 . 1)))))

(ert-deftest eat-test-cursor-right ()
  "Test cursor right control function."
  (eat--tests-with-term '()
    (output "\e[C")
    (should (match-term :cursor '(1 . 2)))
    (output "\e[0C")
    (should (match-term :cursor '(1 . 3)))
    (output "\e[5C")
    (should (match-term :cursor '(1 . 8)))
    (output "\e[20C")
    (should (match-term :cursor '(1 . 20)))))

(ert-deftest eat-test-cursor-left ()
  "Test cursor up control function.

Use spaces to move to an initial position from where the control
function is to be invoked."
  (eat--tests-with-term '()
    (output "                ")
    (should (match-term :cursor '(1 . 17)))
    (output "\e[D")
    (should (match-term :cursor '(1 . 16)))
    (output "\e[0D")
    (should (match-term :cursor '(1 . 15)))
    (output "\e[7D")
    (should (match-term :cursor '(1 . 8)))
    (output "\e[10D")
    (should (match-term :cursor '(1 . 1)))))

(ert-deftest eat-test-cursor-down ()
  "Test cursor down control function."
  (eat--tests-with-term '()
    (output "\e[B")
    (should (match-term :cursor '(2 . 1)))
    (output "\e[0B")
    (should (match-term :cursor '(3 . 1)))
    (output "    ")
    (should (match-term :cursor '(3 . 5)))
    (output "\e[6B")
    (should (match-term :cursor '(6 . 5)))))

(ert-deftest eat-test-cursor-up ()
  "Test cursor up control function.

Use spaces and newlines to move to an initial position from where the
control function is to be invoked."
  (eat--tests-with-term '()
    (output "\n\n\n\n\n")
    (should (match-term :cursor '(6 . 1)))
    (output "\e[A")
    (should (match-term :cursor '(5 . 1)))
    (output "\e[0A")
    (should (match-term :cursor '(4 . 1)))
    (output "    ")
    (should (match-term :cursor '(4 . 5)))
    (output "\e[2A")
    (should (match-term :cursor '(2 . 5)))
    (output "\e[4A")
    (should (match-term :cursor '(1 . 5)))))

(ert-deftest eat-test-cursor-next-line ()
  "Test cursor next line control function."
  (eat--tests-with-term '(:height 10)
    (output "\e[F")
    (should (match-term :cursor '(2 . 1)))
    (output "\e[0F")
    (should (match-term :cursor '(3 . 1)))
    (output "\e[2F")
    (should (match-term :cursor '(5 . 1)))
    (output "    \e[F")
    (should (match-term :cursor '(6 . 1)))
    (output "        \e[0F")
    (should (match-term :cursor '(7 . 1)))
    (output "  \e[3F")
    (should (match-term :cursor '(10 . 1)))
    (output "   \e[F")
    (should (match-term :cursor '(10 . 1)))))

(ert-deftest eat-test-cursor-previous-line ()
  "Test cursor previous line control function.

Use newlines to move to an initial position from where the control
function is to be invoked."
  (eat--tests-with-term '(:height 10)
    (output "\n\n\n\n\n\n\n\n\n" )
    (should (match-term :cursor '(10 . 1)))
    (output "\e[E")
    (should (match-term :cursor '(9 . 1)))
    (output "\e[0E")
    (should (match-term :cursor '(8 . 1)))
    (output "\e[2E")
    (should (match-term :cursor '(6 . 1)))
    (output "    \e[E")
    (should (match-term :cursor '(5 . 1)))
    (output "        \e[0E")
    (should (match-term :cursor '(4 . 1)))
    (output "  \e[3E")
    (should (match-term :cursor '(1 . 1)))
    (output "   \e[E")
    (should (match-term :cursor '(1 . 1)))))

(ert-deftest eat-test-horizontal-position-absolute ()
  "Test horizontal position absolute control function."
  (eat--tests-with-term '()
    (output "\e[5G")
    (should (match-term :cursor '(1 . 5)))
    (output "\e[0G")
    (should (match-term :cursor '(1 . 1)))
    (output "\e[15G")
    (should (match-term :cursor '(1 . 15)))
    (output "\e[G")
    (should (match-term :cursor '(1 . 1)))))

(ert-deftest eat-test-line-position-absolute ()
  "Test line position absolute control function."
  (eat--tests-with-term '()
    (output "\e[2d")
    (should (match-term :cursor '(2 . 1)))
    (output "\e[0d")
    (should (match-term :cursor '(1 . 1)))
    (output "\e[5d")
    (should (match-term :cursor '(5 . 1)))
    (output "\e[d")
    (should (match-term :cursor '(1 . 1)))))

(ert-deftest eat-test-cursor-position ()
  "Test cursor position control function."
  (eat--tests-with-term '()
    (output "\e[2;2H")
    (should (match-term :cursor '(2 . 2)))
    (output "\e[;5H")
    (should (match-term :cursor '(1 . 5)))
    (output "\e[4;H")
    (should (match-term :cursor '(4 . 1)))
    (output "\e[7;H")
    (should (match-term :cursor '(6 . 1)))
    (output "\e[0;0H")
    (should (match-term :cursor '(1 . 1)))
    (output "\e[;30H")
    (should (match-term :cursor '(1 . 20)))
    (output "\e[10;25H")
    (should (match-term :cursor '(6 . 20)))
    (output "\e[H")
    (should (match-term :cursor '(1 . 1)))))

(ert-deftest eat-test-character-and-line-position ()
  "Test character and line position control function."
  (eat--tests-with-term '()
    (output "\e[2;2f")
    (should (match-term :cursor '(2 . 2)))
    (output "\e[;5f")
    (should (match-term :cursor '(1 . 5)))
    (output "\e[4;f")
    (should (match-term :cursor '(4 . 1)))
    (output "\e[7;f")
    (should (match-term :cursor '(6 . 1)))
    (output "\e[0;0f")
    (should (match-term :cursor '(1 . 1)))
    (output "\e[;30f")
    (should (match-term :cursor '(1 . 20)))
    (output "\e[10;25f")
    (should (match-term :cursor '(6 . 20)))
    (output "\e[f")
    (should (match-term :cursor '(1 . 1)))))


;;;;; Scrolling Tests.

(ert-deftest eat-test-scroll-up ()
  "Test scroll up control function."
  (eat--tests-with-term '()
    (output "some test string...\nmore, more...\n"
            "more, more, more...\nand some more")
    (should (match-term :display '("some test string..."
                                   "more, more..."
                                   "more, more, more..."
                                   "and some more")
                        :cursor '(4 . 14)))
    (output "\e[S")
    (should (match-term :scrollback '("some test string...")
                        :display '("more, more..."
                                   "more, more, more..."
                                   "and some more")
                        :cursor '(4 . 14)))
    (output "\e[0S")
    (should (match-term :scrollback '("some test string...")
                        :display '("more, more..."
                                   "more, more, more..."
                                   "and some more")
                        :cursor '(4 . 14)))
    (output "\e[2S")
    (should (match-term :scrollback '("some test string..."
                                      "more, more..."
                                      "more, more, more...")
                        :display '("and some more")
                        :cursor '(4 . 14)))
    (output "\nnew line 1\nnew line 2\nnew line 3\n"
            "new line 4\nnew line 5\nnew line 6\e[2;5r")
    (should (match-term :scrollback '("some test string..."
                                      "more, more..."
                                      "more, more, more..."
                                      "and some more"
                                      ""
                                      ""
                                      "")
                        :display '("new line 1"
                                   "new line 2"
                                   "new line 3"
                                   "new line 4"
                                   "new line 5"
                                   "new line 6")
                        :cursor '(1 . 1)))
    (output "\e[S")
    (should (match-term :scrollback '("some test string..."
                                      "more, more..."
                                      "more, more, more..."
                                      "and some more"
                                      ""
                                      ""
                                      "")
                        :display '("new line 1"
                                   "new line 3"
                                   "new line 4"
                                   "new line 5"
                                   ""
                                   "new line 6")
                        :cursor '(1 . 1)))
    (output "\e[0S")
    (should (match-term :scrollback '("some test string..."
                                      "more, more..."
                                      "more, more, more..."
                                      "and some more"
                                      ""
                                      ""
                                      "")
                        :display '("new line 1"
                                   "new line 3"
                                   "new line 4"
                                   "new line 5"
                                   ""
                                   "new line 6")
                        :cursor '(1 . 1)))
    (output "\e[2S")
    (should (match-term :scrollback '("some test string..."
                                      "more, more..."
                                      "more, more, more..."
                                      "and some more"
                                      ""
                                      ""
                                      "")
                        :display '("new line 1"
                                   "new line 5"
                                   ""
                                   ""
                                   ""
                                   "new line 6")
                        :cursor '(1 . 1)))))

(ert-deftest eat-test-scroll-down ()
  "Test scroll down control function."
  (eat--tests-with-term '()
    (output "some test string...\nmore, more...\n"
            "more, more, more...\nand some more")
    (should (match-term :display '("some test string..."
                                   "more, more..."
                                   "more, more, more..."
                                   "and some more")
                        :cursor '(4 . 14)))
    (output "\e[T")
    (should (match-term :display '(""
                                   "some test string..."
                                   "more, more..."
                                   "more, more, more..."
                                   "and some more")
                        :cursor '(4 . 14)))
    (output "\e[0T")
    (should (match-term :display '(""
                                   "some test string..."
                                   "more, more..."
                                   "more, more, more..."
                                   "and some more")
                        :cursor '(4 . 14)))
    (output "\e[2T")
    (should (match-term :display '(""
                                   ""
                                   ""
                                   "some test string..."
                                   "more, more..."
                                   "more, more, more...")
                        :cursor '(4 . 14)))
    (output "\n\n\nnew line 1\nnew line 2\nnew line 3\n"
            "new line 4\nnew line 5\nnew line 6\e[2;5r")
    (should (match-term :scrollback '(""
                                      ""
                                      ""
                                      "some test string..."
                                      "more, more..."
                                      "more, more, more...")
                        :display '("new line 1"
                                   "new line 2"
                                   "new line 3"
                                   "new line 4"
                                   "new line 5"
                                   "new line 6")
                        :cursor '(1 . 1)))
    (output "\e[T")
    (should (match-term :scrollback '(""
                                      ""
                                      ""
                                      "some test string..."
                                      "more, more..."
                                      "more, more, more...")
                        :display '("new line 1"
                                   ""
                                   "new line 2"
                                   "new line 3"
                                   "new line 4"
                                   "new line 6")
                        :cursor '(1 . 1)))
    (output "\e[0T")
    (should (match-term :scrollback '(""
                                      ""
                                      ""
                                      "some test string..."
                                      "more, more..."
                                      "more, more, more...")
                        :display '("new line 1"
                                   ""
                                   "new line 2"
                                   "new line 3"
                                   "new line 4"
                                   "new line 6")
                        :cursor '(1 . 1)))
    (output "\e[2T")
    (should (match-term :scrollback '(""
                                      ""
                                      ""
                                      "some test string..."
                                      "more, more..."
                                      "more, more, more...")
                        :display '("new line 1"
                                   ""
                                   ""
                                   ""
                                   "new line 2"
                                   "new line 6")
                        :cursor '(1 . 1)))))

(ert-deftest eat-test-auto-scrolling ()
  "Test automatic scrolling when cursor reaches end of display.

Test with every control functions and text combination that trigger
automatic scrolling as a side effect."
  (eat--tests-with-term '()
    ;; Test with newlines.
    (output "some test string...\n\n\n\n\n\nand some more")
    (should (match-term :scrollback '("some test string...")
                        :display '(""
                                   ""
                                   ""
                                   ""
                                   ""
                                   "and some more")
                        :cursor '(6 . 14)))
    ;; Test with automatic margin.
    (output "...more, more, stop.")
    (should (match-term :scrollback '("some test string..."
                                      "")
                        :display '(""
                                   ""
                                   ""
                                   ""
                                   "and some more...more"
                                   ", more, stop.")
                        :cursor '(6 . 14)))
    ;; Test with reverse index.
    (output "\eM\eM\eM\eM\eM\eM")
    (should (match-term :scrollback '("some test string..."
                                      "")
                        :display '(""
                                   ""
                                   ""
                                   ""
                                   ""
                                   "and some more...more")
                        :cursor '(1 . 14)))
    ;; Test with newlines and scroll region.
    (output "\e[2;5rline 1\nline 2\nline 3\nline 4\nline 5")
    (should (match-term :scrollback '("some test string..."
                                      "")
                        :display '("line 1"
                                   "line 2"
                                   "line 3"
                                   "line 4"
                                   "line 5"
                                   "and some more...more")
                        :cursor '(5 . 7)))
    (output "\n")
    (should (match-term :scrollback '("some test string..."
                                      "")
                        :display '("line 1"
                                   "line 3"
                                   "line 4"
                                   "line 5"
                                   ""
                                   "and some more...more")
                        :cursor '(5 . 1)))
    ;; Test with automatic margin and scroll region.
    (output "...more content, more, stop.")
    (should (match-term :scrollback '("some test string..."
                                      "")
                        :display '("line 1"
                                   "line 4"
                                   "line 5"
                                   "...more content, mor"
                                   "e, stop."
                                   "and some more...more")
                        :cursor '(5 . 9)))
    ;; Test with reverse index and scroll region.
    (output "\eM\eM\eM\eM\eM")
    (should (match-term :scrollback '("some test string..."
                                      "")
                        :display '("line 1"
                                   ""
                                   ""
                                   "line 4"
                                   "line 5"
                                   "and some more...more")
                        :cursor '(2 . 9)))))


;;;;; SGR Tests.

;; Beware, this section is a nightware of code repetition.

(ert-deftest eat-test-sgr-foreground ()
  "Test SGR foreground color.  Test 256 colors and 24-bit colors."
  (eat--tests-with-term '()
    ;; ANSI colors.
    (output "\e[31mred\n")
    (should (match-term
             :display `(,(add-props
                          "red"
                          `((0 . 3)
                            :foreground ,(face-foreground
                                          'eat-term-color-1 nil t))))
             :cursor '(2 . 1)))
    (output "\e[32mgreen\n")
    (should (match-term
             :display `(,(add-props
                          "red"
                          `((0 . 3)
                            :foreground ,(face-foreground
                                          'eat-term-color-1 nil t)))
                        ,(add-props
                          "green"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-2 nil t))))
             :cursor '(3 . 1)))
    (output "\e[37mwhite\n")
    (should (match-term
             :display `(,(add-props
                          "red"
                          `((0 . 3)
                            :foreground ,(face-foreground
                                          'eat-term-color-1 nil t)))
                        ,(add-props
                          "green"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-2 nil t)))
                        ,(add-props
                          "white"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-7 nil t))))
             :cursor '(4 . 1)))
    (output "\e[30mblack\n")
    (should (match-term
             :display `(,(add-props
                          "red"
                          `((0 . 3)
                            :foreground ,(face-foreground
                                          'eat-term-color-1 nil t)))
                        ,(add-props
                          "green"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-2 nil t)))
                        ,(add-props
                          "white"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-7 nil t)))
                        ,(add-props
                          "black"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-0 nil t))))
             :cursor '(5 . 1)))
    ;; ANSI bright colors.
    (output "\e[91mbright red\n")
    (should (match-term
             :display `(,(add-props
                          "red"
                          `((0 . 3)
                            :foreground ,(face-foreground
                                          'eat-term-color-1 nil t)))
                        ,(add-props
                          "green"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-2 nil t)))
                        ,(add-props
                          "white"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-7 nil t)))
                        ,(add-props
                          "black"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-0 nil t)))
                        ,(add-props
                          "bright red"
                          `((0 . 10)
                            :foreground ,(face-foreground
                                          'eat-term-color-9 nil t))))
             :cursor '(6 . 1)))
    (output "\e[92mbright green\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t))))
             :display `(,(add-props
                          "green"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-2 nil t)))
                        ,(add-props
                          "white"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-7 nil t)))
                        ,(add-props
                          "black"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-0 nil t)))
                        ,(add-props
                          "bright red"
                          `((0 . 10)
                            :foreground ,(face-foreground
                                          'eat-term-color-9 nil t)))
                        ,(add-props
                          "bright green"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-10 nil t))))
             :cursor '(6 . 1)))
    (output "\e[97mbright white\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t))))
             :display `(,(add-props
                          "white"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-7 nil t)))
                        ,(add-props
                          "black"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-0 nil t)))
                        ,(add-props
                          "bright red"
                          `((0 . 10)
                            :foreground ,(face-foreground
                                          'eat-term-color-9 nil t)))
                        ,(add-props
                          "bright green"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-10 nil t)))
                        ,(add-props
                          "bright white"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-15 nil t))))
             :cursor '(6 . 1)))
    (output "\e[90mbright black\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t))))
             :display `(,(add-props
                          "black"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-0 nil t)))
                        ,(add-props
                          "bright red"
                          `((0 . 10)
                            :foreground ,(face-foreground
                                          'eat-term-color-9 nil t)))
                        ,(add-props
                          "bright green"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-10 nil t)))
                        ,(add-props
                          "bright white"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-15 nil t)))
                        ,(add-props
                          "bright black"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-8 nil t))))
             :cursor '(6 . 1)))
    ;; ANSI colors using 256-color sequence.
    (output "\e[38;5;1mred\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-0
                                             nil t))))
             :display `(,(add-props
                          "bright red"
                          `((0 . 10)
                            :foreground ,(face-foreground
                                          'eat-term-color-9 nil t)))
                        ,(add-props
                          "bright green"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-10 nil t)))
                        ,(add-props
                          "bright white"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-15 nil t)))
                        ,(add-props
                          "bright black"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-8 nil t)))
                        ,(add-props
                          "red"
                          `((0 . 3)
                            :foreground ,(face-foreground
                                          'eat-term-color-1 nil t))))
             :cursor '(6 . 1)))
    (output "\e[38;5;2mgreen\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :foreground ,(face-foreground
                                             'eat-term-color-9
                                             nil t))))
             :display `(,(add-props
                          "bright green"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-10 nil t)))
                        ,(add-props
                          "bright white"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-15 nil t)))
                        ,(add-props
                          "bright black"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-8 nil t)))
                        ,(add-props
                          "red"
                          `((0 . 3)
                            :foreground ,(face-foreground
                                          'eat-term-color-1 nil t)))
                        ,(add-props
                          "green"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-2 nil t))))
             :cursor '(6 . 1)))
    (output "\e[38;5;7mwhite\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :foreground ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-10
                                             nil t))))
             :display `(,(add-props
                          "bright white"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-15 nil t)))
                        ,(add-props
                          "bright black"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-8 nil t)))
                        ,(add-props
                          "red"
                          `((0 . 3)
                            :foreground ,(face-foreground
                                          'eat-term-color-1 nil t)))
                        ,(add-props
                          "green"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-2 nil t)))
                        ,(add-props
                          "white"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-7 nil t))))
             :cursor '(6 . 1)))
    (output "\e[38;5;0mblack\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :foreground ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-15
                                             nil t))))
             :display `(,(add-props
                          "bright black"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-8 nil t)))
                        ,(add-props
                          "red"
                          `((0 . 3)
                            :foreground ,(face-foreground
                                          'eat-term-color-1 nil t)))
                        ,(add-props
                          "green"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-2 nil t)))
                        ,(add-props
                          "white"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-7 nil t)))
                        ,(add-props
                          "black"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-0 nil t))))
             :cursor '(6 . 1)))
    ;; ANSI bright colors using 256-color sequence.
    (output "\e[38;5;9mbright red\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :foreground ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-8
                                             nil t))))
             :display `(,(add-props
                          "red"
                          `((0 . 3)
                            :foreground ,(face-foreground
                                          'eat-term-color-1 nil t)))
                        ,(add-props
                          "green"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-2 nil t)))
                        ,(add-props
                          "white"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-7 nil t)))
                        ,(add-props
                          "black"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-0 nil t)))
                        ,(add-props
                          "bright red"
                          `((0 . 10)
                            :foreground ,(face-foreground
                                          'eat-term-color-9 nil t))))
             :cursor '(6 . 1)))
    (output "\e[38;5;10mbright green\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :foreground ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-8
                                             nil t)))
                           ,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t))))
             :display `(,(add-props
                          "green"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-2 nil t)))
                        ,(add-props
                          "white"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-7 nil t)))
                        ,(add-props
                          "black"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-0 nil t)))
                        ,(add-props
                          "bright red"
                          `((0 . 10)
                            :foreground ,(face-foreground
                                          'eat-term-color-9 nil t)))
                        ,(add-props
                          "bright green"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-10 nil t))))
             :cursor '(6 . 1)))
    (output "\e[38;5;15mbright white\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :foreground ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-8
                                             nil t)))
                           ,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t))))
             :display `(,(add-props
                          "white"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-7 nil t)))
                        ,(add-props
                          "black"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-0 nil t)))
                        ,(add-props
                          "bright red"
                          `((0 . 10)
                            :foreground ,(face-foreground
                                          'eat-term-color-9 nil t)))
                        ,(add-props
                          "bright green"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-10 nil t)))
                        ,(add-props
                          "bright white"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-15 nil t))))
             :cursor '(6 . 1)))
    (output "\e[38;5;8mbright black\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :foreground ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-8
                                             nil t)))
                           ,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t))))
             :display `(,(add-props
                          "black"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-0 nil t)))
                        ,(add-props
                          "bright red"
                          `((0 . 10)
                            :foreground ,(face-foreground
                                          'eat-term-color-9 nil t)))
                        ,(add-props
                          "bright green"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-10 nil t)))
                        ,(add-props
                          "bright white"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-15 nil t)))
                        ,(add-props
                          "bright black"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-8 nil t))))
             :cursor '(6 . 1)))
    ;; 256-color.
    (output "\e[38;5;119mcolor-119\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :foreground ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-8
                                             nil t)))
                           ,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-0
                                             nil t))))
             :display `(,(add-props
                          "bright red"
                          `((0 . 10)
                            :foreground ,(face-foreground
                                          'eat-term-color-9 nil t)))
                        ,(add-props
                          "bright green"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-10 nil t)))
                        ,(add-props
                          "bright white"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-15 nil t)))
                        ,(add-props
                          "bright black"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-8 nil t)))
                        ,(add-props
                          "color-119"
                          `((0 . 9)
                            :foreground ,(face-foreground
                                          'eat-term-color-119 nil t))))
             :cursor '(6 . 1)))
    (output "\e[38;5;255mcolor-255\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :foreground ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-8
                                             nil t)))
                           ,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :foreground ,(face-foreground
                                             'eat-term-color-9
                                             nil t))))
             :display `(,(add-props
                          "bright green"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-10 nil t)))
                        ,(add-props
                          "bright white"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-15 nil t)))
                        ,(add-props
                          "bright black"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-8 nil t)))
                        ,(add-props
                          "color-119"
                          `((0 . 9)
                            :foreground ,(face-foreground
                                          'eat-term-color-119 nil t)))
                        ,(add-props
                          "color-255"
                          `((0 . 9)
                            :foreground ,(face-foreground
                                          'eat-term-color-255 nil t))))
             :cursor '(6 . 1)))
    ;; 24-bit color (truecolor).
    (output "\e[38;2;34;139;34mforest green\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :foreground ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-8
                                             nil t)))
                           ,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :foreground ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-10
                                             nil t))))
             :display `(,(add-props
                          "bright white"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-15 nil t)))
                        ,(add-props
                          "bright black"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-8 nil t)))
                        ,(add-props
                          "color-119"
                          `((0 . 9)
                            :foreground ,(face-foreground
                                          'eat-term-color-119 nil t)))
                        ,(add-props
                          "color-255"
                          `((0 . 9)
                            :foreground ,(face-foreground
                                          'eat-term-color-255 nil t)))
                        ,(add-props
                          "forest green"
                          `((0 . 12)
                            :foreground "#228b22")))
             :cursor '(6 . 1)))
    (output "\e[38;2;160;32;240mpurple\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :foreground ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-8
                                             nil t)))
                           ,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :foreground ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-15
                                             nil t))))
             :display `(,(add-props
                          "bright black"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-8 nil t)))
                        ,(add-props
                          "color-119"
                          `((0 . 9)
                            :foreground ,(face-foreground
                                          'eat-term-color-119 nil t)))
                        ,(add-props
                          "color-255"
                          `((0 . 9)
                            :foreground ,(face-foreground
                                          'eat-term-color-255 nil t)))
                        ,(add-props
                          "forest green"
                          `((0 . 12)
                            :foreground "#228b22"))
                        ,(add-props
                          "purple"
                          `((0 . 6)
                            :foreground "#a020f0")))
             :cursor '(6 . 1)))
    (output "\e[39mdefault\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :foreground ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-8
                                             nil t)))
                           ,(add-props
                             "red"
                             `((0 . 3)
                               :foreground ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :foreground ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :foreground ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :foreground ,(face-foreground
                                             'eat-term-color-8
                                             nil t))))
             :display `(,(add-props
                          "color-119"
                          `((0 . 9)
                            :foreground ,(face-foreground
                                          'eat-term-color-119 nil t)))
                        ,(add-props
                          "color-255"
                          `((0 . 9)
                            :foreground ,(face-foreground
                                          'eat-term-color-255 nil t)))
                        ,(add-props
                          "forest green"
                          `((0 . 12)
                            :foreground "#228b22"))
                        ,(add-props
                          "purple"
                          `((0 . 6)
                            :foreground "#a020f0"))
                        "default")
             :cursor '(6 . 1)))))

(ert-deftest eat-test-sgr-background ()
  "Test SGR background color.  Test 256 colors and 24-bit colors."
  (eat--tests-with-term '()
    ;; ANSI colors.
    (output "\e[41mred\n")
    (should (match-term
             :display `(,(add-props
                          "red"
                          `((0 . 3)
                            :background ,(face-foreground
                                          'eat-term-color-1 nil t))))
             :cursor '(2 . 1)))
    (output "\e[42mgreen\n")
    (should (match-term
             :display `(,(add-props
                          "red"
                          `((0 . 3)
                            :background ,(face-foreground
                                          'eat-term-color-1 nil t)))
                        ,(add-props
                          "green"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-2 nil t))))
             :cursor '(3 . 1)))
    (output "\e[47mwhite\n")
    (should (match-term
             :display `(,(add-props
                          "red"
                          `((0 . 3)
                            :background ,(face-foreground
                                          'eat-term-color-1 nil t)))
                        ,(add-props
                          "green"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-2 nil t)))
                        ,(add-props
                          "white"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-7 nil t))))
             :cursor '(4 . 1)))
    (output "\e[40mblack\n")
    (should (match-term
             :display `(,(add-props
                          "red"
                          `((0 . 3)
                            :background ,(face-foreground
                                          'eat-term-color-1 nil t)))
                        ,(add-props
                          "green"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-2 nil t)))
                        ,(add-props
                          "white"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-7 nil t)))
                        ,(add-props
                          "black"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-0 nil t))))
             :cursor '(5 . 1)))
    ;; ANSI bright colors.
    (output "\e[101mbright red\n")
    (should (match-term
             :display `(,(add-props
                          "red"
                          `((0 . 3)
                            :background ,(face-foreground
                                          'eat-term-color-1 nil t)))
                        ,(add-props
                          "green"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-2 nil t)))
                        ,(add-props
                          "white"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-7 nil t)))
                        ,(add-props
                          "black"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-0 nil t)))
                        ,(add-props
                          "bright red"
                          `((0 . 10)
                            :background ,(face-foreground
                                          'eat-term-color-9 nil t))))
             :cursor '(6 . 1)))
    (output "\e[102mbright green\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t))))
             :display `(,(add-props
                          "green"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-2 nil t)))
                        ,(add-props
                          "white"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-7 nil t)))
                        ,(add-props
                          "black"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-0 nil t)))
                        ,(add-props
                          "bright red"
                          `((0 . 10)
                            :background ,(face-foreground
                                          'eat-term-color-9 nil t)))
                        ,(add-props
                          "bright green"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-10 nil t))))
             :cursor '(6 . 1)))
    (output "\e[107mbright white\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t))))
             :display `(,(add-props
                          "white"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-7 nil t)))
                        ,(add-props
                          "black"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-0 nil t)))
                        ,(add-props
                          "bright red"
                          `((0 . 10)
                            :background ,(face-foreground
                                          'eat-term-color-9 nil t)))
                        ,(add-props
                          "bright green"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-10 nil t)))
                        ,(add-props
                          "bright white"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-15 nil t))))
             :cursor '(6 . 1)))
    (output "\e[100mbright black\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t))))
             :display `(,(add-props
                          "black"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-0 nil t)))
                        ,(add-props
                          "bright red"
                          `((0 . 10)
                            :background ,(face-foreground
                                          'eat-term-color-9 nil t)))
                        ,(add-props
                          "bright green"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-10 nil t)))
                        ,(add-props
                          "bright white"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-15 nil t)))
                        ,(add-props
                          "bright black"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-8 nil t))))
             :cursor '(6 . 1)))
    ;; ANSI colors using 256-color sequence.
    (output "\e[48;5;1mred\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-0
                                             nil t))))
             :display `(,(add-props
                          "bright red"
                          `((0 . 10)
                            :background ,(face-foreground
                                          'eat-term-color-9 nil t)))
                        ,(add-props
                          "bright green"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-10 nil t)))
                        ,(add-props
                          "bright white"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-15 nil t)))
                        ,(add-props
                          "bright black"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-8 nil t)))
                        ,(add-props
                          "red"
                          `((0 . 3)
                            :background ,(face-foreground
                                          'eat-term-color-1 nil t))))
             :cursor '(6 . 1)))
    (output "\e[48;5;2mgreen\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :background ,(face-foreground
                                             'eat-term-color-9
                                             nil t))))
             :display `(,(add-props
                          "bright green"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-10 nil t)))
                        ,(add-props
                          "bright white"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-15 nil t)))
                        ,(add-props
                          "bright black"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-8 nil t)))
                        ,(add-props
                          "red"
                          `((0 . 3)
                            :background ,(face-foreground
                                          'eat-term-color-1 nil t)))
                        ,(add-props
                          "green"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-2 nil t))))
             :cursor '(6 . 1)))
    (output "\e[48;5;7mwhite\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :background ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-10
                                             nil t))))
             :display `(,(add-props
                          "bright white"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-15 nil t)))
                        ,(add-props
                          "bright black"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-8 nil t)))
                        ,(add-props
                          "red"
                          `((0 . 3)
                            :background ,(face-foreground
                                          'eat-term-color-1 nil t)))
                        ,(add-props
                          "green"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-2 nil t)))
                        ,(add-props
                          "white"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-7 nil t))))
             :cursor '(6 . 1)))
    (output "\e[48;5;0mblack\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :background ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-15
                                             nil t))))
             :display `(,(add-props
                          "bright black"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-8 nil t)))
                        ,(add-props
                          "red"
                          `((0 . 3)
                            :background ,(face-foreground
                                          'eat-term-color-1 nil t)))
                        ,(add-props
                          "green"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-2 nil t)))
                        ,(add-props
                          "white"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-7 nil t)))
                        ,(add-props
                          "black"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-0 nil t))))
             :cursor '(6 . 1)))
    ;; ANSI bright colors using 256-color sequence.
    (output "\e[48;5;9mbright red\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :background ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-8
                                             nil t))))
             :display `(,(add-props
                          "red"
                          `((0 . 3)
                            :background ,(face-foreground
                                          'eat-term-color-1 nil t)))
                        ,(add-props
                          "green"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-2 nil t)))
                        ,(add-props
                          "white"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-7 nil t)))
                        ,(add-props
                          "black"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-0 nil t)))
                        ,(add-props
                          "bright red"
                          `((0 . 10)
                            :background ,(face-foreground
                                          'eat-term-color-9 nil t))))
             :cursor '(6 . 1)))
    (output "\e[48;5;10mbright green\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :background ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-8
                                             nil t)))
                           ,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t))))
             :display `(,(add-props
                          "green"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-2 nil t)))
                        ,(add-props
                          "white"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-7 nil t)))
                        ,(add-props
                          "black"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-0 nil t)))
                        ,(add-props
                          "bright red"
                          `((0 . 10)
                            :background ,(face-foreground
                                          'eat-term-color-9 nil t)))
                        ,(add-props
                          "bright green"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-10 nil t))))
             :cursor '(6 . 1)))
    (output "\e[48;5;15mbright white\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :background ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-8
                                             nil t)))
                           ,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t))))
             :display `(,(add-props
                          "white"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-7 nil t)))
                        ,(add-props
                          "black"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-0 nil t)))
                        ,(add-props
                          "bright red"
                          `((0 . 10)
                            :background ,(face-foreground
                                          'eat-term-color-9 nil t)))
                        ,(add-props
                          "bright green"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-10 nil t)))
                        ,(add-props
                          "bright white"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-15 nil t))))
             :cursor '(6 . 1)))
    (output "\e[48;5;8mbright black\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :background ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-8
                                             nil t)))
                           ,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t))))
             :display `(,(add-props
                          "black"
                          `((0 . 5)
                            :background ,(face-foreground
                                          'eat-term-color-0 nil t)))
                        ,(add-props
                          "bright red"
                          `((0 . 10)
                            :background ,(face-foreground
                                          'eat-term-color-9 nil t)))
                        ,(add-props
                          "bright green"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-10 nil t)))
                        ,(add-props
                          "bright white"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-15 nil t)))
                        ,(add-props
                          "bright black"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-8 nil t))))
             :cursor '(6 . 1)))
    ;; 256-color.
    (output "\e[48;5;119mcolor-119\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :background ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-8
                                             nil t)))
                           ,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-0
                                             nil t))))
             :display `(,(add-props
                          "bright red"
                          `((0 . 10)
                            :background ,(face-foreground
                                          'eat-term-color-9 nil t)))
                        ,(add-props
                          "bright green"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-10 nil t)))
                        ,(add-props
                          "bright white"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-15 nil t)))
                        ,(add-props
                          "bright black"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-8 nil t)))
                        ,(add-props
                          "color-119"
                          `((0 . 9)
                            :background ,(face-foreground
                                          'eat-term-color-119 nil t))))
             :cursor '(6 . 1)))
    (output "\e[48;5;255mcolor-255\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :background ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-8
                                             nil t)))
                           ,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :background ,(face-foreground
                                             'eat-term-color-9
                                             nil t))))
             :display `(,(add-props
                          "bright green"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-10 nil t)))
                        ,(add-props
                          "bright white"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-15 nil t)))
                        ,(add-props
                          "bright black"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-8 nil t)))
                        ,(add-props
                          "color-119"
                          `((0 . 9)
                            :background ,(face-foreground
                                          'eat-term-color-119 nil t)))
                        ,(add-props
                          "color-255"
                          `((0 . 9)
                            :background ,(face-foreground
                                          'eat-term-color-255 nil t))))
             :cursor '(6 . 1)))
    ;; 24-bit color (truecolor).
    (output "\e[48;2;34;139;34mforest green\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :background ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-8
                                             nil t)))
                           ,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :background ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-10
                                             nil t))))
             :display `(,(add-props
                          "bright white"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-15 nil t)))
                        ,(add-props
                          "bright black"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-8 nil t)))
                        ,(add-props
                          "color-119"
                          `((0 . 9)
                            :background ,(face-foreground
                                          'eat-term-color-119 nil t)))
                        ,(add-props
                          "color-255"
                          `((0 . 9)
                            :background ,(face-foreground
                                          'eat-term-color-255 nil t)))
                        ,(add-props
                          "forest green"
                          `((0 . 12)
                            :background "#228b22")))
             :cursor '(6 . 1)))
    (output "\e[48;2;160;32;240mpurple\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :background ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-8
                                             nil t)))
                           ,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :background ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-15
                                             nil t))))
             :display `(,(add-props
                          "bright black"
                          `((0 . 12)
                            :background ,(face-foreground
                                          'eat-term-color-8 nil t)))
                        ,(add-props
                          "color-119"
                          `((0 . 9)
                            :background ,(face-foreground
                                          'eat-term-color-119 nil t)))
                        ,(add-props
                          "color-255"
                          `((0 . 9)
                            :background ,(face-foreground
                                          'eat-term-color-255 nil t)))
                        ,(add-props
                          "forest green"
                          `((0 . 12)
                            :background "#228b22"))
                        ,(add-props
                          "purple"
                          `((0 . 6)
                            :background "#a020f0")))
             :cursor '(6 . 1)))
    (output "\e[49mdefault\n")
    (should (match-term
             :scrollback `(,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :background ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-8
                                             nil t)))
                           ,(add-props
                             "red"
                             `((0 . 3)
                               :background ,(face-foreground
                                             'eat-term-color-1
                                             nil t)))
                           ,(add-props
                             "green"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-2
                                             nil t)))
                           ,(add-props
                             "white"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-7
                                             nil t)))
                           ,(add-props
                             "black"
                             `((0 . 5)
                               :background ,(face-foreground
                                             'eat-term-color-0
                                             nil t)))
                           ,(add-props
                             "bright red"
                             `((0 . 10)
                               :background ,(face-foreground
                                             'eat-term-color-9
                                             nil t)))
                           ,(add-props
                             "bright green"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-10
                                             nil t)))
                           ,(add-props
                             "bright white"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-15
                                             nil t)))
                           ,(add-props
                             "bright black"
                             `((0 . 12)
                               :background ,(face-foreground
                                             'eat-term-color-8
                                             nil t))))
             :display `(,(add-props
                          "color-119"
                          `((0 . 9)
                            :background ,(face-foreground
                                          'eat-term-color-119 nil t)))
                        ,(add-props
                          "color-255"
                          `((0 . 9)
                            :background ,(face-foreground
                                          'eat-term-color-255 nil t)))
                        ,(add-props
                          "forest green"
                          `((0 . 12)
                            :background "#228b22"))
                        ,(add-props
                          "purple"
                          `((0 . 6)
                            :background "#a020f0"))
                        "default")
             :cursor '(6 . 1)))))

(ert-deftest eat-test-sgr-intensity ()
  "Test SGR intensity attributes (both bold and faint)."
  (eat--tests-with-term '()
    (output "\e[1mbold\n")
    (should (match-term
             :display `(,(add-props "bold" `((0 . 4)
                                             :intensity bold)))
             :cursor '(2 . 1)))
    (output "\e[2mfaint\n")
    (should (match-term
             :display `(,(add-props "bold" `((0 . 4)
                                             :intensity bold))
                        ,(add-props "faint" `((0 . 5)
                                              :intensity faint)))
             :cursor '(3 . 1)))
    (output "\e[22mnormal\n")
    (should (match-term
             :display `(,(add-props "bold" `((0 . 4)
                                             :intensity bold))
                        ,(add-props "faint" `((0 . 5)
                                              :intensity faint))
                        "normal")
             :cursor '(4 . 1)))))

(ert-deftest eat-test-sgr-italic ()
  "Test SGR italic attribute."
  (eat--tests-with-term '()
    (output "\e[3mitalic\n")
    (should (match-term
             :display `(,(add-props "italic" `((0 . 6) :italic t)))
             :cursor '(2 . 1)))
    (output "\e[23mnormal\n")
    (should (match-term
             :display `(,(add-props "italic" `((0 . 6) :italic t))
                        "normal")
             :cursor '(3 . 1)))))

(ert-deftest eat-test-sgr-underline ()
  "Test SGR underline with no color, 256 colors and 24-bit colors."
  (eat--tests-with-term '()
    ;; Without colors.
    (output "\e[4mdefault line\n")
    (should (match-term
             :display `(,(add-props
                          "default line"
                          `((0 . 12)
                            :underline-type line)))
             :cursor '(2 . 1)))
    (output "\e[4:0mnormal\n")
    (should (match-term
             :display `(,(add-props
                          "default line"
                          `((0 . 12)
                            :underline-type line))
                        "normal")
             :cursor '(3 . 1)))
    (output "\e[4:1mdefault line\n")
    (should (match-term
             :display `(,(add-props
                          "default line"
                          `((0 . 12)
                            :underline-type line))
                        "normal"
                        ,(add-props
                          "default line"
                          `((0 . 12)
                            :underline-type line)))
             :cursor '(4 . 1)))
    (output "\e[4:2mdefault line\n")
    (should (match-term
             :display `(,(add-props
                          "default line"
                          `((0 . 12)
                            :underline-type line))
                        "normal"
                        ,(add-props
                          "default line"
                          `((0 . 12)
                            :underline-type line))
                        ,(add-props
                          "default line"
                          `((0 . 12)
                            :underline-type line)))
             :cursor '(5 . 1)))
    (output "\e[4:3mdefault wave\n")
    (should (match-term
             :display `(,(add-props
                          "default line"
                          `((0 . 12)
                            :underline-type line))
                        "normal"
                        ,(add-props
                          "default line"
                          `((0 . 12)
                            :underline-type line))
                        ,(add-props
                          "default line"
                          `((0 . 12)
                            :underline-type line))
                        ,(add-props
                          "default wave"
                          `((0 . 12)
                            :underline-type wave)))
             :cursor '(6 . 1)))
    (output "\e[4:4mdefault wave\n")
    (should (match-term
             :scrollback `(,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line)))
             :display `("normal"
                        ,(add-props
                          "default line"
                          `((0 . 12)
                            :underline-type line))
                        ,(add-props
                          "default line"
                          `((0 . 12)
                            :underline-type line))
                        ,(add-props
                          "default wave"
                          `((0 . 12)
                            :underline-type wave))
                        ,(add-props
                          "default wave"
                          `((0 . 12)
                            :underline-type wave)))
             :cursor '(6 . 1)))
    (output "\e[4:5mdefault wave\n")
    (should (match-term
             :scrollback `(,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           "normal")
             :display `(,(add-props
                          "default line"
                          `((0 . 12)
                            :underline-type line))
                        ,(add-props
                          "default line"
                          `((0 . 12)
                            :underline-type line))
                        ,(add-props
                          "default wave"
                          `((0 . 12)
                            :underline-type wave))
                        ,(add-props
                          "default wave"
                          `((0 . 12)
                            :underline-type wave))
                        ,(add-props
                          "default wave"
                          `((0 . 12)
                            :underline-type wave)))
             :cursor '(6 . 1)))
    (output "\e[4;58;5;6mcyan line\n")
    (should (match-term
             :scrollback `(,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           "normal"
                           ,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line)))
             :display `(,(add-props
                          "default line"
                          `((0 . 12)
                            :underline-type line))
                        ,(add-props
                          "default wave"
                          `((0 . 12)
                            :underline-type wave))
                        ,(add-props
                          "default wave"
                          `((0 . 12)
                            :underline-type wave))
                        ,(add-props
                          "default wave"
                          `((0 . 12)
                            :underline-type wave))
                        ,(add-props
                          "cyan line"
                          `((0 . 9)
                            :underline-type line
                            :underline-color ,(face-foreground
                                               'eat-term-color-6
                                               nil t))))
             :cursor '(6 . 1)))
    (output "\e[4:3;58;5;3myellow wave\n")
    (should (match-term
             :scrollback `(,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           "normal"
                           ,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           ,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line)))
             :display `(,(add-props
                          "default wave"
                          `((0 . 12)
                            :underline-type wave))
                        ,(add-props
                          "default wave"
                          `((0 . 12)
                            :underline-type wave))
                        ,(add-props
                          "default wave"
                          `((0 . 12)
                            :underline-type wave))
                        ,(add-props
                          "cyan line"
                          `((0 . 9)
                            :underline-type line
                            :underline-color ,(face-foreground
                                               'eat-term-color-6
                                               nil t)))
                        ,(add-props
                          "yellow wave"
                          `((0 . 11)
                            :underline-type wave
                            :underline-color ,(face-foreground
                                               'eat-term-color-3
                                               nil t))))
             :cursor '(6 . 1)))
    (output "\e[4:1;58;5;13mbright magenta line\n")
    (should (match-term
             :scrollback `(,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           "normal"
                           ,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           ,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           ,(add-props
                             "default wave"
                             `((0 . 12)
                               :underline-type wave)))
             :display `(,(add-props
                          "default wave"
                          `((0 . 12)
                            :underline-type wave))
                        ,(add-props
                          "default wave"
                          `((0 . 12)
                            :underline-type wave))
                        ,(add-props
                          "cyan line"
                          `((0 . 9)
                            :underline-type line
                            :underline-color ,(face-foreground
                                               'eat-term-color-6
                                               nil t)))
                        ,(add-props
                          "yellow wave"
                          `((0 . 11)
                            :underline-type wave
                            :underline-color ,(face-foreground
                                               'eat-term-color-3
                                               nil t)))
                        ,(add-props
                          "bright magenta line"
                          `((0 . 19)
                            :underline-type line
                            :underline-color ,(face-foreground
                                               'eat-term-color-13
                                               nil t))))
             :cursor '(6 . 1)))
    (output "\e[4:4;58;5;133mcolor-133 wave\n")
    (should (match-term
             :scrollback `(,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           "normal"
                           ,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           ,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           ,(add-props
                             "default wave"
                             `((0 . 12)
                               :underline-type wave))
                           ,(add-props
                             "default wave"
                             `((0 . 12)
                               :underline-type wave)))
             :display `(,(add-props
                          "default wave"
                          `((0 . 12)
                            :underline-type wave))
                        ,(add-props
                          "cyan line"
                          `((0 . 9)
                            :underline-type line
                            :underline-color ,(face-foreground
                                               'eat-term-color-6
                                               nil t)))
                        ,(add-props
                          "yellow wave"
                          `((0 . 11)
                            :underline-type wave
                            :underline-color ,(face-foreground
                                               'eat-term-color-3
                                               nil t)))
                        ,(add-props
                          "bright magenta line"
                          `((0 . 19)
                            :underline-type line
                            :underline-color ,(face-foreground
                                               'eat-term-color-13
                                               nil t)))
                        ,(add-props
                          "color-133 wave"
                          `((0 . 14)
                            :underline-type wave
                            :underline-color ,(face-foreground
                                               'eat-term-color-133
                                               nil t))))
             :cursor '(6 . 1)))
    (output "\e[4:2;58;2;160;32;240mpurple line\n")
    (should (match-term
             :scrollback `(,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           "normal"
                           ,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           ,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           ,(add-props
                             "default wave"
                             `((0 . 12)
                               :underline-type wave))
                           ,(add-props
                             "default wave"
                             `((0 . 12)
                               :underline-type wave))
                           ,(add-props
                             "default wave"
                             `((0 . 12)
                               :underline-type wave)))
             :display `(,(add-props
                          "cyan line"
                          `((0 . 9)
                            :underline-type line
                            :underline-color ,(face-foreground
                                               'eat-term-color-6
                                               nil t)))
                        ,(add-props
                          "yellow wave"
                          `((0 . 11)
                            :underline-type wave
                            :underline-color ,(face-foreground
                                               'eat-term-color-3
                                               nil t)))
                        ,(add-props
                          "bright magenta line"
                          `((0 . 19)
                            :underline-type line
                            :underline-color ,(face-foreground
                                               'eat-term-color-13
                                               nil t)))
                        ,(add-props
                          "color-133 wave"
                          `((0 . 14)
                            :underline-type wave
                            :underline-color ,(face-foreground
                                               'eat-term-color-133
                                               nil t)))
                        ,(add-props
                          "purple line"
                          `((0 . 11)
                            :underline-type line
                            :underline-color "#a020f0")))
             :cursor '(6 . 1)))
    (output "\e[4:5;58;2;0;0;139mdark blue wave\n")
    (should (match-term
             :scrollback `(,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           "normal"
                           ,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           ,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           ,(add-props
                             "default wave"
                             `((0 . 12)
                               :underline-type wave))
                           ,(add-props
                             "default wave"
                             `((0 . 12)
                               :underline-type wave))
                           ,(add-props
                             "default wave"
                             `((0 . 12)
                               :underline-type wave))
                           ,(add-props
                             "cyan line"
                             `((0 . 9)
                               :underline-type line
                               :underline-color ,(face-foreground
                                                  'eat-term-color-6
                                                  nil t))))
             :display `(,(add-props
                          "yellow wave"
                          `((0 . 11)
                            :underline-type wave
                            :underline-color ,(face-foreground
                                               'eat-term-color-3
                                               nil t)))
                        ,(add-props
                          "bright magenta line"
                          `((0 . 19)
                            :underline-type line
                            :underline-color ,(face-foreground
                                               'eat-term-color-13
                                               nil t)))
                        ,(add-props
                          "color-133 wave"
                          `((0 . 14)
                            :underline-type wave
                            :underline-color ,(face-foreground
                                               'eat-term-color-133
                                               nil t)))
                        ,(add-props
                          "purple line"
                          `((0 . 11)
                            :underline-type line
                            :underline-color "#a020f0"))
                        ,(add-props
                          "dark blue wave"
                          `((0 . 14)
                            :underline-type wave
                            :underline-color "#00008b")))
             :cursor '(6 . 1)))
    (output "\e[59mdefault wave\n")
    (should (match-term
             :scrollback `(,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           "normal"
                           ,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           ,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           ,(add-props
                             "default wave"
                             `((0 . 12)
                               :underline-type wave))
                           ,(add-props
                             "default wave"
                             `((0 . 12)
                               :underline-type wave))
                           ,(add-props
                             "default wave"
                             `((0 . 12)
                               :underline-type wave))
                           ,(add-props
                             "cyan line"
                             `((0 . 9)
                               :underline-type line
                               :underline-color ,(face-foreground
                                                  'eat-term-color-6
                                                  nil t)))
                           ,(add-props
                             "yellow wave"
                             `((0 . 11)
                               :underline-type wave
                               :underline-color ,(face-foreground
                                                  'eat-term-color-3
                                                  nil t))))
             :display `(,(add-props
                          "bright magenta line"
                          `((0 . 19)
                            :underline-type line
                            :underline-color ,(face-foreground
                                               'eat-term-color-13
                                               nil t)))
                        ,(add-props
                          "color-133 wave"
                          `((0 . 14)
                            :underline-type wave
                            :underline-color ,(face-foreground
                                               'eat-term-color-133
                                               nil t)))
                        ,(add-props
                          "purple line"
                          `((0 . 11)
                            :underline-type line
                            :underline-color "#a020f0"))
                        ,(add-props
                          "dark blue wave"
                          `((0 . 14)
                            :underline-type wave
                            :underline-color "#00008b"))
                        ,(add-props
                          "default wave"
                          `((0 . 12)
                            :underline-type wave)))
             :cursor '(6 . 1)))
    (output "\e[24mnormal\n")
    (should (match-term
             :scrollback `(,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           "normal"
                           ,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           ,(add-props
                             "default line"
                             `((0 . 12)
                               :underline-type line))
                           ,(add-props
                             "default wave"
                             `((0 . 12)
                               :underline-type wave))
                           ,(add-props
                             "default wave"
                             `((0 . 12)
                               :underline-type wave))
                           ,(add-props
                             "default wave"
                             `((0 . 12)
                               :underline-type wave))
                           ,(add-props
                             "cyan line"
                             `((0 . 9)
                               :underline-type line
                               :underline-color ,(face-foreground
                                                  'eat-term-color-6
                                                  nil t)))
                           ,(add-props
                             "yellow wave"
                             `((0 . 11)
                               :underline-type wave
                               :underline-color ,(face-foreground
                                                  'eat-term-color-3
                                                  nil t)))
                           ,(add-props
                             "bright magenta line"
                             `((0 . 19)
                               :underline-type line
                               :underline-color ,(face-foreground
                                                  'eat-term-color-13
                                                  nil t))))
             :display `(,(add-props
                          "color-133 wave"
                          `((0 . 14)
                            :underline-type wave
                            :underline-color ,(face-foreground
                                               'eat-term-color-133
                                               nil t)))
                        ,(add-props
                          "purple line"
                          `((0 . 11)
                            :underline-type line
                            :underline-color "#a020f0"))
                        ,(add-props
                          "dark blue wave"
                          `((0 . 14)
                            :underline-type wave
                            :underline-color "#00008b"))
                        ,(add-props
                          "default wave"
                          `((0 . 12)
                            :underline-type wave))
                        "normal")
             :cursor '(6 . 1)))))

(ert-deftest eat-test-sgr-crossed ()
  "Test SGR crossed attribute."
  (eat--tests-with-term '()
    (output "\e[9mcrossed\n")
    (should (match-term
             :display `(,(add-props "crossed" `((0 . 7) :crossed t)))
             :cursor '(2 . 1)))
    (output "\e[29mnormal\n")
    (should (match-term
             :display `(,(add-props "crossed" `((0 . 7) :crossed t))
                        "normal")
             :cursor '(3 . 1)))))

(ert-deftest eat-test-sgr-inverse ()
  "Test SGR inverse attributes."
  (eat--tests-with-term '()
    (output "\e[7mdefault\n")
    (should (match-term
             :display `(,(add-props
                          "default"
                          `((0 . 7)
                            :foreground ,(face-background
                                          'default nil t)
                            :background ,(face-foreground
                                          'default nil t))))
             :cursor '(2 . 1)))
    (output "\e[31mred fg\n")
    (should (match-term
             :display `(,(add-props
                          "default"
                          `((0 . 7)
                            :foreground ,(face-background
                                          'default nil t)
                            :background ,(face-foreground
                                          'default nil t)))
                        ,(add-props
                          "red fg"
                          `((0 . 6)
                            :foreground ,(face-background
                                          'default nil t)
                            :background ,(face-foreground
                                          'eat-term-color-1
                                          nil t))))
             :cursor '(3 . 1)))
    (output "\e[42mred fg green bg\n")
    (should (match-term
             :display `(,(add-props
                          "default"
                          `((0 . 7)
                            :foreground ,(face-background
                                          'default nil t)
                            :background ,(face-foreground
                                          'default nil t)))
                        ,(add-props
                          "red fg"
                          `((0 . 6)
                            :foreground ,(face-background
                                          'default nil t)
                            :background ,(face-foreground
                                          'eat-term-color-1
                                          nil t)))
                        ,(add-props
                          "red fg green bg"
                          `((0 . 15)
                            :foreground ,(face-foreground
                                          'eat-term-color-2
                                          nil t)
                            :background ,(face-foreground
                                          'eat-term-color-1
                                          nil t))))
             :cursor '(4 . 1)))
    (output "\e[39mgreen bg\n")
    (should (match-term
             :display `(,(add-props
                          "default"
                          `((0 . 7)
                            :foreground ,(face-background
                                          'default nil t)
                            :background ,(face-foreground
                                          'default nil t)))
                        ,(add-props
                          "red fg"
                          `((0 . 6)
                            :foreground ,(face-background
                                          'default nil t)
                            :background ,(face-foreground
                                          'eat-term-color-1
                                          nil t)))
                        ,(add-props
                          "red fg green bg"
                          `((0 . 15)
                            :foreground ,(face-foreground
                                          'eat-term-color-2
                                          nil t)
                            :background ,(face-foreground
                                          'eat-term-color-1
                                          nil t)))
                        ,(add-props
                          "green bg"
                          `((0 . 8)
                            :foreground ,(face-foreground
                                          'eat-term-color-2
                                          nil t)
                            :background ,(face-foreground
                                          'default nil t))))
             :cursor '(5 . 1)))
    (output "\e[27;49mnormal\n")
    (should (match-term
             :display `(,(add-props
                          "default"
                          `((0 . 7)
                            :foreground ,(face-background
                                          'default nil t)
                            :background ,(face-foreground
                                          'default nil t)))
                        ,(add-props
                          "red fg"
                          `((0 . 6)
                            :foreground ,(face-background
                                          'default nil t)
                            :background ,(face-foreground
                                          'eat-term-color-1
                                          nil t)))
                        ,(add-props
                          "red fg green bg"
                          `((0 . 15)
                            :foreground ,(face-foreground
                                          'eat-term-color-2
                                          nil t)
                            :background ,(face-foreground
                                          'eat-term-color-1
                                          nil t)))
                        ,(add-props
                          "green bg"
                          `((0 . 8)
                            :foreground ,(face-foreground
                                          'eat-term-color-2
                                          nil t)
                            :background ,(face-foreground
                                          'default nil t)))
                        "normal")
             :cursor '(6 . 1)))))

(ert-deftest eat-test-sgr-blink ()
  "Test SGR blink attributes (both slow and fast blink)."
  (eat--tests-with-term '()
    (output "\e[5mslow\n")
    (should (match-term
             :display `(,(add-props "slow" `((0 . 4) :blink slow)))
             :cursor '(2 . 1)))
    (output "\e[6mfast\n")
    (should (match-term
             :display `(,(add-props "slow" `((0 . 4) :blink slow))
                        ,(add-props "fast" `((0 . 4) :blink fast)))
             :cursor '(3 . 1)))
    (output "\e[25mnormal\n")
    (should (match-term
             :display `(,(add-props "slow" `((0 . 4) :blink slow))
                        ,(add-props "fast" `((0 . 4) :blink fast))
                        "normal")
             :cursor '(4 . 1)))))

(ert-deftest eat-test-sgr-conceal ()
  (eat--tests-with-term '()
    (output "\e[8mdefault\n")
    (should (match-term
             :display `(,(add-props
                          "default"
                          `((0 . 7)
                            :foreground ,(face-background
                                          'default nil t))))
             :cursor '(2 . 1)))
    (output "\e[31mdefault with fg\n")
    (should (match-term
             :display `(,(add-props
                          "default"
                          `((0 . 7)
                            :foreground ,(face-background
                                          'default nil t)))
                        ,(add-props
                          "default with fg"
                          `((0 . 15)
                            :foreground ,(face-background
                                          'default nil t))))
             :cursor '(3 . 1)))
    (output "\e[41mred\n")
    (should (match-term
             :display `(,(add-props
                          "default"
                          `((0 . 7)
                            :foreground ,(face-background
                                          'default nil t)))
                        ,(add-props
                          "default with fg"
                          `((0 . 15)
                            :foreground ,(face-background
                                          'default nil t)))
                        ,(add-props
                          "red"
                          `((0 . 3)
                            :foreground ,(face-foreground
                                          'eat-term-color-1
                                          nil t)
                            :background ,(face-foreground
                                          'eat-term-color-1
                                          nil t))))
             :cursor '(4 . 1)))
    (output "\e[31;42mgreen\n")
    (should (match-term
             :display `(,(add-props
                          "default"
                          `((0 . 7)
                            :foreground ,(face-background
                                          'default nil t)))
                        ,(add-props
                          "default with fg"
                          `((0 . 15)
                            :foreground ,(face-background
                                          'default nil t)))
                        ,(add-props
                          "red"
                          `((0 . 3)
                            :foreground ,(face-foreground
                                          'eat-term-color-1
                                          nil t)
                            :background ,(face-foreground
                                          'eat-term-color-1
                                          nil t)))
                        ,(add-props
                          "green"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-2
                                          nil t)
                            :background ,(face-foreground
                                          'eat-term-color-2
                                          nil t))))
             :cursor '(5 . 1)))
    (output "\e[28mred on green\n")
    (should (match-term
             :display `(,(add-props
                          "default"
                          `((0 . 7)
                            :foreground ,(face-background
                                          'default nil t)))
                        ,(add-props
                          "default with fg"
                          `((0 . 15)
                            :foreground ,(face-background
                                          'default nil t)))
                        ,(add-props
                          "red"
                          `((0 . 3)
                            :foreground ,(face-foreground
                                          'eat-term-color-1
                                          nil t)
                            :background ,(face-foreground
                                          'eat-term-color-1
                                          nil t)))
                        ,(add-props
                          "green"
                          `((0 . 5)
                            :foreground ,(face-foreground
                                          'eat-term-color-2
                                          nil t)
                            :background ,(face-foreground
                                          'eat-term-color-2
                                          nil t)))
                        ,(add-props
                          "red on green"
                          `((0 . 12)
                            :foreground ,(face-foreground
                                          'eat-term-color-1
                                          nil t)
                            :background ,(face-foreground
                                          'eat-term-color-2
                                          nil t))))
             :cursor '(6 . 1)))))

(ert-deftest eat-test-sgr-font ()
  "Test SGR font attributes."
  (eat--tests-with-term '()
    (output "font 0\n")
    (should (match-term
             :display '("font 0")
             :cursor '(2 . 1)))
    (should (match-term
             :display `(,(add-props "font 0" `((0 . 6) :font 0)))
             :cursor '(2 . 1)))
    (output "\e[13mfont 3\n")
    (should (match-term
             :display `("font 0"
                        ,(add-props "font 3" `((0 . 6) :font 3)))
             :cursor '(3 . 1)))
    (output "\e[19mfont 9\n")
    (should (match-term
             :display `("font 0"
                        ,(add-props "font 3" `((0 . 6) :font 3))
                        ,(add-props "font 9" `((0 . 6) :font 9)))
             :cursor '(4 . 1)))
    (output "\e[12mfont 2\n")
    (should (match-term
             :display `("font 0"
                        ,(add-props "font 3" `((0 . 6) :font 3))
                        ,(add-props "font 9" `((0 . 6) :font 9))
                        ,(add-props "font 2" `((0 . 6) :font 2)))
             :cursor '(5 . 1)))
    (output "\e[10mfont 0, normal\n")
    (should (match-term
             :display `("font 0"
                        ,(add-props "font 3" `((0 . 6) :font 3))
                        ,(add-props "font 9" `((0 . 6) :font 9))
                        ,(add-props "font 2" `((0 . 6) :font 2))
                        "font 0, normal")
             :cursor '(6 . 1)))))


;;;;; Miscellaneous Tests.

(ert-deftest eat-test-bell ()
  "Test bell control function."
  (eat--tests-with-term '()
    (let ((bell-rang nil))
      (setf (eat-term-ring-bell-function (terminal))
            (lambda (term)
              (should (eq term (terminal)))
              (setq bell-rang t)))
      (output "\a")
      (should bell-rang))))

(ert-deftest eat-test-save-and-restore-cursor ()
  "Test saving and restoring cursor position.

Write plain text and newline to move cursor."
  (eat--tests-with-term '()
    (output "foo")
    (should (match-term :display '("foo")
                        :cursor '(1 . 4)))
    (output "\e7")
    (should (match-term :display '("foo")
                        :cursor '(1 . 4)))
    (output "bar\nfrob")
    (should (match-term :display '("foobar"
                                   "frob")
                        :cursor '(2 . 5)))
    (output "\e8")
    (should (match-term :display '("foobar"
                                   "frob")
                        :cursor '(1 . 4)))))

;;; eat-tests.el ends here
