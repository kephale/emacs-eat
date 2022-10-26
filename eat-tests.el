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
    (cl-flet
        ((visually-equal (a b)
           (if (> emacs-major-version 28)
               (should (equal-including-properties a b))
             ;; On Emacs versions less than 29,
             ;; `equal-including-properties' returns t only if the
             ;; properties of a and b are `eq', so we compare the
             ;; strings ourselves.
             (and
              (should (string= a b))
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
                   (should
                    (equal
                     (plist-to-alist (text-properties-at i a))
                     (plist-to-alist (text-properties-at i b))))))
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
            (eat-term-beginning terminal)
            (eat-term-display-beginning terminal))
           "\n" nil nil))))
    (and (should
          (or (= (eat-term-display-beginning terminal)
                 (eat-term-beginning terminal))
              (= (char-before (eat-term-display-beginning terminal))
                 ?\n)))
         (should (= (length scrollback) (length lines)))
         (cl-every (lambda (pair)
                     (eat--tests-compare-lines
                      (car pair) (cdr pair)))
                   (cl-mapcar #'cons scrollback lines)))))

(defun eat--tests-compare-display (terminal lines)
  "Compare TERMINAL's scrollback buffer with LINES."
  (let ((display
         (split-string
          (buffer-substring
           (eat-term-display-beginning terminal)
           (eat-term-end terminal))
          "\n" nil nil)))
    (and (should (<= (length display) (cdr (eat-term-size terminal))))
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
    (should (= (car cursor-pos) y))
    (should (= (cdr cursor-pos) x))))

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
        (input (make-symbol "input")))
    `(with-temp-buffer
       (let ((,term (eat-term-make (current-buffer) (point)))
             (,input ""))
         (unwind-protect
             (progn
               (cl-destructuring-bind
                   (&key (width 20) (height 6)) ,spec
                 (eat-term-resize ,term width height))
               (setf (eat-term-input-function ,term)
                     (lambda (_ str)
                       (setq ,input (concat ,input str))))
               (cl-labels
                   ((terminal ()
                      ,term)
                    (output (&rest args)
                      (dolist (str args)
                        (eat-term-process-output ,term str))
                      (eat-term-redisplay ,term))
                    (input-event (event &optional ref-pos n)
                      (eat-term-input-event ,term n event ref-pos))
                    (input ()
                      (prog1 ,input
                        (setq ,input "")))
                    (should-term (&key scrollback display cursor)
                      (eat--tests-compare-scrollback ,term scrollback)
                      (eat--tests-compare-display ,term display)
                      (eat--tests-compare-cursor-pos ,term cursor))
                    (add-props (str &rest intervals)
                      (apply #'eat--tests-add-properties
                             str intervals)))
                 ,@body))
           (eat-term-delete ,term))))))


;;;; Tests.

;;;;; Text Writing and Insertion Tests.

(ert-deftest eat-test-plain-text ()
  "Test plain text handling.

Send only plain text (i.e. no control sequences, not even newline) and
compare the output with the expected output.  Don't do test automatic
margin."
  (eat--tests-with-term '()
    (output "some test string")
    (should-term :display '("some test string    ")
                 :cursor '(1 . 17))))

(ert-deftest eat-test-auto-margin ()
  "Test automatic margin and toggling it."
  (eat--tests-with-term '()
    (should-term :cursor '(1 . 1))
    ;; Default: Automatic margin enabled.
    (output "some test string... and some more")
    (should-term :display '("some test string..."
                            "and some more")
                 :cursor '(2 . 14))
    ;; Automatic margin disabled.
    (output "\n\e[?7lsome test string... and some more")
    (should-term :display '("some test string..."
                            "and some more"
                            "some test string...e")
                 :cursor '(3 . 20))
    ;; Automatic margin enabled.
    (output "\n\e[?7hsome test string... and some more")
    (should-term :display '("some test string..."
                            "and some more"
                            "some test string...e"
                            "some test string..."
                            "and some more")
                 :cursor '(5 . 14))))

(ert-deftest eat-test-insert-mode ()
  "Test automatic margin and toggling it."
  (eat--tests-with-term '()
    ;; Default: Insert mode disabled.
    (output "a\bb")
    (should-term :display '("b")
                 :cursor '(1 . 2))
    ;; Insert mode enabled.
    (output "\e[4hb\ba")
    (should-term :display '("bab")
                 :cursor '(1 . 3))
    ;; Insert mode disabled.
    (output "\e[4lc\by")
    (should-term :display '("bay")
                 :cursor '(1 . 4))))


;;;;; Cursor Motion Tests.

(ert-deftest eat-test-character-tabulation ()
  "Test character tabulation control function."
  (eat--tests-with-term '()
    (output "\t")
    (should-term :cursor '(1 . 9))
    (output "\t")
    (should-term :cursor '(1 . 17))
    (output "\t")
    (should-term :cursor '(1 . 20))
    (output "\n ")
    (should-term :cursor '(2 . 2))
    (output "\t")
    (should-term :cursor '(2 . 9))))

(ert-deftest eat-test-cursor-backward-tabulation ()
  "Test cursor backward tabulation control function."
  (eat--tests-with-term '()
    (output "\t")
    (should-term :cursor '(1 . 9))
    (output "\t")
    (should-term :cursor '(1 . 17))
    (output "\t")
    (should-term :cursor '(1 . 20))
    (output "\n ")
    (should-term :cursor '(2 . 2))
    (output "\t")
    (should-term :cursor '(2 . 9))))

(ert-deftest eat-test-line-tabulation ()
  "Test line tabulation control function."
  (eat--tests-with-term '()
    (output "\v")
    (should-term :cursor '(2 . 1))
    (output "  ")
    (should-term :cursor '(2 . 3))
    (output "\v")
    (should-term :cursor '(3 . 3))
    (output "\v\v\v")
    (should-term :cursor '(6 . 3))
    (output "\v")
    (should-term :scrollback '("")
                 :cursor '(6 . 3))))

(ert-deftest eat-test-form-feed ()
  "Test form feed."
  (eat--tests-with-term '()
    (output "\f")
    (should-term :cursor '(2 . 1))
    (output "  ")
    (should-term :cursor '(2 . 3))
    (output "\f")
    (should-term :cursor '(3 . 3))
    (output "\f\f\f")
    (should-term :cursor '(6 . 3))
    (output "\f")
    (should-term :scrollback '("")
                 :cursor '(6 . 3))))

(ert-deftest eat-test-line-feed ()
  "Test line feed control function."
  (eat--tests-with-term '()
    (output "\n")
    (should-term :cursor '(2 . 1))
    (output "    \n")
    (should-term :cursor '(3 . 1))
    (output "\eE")
    (should-term :cursor '(4 . 1))
    (output "    \eE")
    (should-term :cursor '(5 . 1))))

(ert-deftest eat-test-reverse-index ()
  "Test reverse index control function.

Use newlines to move to an initial position from where the control
function is to be invoked."
  (eat--tests-with-term '()
    (output "\n\n")
    (should-term :cursor '(3 . 1))
    (output "\eM")
    (should-term :cursor '(2 . 1))
    (output "    \eM")
    (should-term :cursor '(1 . 5))))

(ert-deftest eat-test-backspace ()
  "Test backspace control function.

Use spaces to move to an initial position from where the control
function is to be invoked."
  (eat--tests-with-term '()
    (output " ")
    (should-term :cursor '(1 . 2))
    (output "\b")
    (should-term :cursor '(1 . 1))))

(ert-deftest eat-test-carriage-return ()
  "Test carriage return control function.

Use spaces to move to an initial position from where the control
function is to be invoked."
  (eat--tests-with-term '()
    (output "\r")
    (should-term :cursor '(1 . 1))
    (output "    ")
    (should-term :cursor '(1 . 5))
    (output "\r")
    (should-term :cursor '(1 . 1))))

(ert-deftest eat-test-cursor-right ()
  "Test cursor right control function."
  (eat--tests-with-term '()
    (output "\e[C")
    (should-term :cursor '(1 . 2))
    (output "\e[0C")
    (should-term :cursor '(1 . 3))
    (output "\e[5C")
    (should-term :cursor '(1 . 8))
    (output "\e[20C")
    (should-term :cursor '(1 . 20))))

(ert-deftest eat-test-cursor-left ()
  "Test cursor up control function.

Use spaces to move to an initial position from where the control
function is to be invoked."
  (eat--tests-with-term '()
    (output "                ")
    (should-term :cursor '(1 . 17))
    (output "\e[D")
    (should-term :cursor '(1 . 16))
    (output "\e[0D")
    (should-term :cursor '(1 . 15))
    (output "\e[7D")
    (should-term :cursor '(1 . 8))
    (output "\e[10D")
    (should-term :cursor '(1 . 1))))

(ert-deftest eat-test-cursor-down ()
  "Test cursor down control function."
  (eat--tests-with-term '()
    (output "\e[B")
    (should-term :cursor '(2 . 1))
    (output "\e[0B")
    (should-term :cursor '(3 . 1))
    (output "    ")
    (should-term :cursor '(3 . 5))
    (output "\e[6B")
    (should-term :cursor '(6 . 5))))

(ert-deftest eat-test-cursor-up ()
  "Test cursor up control function.

Use spaces and newlines to move to an initial position from where the
control function is to be invoked."
  (eat--tests-with-term '()
    (output "\n\n\n\n\n")
    (should-term :cursor '(6 . 1))
    (output "\e[A")
    (should-term :cursor '(5 . 1))
    (output "\e[0A")
    (should-term :cursor '(4 . 1))
    (output "    ")
    (should-term :cursor '(4 . 5))
    (output "\e[2A")
    (should-term :cursor '(2 . 5))
    (output "\e[4A")
    (should-term :cursor '(1 . 5))))

(ert-deftest eat-test-cursor-next-line ()
  "Test cursor next line control function."
  (eat--tests-with-term '(:height 10)
    (output "\e[F")
    (should-term :cursor '(2 . 1))
    (output "\e[0F")
    (should-term :cursor '(3 . 1))
    (output "\e[2F")
    (should-term :cursor '(5 . 1))
    (output "    \e[F")
    (should-term :cursor '(6 . 1))
    (output "        \e[0F")
    (should-term :cursor '(7 . 1))
    (output "  \e[3F")
    (should-term :cursor '(10 . 1))
    (output "   \e[F")
    (should-term :cursor '(10 . 1))))

(ert-deftest eat-test-cursor-previous-line ()
  "Test cursor previous line control function.

Use newlines to move to an initial position from where the control
function is to be invoked."
  (eat--tests-with-term '(:height 10)
    (output "\n\n\n\n\n\n\n\n\n")
    (should-term :cursor '(10 . 1))
    (output "\e[E")
    (should-term :cursor '(9 . 1))
    (output "\e[0E")
    (should-term :cursor '(8 . 1))
    (output "\e[2E")
    (should-term :cursor '(6 . 1))
    (output "    \e[E")
    (should-term :cursor '(5 . 1))
    (output "        \e[0E")
    (should-term :cursor '(4 . 1))
    (output "  \e[3E")
    (should-term :cursor '(1 . 1))
    (output "   \e[E")
    (should-term :cursor '(1 . 1))))

(ert-deftest eat-test-cursor-character-absolute ()
  "Test cursor character absolute control function."
  (eat--tests-with-term '()
    (output "\e[5G")
    (should-term :cursor '(1 . 5))
    (output "\e[0G")
    (should-term :cursor '(1 . 1))
    (output "\e[15G")
    (should-term :cursor '(1 . 15))
    (output "\e[G")
    (should-term :cursor '(1 . 1))))

(ert-deftest eat-test-character-position-absolute ()
  "Test character position absolute control function."
  (eat--tests-with-term '()
    (output "\e[5`")
    (should-term :cursor '(1 . 5))
    (output "\e[0`")
    (should-term :cursor '(1 . 1))
    (output "\e[15`")
    (should-term :cursor '(1 . 15))
    (output "\e[`")
    (should-term :cursor '(1 . 1))))

(ert-deftest eat-test-line-position-absolute ()
  "Test line position absolute control function."
  (eat--tests-with-term '()
    (output "\e[2d")
    (should-term :cursor '(2 . 1))
    (output "\e[0d")
    (should-term :cursor '(1 . 1))
    (output "\e[5d")
    (should-term :cursor '(5 . 1))
    (output "\e[d")
    (should-term :cursor '(1 . 1))))

(ert-deftest eat-test-cursor-position ()
  "Test cursor position control function."
  (eat--tests-with-term '()
    (output "\e[2;2H")
    (should-term :cursor '(2 . 2))
    (output "\e[;5H")
    (should-term :cursor '(1 . 5))
    (output "\e[4;H")
    (should-term :cursor '(4 . 1))
    (output "\e[7;H")
    (should-term :cursor '(6 . 1))
    (output "\e[0;0H")
    (should-term :cursor '(1 . 1))
    (output "\e[;30H")
    (should-term :cursor '(1 . 20))
    (output "\e[10;25H")
    (should-term :cursor '(6 . 20))
    (output "\e[H")
    (should-term :cursor '(1 . 1))))

(ert-deftest eat-test-character-and-line-position ()
  "Test character and line position control function."
  (eat--tests-with-term '()
    (output "\e[2;2f")
    (should-term :cursor '(2 . 2))
    (output "\e[;5f")
    (should-term :cursor '(1 . 5))
    (output "\e[4;f")
    (should-term :cursor '(4 . 1))
    (output "\e[7;f")
    (should-term :cursor '(6 . 1))
    (output "\e[0;0f")
    (should-term :cursor '(1 . 1))
    (output "\e[;30f")
    (should-term :cursor '(1 . 20))
    (output "\e[10;25f")
    (should-term :cursor '(6 . 20))
    (output "\e[f")
    (should-term :cursor '(1 . 1))))


;;;;; Scrolling Tests.

(ert-deftest eat-test-scroll-up ()
  "Test scroll up control function."
  (eat--tests-with-term '()
    (output "some test string...\nmore, more...\n"
            "more, more, more...\nand some more")
    (should-term :display '("some test string..."
                            "more, more..."
                            "more, more, more..."
                            "and some more")
                 :cursor '(4 . 14))
    (output "\e[S")
    (should-term :scrollback '("some test string...")
                 :display '("more, more..."
                            "more, more, more..."
                            "and some more")
                 :cursor '(4 . 14))
    (output "\e[0S")
    (should-term :scrollback '("some test string...")
                 :display '("more, more..."
                            "more, more, more..."
                            "and some more")
                 :cursor '(4 . 14))
    (output "\e[2S")
    (should-term :scrollback '("some test string..."
                               "more, more..."
                               "more, more, more...")
                 :display '("and some more")
                 :cursor '(4 . 14))
    (output "\nnew line 1\nnew line 2\nnew line 3\n"
            "new line 4\nnew line 5\nnew line 6\e[2;4r")
    (should-term :scrollback '("some test string..."
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
                 :cursor '(1 . 1))
    (output "\e[S")
    (should-term :scrollback '("some test string..."
                               "more, more..."
                               "more, more, more..."
                               "and some more"
                               ""
                               ""
                               "")
                 :display '("new line 1"
                            "new line 3"
                            "new line 4"
                            ""
                            "new line 5"
                            "new line 6")
                 :cursor '(1 . 1))
    (output "\e[0S")
    (should-term :scrollback '("some test string..."
                               "more, more..."
                               "more, more, more..."
                               "and some more"
                               ""
                               ""
                               "")
                 :display '("new line 1"
                            "new line 3"
                            "new line 4"
                            ""
                            "new line 5"
                            "new line 6")
                 :cursor '(1 . 1))
    (output "\e[2S")
    (should-term :scrollback '("some test string..."
                               "more, more..."
                               "more, more, more..."
                               "and some more"
                               ""
                               ""
                               "")
                 :display '("new line 1"
                            ""
                            ""
                            ""
                            "new line 5"
                            "new line 6")
                 :cursor '(1 . 1))))

(ert-deftest eat-test-scroll-down ()
  "Test scroll down control function."
  (eat--tests-with-term '()
    (output "some test string...\nmore, more...\n"
            "more, more, more...\nand some more")
    (should-term :display '("some test string..."
                            "more, more..."
                            "more, more, more..."
                            "and some more")
                 :cursor '(4 . 14))
    (output "\e[T")
    (should-term :display '(""
                            "some test string..."
                            "more, more..."
                            "more, more, more..."
                            "and some more")
                 :cursor '(4 . 14))
    (output "\e[0T")
    (should-term :display '(""
                            "some test string..."
                            "more, more..."
                            "more, more, more..."
                            "and some more")
                 :cursor '(4 . 14))
    (output "\e[2T")
    (should-term :display '(""
                            ""
                            ""
                            "some test string..."
                            "more, more..."
                            "more, more, more...")
                 :cursor '(4 . 14))
    (output "\n\n\nnew line 1\nnew line 2\nnew line 3\n"
            "new line 4\nnew line 5\nnew line 6\e[2;5r")
    (should-term :scrollback '(""
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
                 :cursor '(1 . 1))
    (output "\e[T")
    (should-term :scrollback '(""
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
                 :cursor '(1 . 1))
    (output "\e[0T")
    (should-term :scrollback '(""
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
                 :cursor '(1 . 1))
    (output "\e[2T")
    (should-term :scrollback '(""
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
                 :cursor '(1 . 1))))

(ert-deftest eat-test-auto-scrolling ()
  "Test automatic scrolling when cursor reaches end of display.

Test with every control functions and text combination that trigger
automatic scrolling as a side effect."
  (eat--tests-with-term '()
    ;; Test with newlines.
    (output "some test string...\n\n\n\n\n\nand some more")
    (should-term :scrollback '("some test string...")
                 :display '(""
                            ""
                            ""
                            ""
                            ""
                            "and some more")
                 :cursor '(6 . 14))
    ;; Test with automatic margin.
    (output "...more, more, stop.")
    (should-term :scrollback '("some test string..."
                               "")
                 :display '(""
                            ""
                            ""
                            ""
                            "and some more...more"
                            ", more, stop.")
                 :cursor '(6 . 14))
    ;; Test with reverse index.
    (output "\eM\eM\eM\eM\eM\eM")
    (should-term :scrollback '("some test string..."
                               "")
                 :display '(""
                            ""
                            ""
                            ""
                            ""
                            "and some more...more")
                 :cursor '(1 . 14))
    ;; Test with newlines and scroll region.
    (output "\e[2;5rline 1\nline 2\nline 3\nline 4\nline 5")
    (should-term :scrollback '("some test string..."
                               "")
                 :display '("line 1"
                            "line 2"
                            "line 3"
                            "line 4"
                            "line 5"
                            "and some more...more")
                 :cursor '(5 . 7))
    (output "\n")
    (should-term :scrollback '("some test string..."
                               "")
                 :display '("line 1"
                            "line 3"
                            "line 4"
                            "line 5"
                            ""
                            "and some more...more")
                 :cursor '(5 . 1))
    ;; Test with automatic margin and scroll region.
    (output "...more content, more, stop.")
    (should-term :scrollback '("some test string..."
                               "")
                 :display '("line 1"
                            "line 4"
                            "line 5"
                            "...more content, mor"
                            "e, stop."
                            "and some more...more")
                 :cursor '(5 . 9))
    ;; Test with reverse index and scroll region.
    (output "\eM\eM\eM\eM\eM")
    (should-term :scrollback '("some test string..."
                               "")
                 :display '("line 1"
                            ""
                            ""
                            "line 4"
                            "line 5"
                            "and some more...more")
                 :cursor '(2 . 9))))


;;;;; SGR Tests.

;; Beware, this section is a nightware of code repetition.

(ert-deftest eat-test-sgr-foreground ()
  "Test SGR foreground color.  Test 256 colors and 24-bit colors."
  (eat--tests-with-term '()
    ;; ANSI colors.
    (output "\e[31mred\n")
    (should-term
     :display `(,(add-props
                  "red"
                  `((0 . 3)
                    :foreground ,(face-foreground
                                  'eat-term-color-1 nil t))))
     :cursor '(2 . 1))
    (output "\e[32mgreen\n")
    (should-term
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
     :cursor '(3 . 1))
    (output "\e[37mwhite\n")
    (should-term
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
     :cursor '(4 . 1))
    (output "\e[30mblack\n")
    (should-term
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
     :cursor '(5 . 1))
    ;; ANSI bright colors.
    (output "\e[91mbright red\n")
    (should-term
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
     :cursor '(6 . 1))
    (output "\e[92mbright green\n")
    (should-term
     :scrollback `(,(add-props
                     "red"
                     `((0 . 3)
                       :foreground ,(face-foreground
                                     'eat-term-color-1 nil t))))
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
     :cursor '(6 . 1))
    (output "\e[97mbright white\n")
    (should-term
     :scrollback `(,(add-props
                     "red"
                     `((0 . 3)
                       :foreground ,(face-foreground
                                     'eat-term-color-1 nil t)))
                   ,(add-props
                     "green"
                     `((0 . 5)
                       :foreground ,(face-foreground
                                     'eat-term-color-2 nil t))))
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
     :cursor '(6 . 1))
    (output "\e[90mbright black\n")
    (should-term
     :scrollback `(,(add-props
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
     :cursor '(6 . 1))
    ;; ANSI colors using 256-color sequence.
    (output "\e[38;5;1mred\n")
    (should-term
     :scrollback `(,(add-props
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
     :cursor '(6 . 1))
    (output "\e[38;5;2mgreen\n")
    (should-term
     :scrollback `(,(add-props
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
     :cursor '(6 . 1))
    (output "\e[38;5;7mwhite\n")
    (should-term
     :scrollback `(,(add-props
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
                                     'eat-term-color-9 nil t)))
                   ,(add-props
                     "bright green"
                     `((0 . 12)
                       :foreground ,(face-foreground
                                     'eat-term-color-10 nil t))))
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
     :cursor '(6 . 1))
    (output "\e[38;5;0mblack\n")
    (should-term
     :scrollback `(,(add-props
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
     :cursor '(6 . 1))
    ;; ANSI bright colors using 256-color sequence.
    (output "\e[38;5;9mbright red\n")
    (should-term
     :scrollback `(,(add-props
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
     :cursor '(6 . 1))
    (output "\e[38;5;10mbright green\n")
    (should-term
     :scrollback `(,(add-props
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
     :cursor '(6 . 1))
    (output "\e[38;5;15mbright white\n")
    (should-term
     :scrollback `(,(add-props
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
                                     'eat-term-color-1 nil t)))
                   ,(add-props
                     "green"
                     `((0 . 5)
                       :foreground ,(face-foreground
                                     'eat-term-color-2 nil t))))
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
     :cursor '(6 . 1))
    (output "\e[38;5;8mbright black\n")
    (should-term
     :scrollback `(,(add-props
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
     :cursor '(6 . 1))
    ;; 256-color.
    (output "\e[38;5;119mcolor-119\n")
    (should-term
     :scrollback `(,(add-props
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
     :cursor '(6 . 1))
    (output "\e[38;5;255mcolor-255\n")
    (should-term
     :scrollback `(,(add-props
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
     :cursor '(6 . 1))
    ;; 24-bit color (truecolor).
    (output "\e[38;2;34;139;34mforest green\n")
    (should-term
     :scrollback `(,(add-props
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
                                     'eat-term-color-9 nil t)))
                   ,(add-props
                     "bright green"
                     `((0 . 12)
                       :foreground ,(face-foreground
                                     'eat-term-color-10 nil t))))
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
                  '((0 . 12)
                    :foreground "#228b22")))
     :cursor '(6 . 1))
    (output "\e[38;2;160;32;240mpurple\n")
    (should-term
     :scrollback `(,(add-props
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
                  '((0 . 12)
                    :foreground "#228b22"))
                ,(add-props
                  "purple"
                  '((0 . 6)
                    :foreground "#a020f0")))
     :cursor '(6 . 1))
    (output "\e[39mdefault\n")
    (should-term
     :scrollback `(,(add-props
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
                  '((0 . 12)
                    :foreground "#228b22"))
                ,(add-props
                  "purple"
                  '((0 . 6)
                    :foreground "#a020f0"))
                "default")
     :cursor '(6 . 1))))

(ert-deftest eat-test-sgr-background ()
  "Test SGR background color.  Test 256 colors and 24-bit colors."
  (eat--tests-with-term '()
    ;; ANSI colors.
    (output "\e[41mred\n")
    (should-term
     :display `(,(add-props
                  "red"
                  `((0 . 3)
                    :background ,(face-foreground
                                  'eat-term-color-1 nil t))))
     :cursor '(2 . 1))
    (output "\e[42mgreen\n")
    (should-term
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
     :cursor '(3 . 1))
    (output "\e[47mwhite\n")
    (should-term
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
     :cursor '(4 . 1))
    (output "\e[40mblack\n")
    (should-term
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
     :cursor '(5 . 1))
    ;; ANSI bright colors.
    (output "\e[101mbright red\n")
    (should-term
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
     :cursor '(6 . 1))
    (output "\e[102mbright green\n")
    (should-term
     :scrollback `(,(add-props
                     "red"
                     `((0 . 3)
                       :background ,(face-foreground
                                     'eat-term-color-1 nil t))))
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
     :cursor '(6 . 1))
    (output "\e[107mbright white\n")
    (should-term
     :scrollback `(,(add-props
                     "red"
                     `((0 . 3)
                       :background ,(face-foreground
                                     'eat-term-color-1 nil t)))
                   ,(add-props
                     "green"
                     `((0 . 5)
                       :background ,(face-foreground
                                     'eat-term-color-2 nil t))))
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
     :cursor '(6 . 1))
    (output "\e[100mbright black\n")
    (should-term
     :scrollback `(,(add-props
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
     :cursor '(6 . 1))
    ;; ANSI colors using 256-color sequence.
    (output "\e[48;5;1mred\n")
    (should-term
     :scrollback `(,(add-props
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
     :cursor '(6 . 1))
    (output "\e[48;5;2mgreen\n")
    (should-term
     :scrollback `(,(add-props
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
     :cursor '(6 . 1))
    (output "\e[48;5;7mwhite\n")
    (should-term
     :scrollback `(,(add-props
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
                                     'eat-term-color-9 nil t)))
                   ,(add-props
                     "bright green"
                     `((0 . 12)
                       :background ,(face-foreground
                                     'eat-term-color-10 nil t))))
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
     :cursor '(6 . 1))
    (output "\e[48;5;0mblack\n")
    (should-term
     :scrollback `(,(add-props
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
     :cursor '(6 . 1))
    ;; ANSI bright colors using 256-color sequence.
    (output "\e[48;5;9mbright red\n")
    (should-term
     :scrollback `(,(add-props
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
     :cursor '(6 . 1))
    (output "\e[48;5;10mbright green\n")
    (should-term
     :scrollback `(,(add-props
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
     :cursor '(6 . 1))
    (output "\e[48;5;15mbright white\n")
    (should-term
     :scrollback `(,(add-props
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
                                     'eat-term-color-1 nil t)))
                   ,(add-props
                     "green"
                     `((0 . 5)
                       :background ,(face-foreground
                                     'eat-term-color-2 nil t))))
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
     :cursor '(6 . 1))
    (output "\e[48;5;8mbright black\n")
    (should-term
     :scrollback `(,(add-props
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
     :cursor '(6 . 1))
    ;; 256-color.
    (output "\e[48;5;119mcolor-119\n")
    (should-term
     :scrollback `(,(add-props
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
     :cursor '(6 . 1))
    (output "\e[48;5;255mcolor-255\n")
    (should-term
     :scrollback `(,(add-props
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
     :cursor '(6 . 1))
    ;; 24-bit color (truecolor).
    (output "\e[48;2;34;139;34mforest green\n")
    (should-term
     :scrollback `(,(add-props
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
                                     'eat-term-color-9 nil t)))
                   ,(add-props
                     "bright green"
                     `((0 . 12)
                       :background ,(face-foreground
                                     'eat-term-color-10 nil t))))
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
                  '((0 . 12)
                    :background "#228b22")))
     :cursor '(6 . 1))
    (output "\e[48;2;160;32;240mpurple\n")
    (should-term
     :scrollback `(,(add-props
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
                  '((0 . 12)
                    :background "#228b22"))
                ,(add-props
                  "purple"
                  '((0 . 6)
                    :background "#a020f0")))
     :cursor '(6 . 1))
    (output "\e[49mdefault\n")
    (should-term
     :scrollback `(,(add-props
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
                  '((0 . 12)
                    :background "#228b22"))
                ,(add-props
                  "purple"
                  '((0 . 6)
                    :background "#a020f0"))
                "default")
     :cursor '(6 . 1))))

(ert-deftest eat-test-sgr-intensity ()
  "Test SGR intensity attributes (both bold and faint)."
  (eat--tests-with-term '()
    (output "\e[1mbold\n")
    (should-term
     :display `(,(add-props "bold" `((0 . 4)
                                     :intensity bold)))
     :cursor '(2 . 1))
    (output "\e[2mfaint\n")
    (should-term
     :display `(,(add-props "bold" `((0 . 4)
                                     :intensity bold))
                ,(add-props "faint" `((0 . 5)
                                      :intensity faint)))
     :cursor '(3 . 1))
    (output "\e[22mnormal\n")
    (should-term
     :display `(,(add-props "bold" `((0 . 4)
                                     :intensity bold))
                ,(add-props "faint" `((0 . 5)
                                      :intensity faint))
                "normal")
     :cursor '(4 . 1))))

(ert-deftest eat-test-sgr-italic ()
  "Test SGR italic attribute."
  (eat--tests-with-term '()
    (output "\e[3mitalic\n")
    (should-term
     :display `(,(add-props "italic" `((0 . 6) :italic t)))
     :cursor '(2 . 1))
    (output "\e[23mnormal\n")
    (should-term
     :display `(,(add-props "italic" `((0 . 6) :italic t))
                "normal")
     :cursor '(3 . 1))))

(ert-deftest eat-test-sgr-underline ()
  "Test SGR underline with no color, 256 colors and 24-bit colors."
  (eat--tests-with-term '()
    ;; Without colors.
    (output "\e[4mdefault line\n")
    (should-term
     :display `(,(add-props
                  "default line"
                  '((0 . 12)
                    :underline-type line)))
     :cursor '(2 . 1))
    (output "\e[4:0mnormal\n")
    (should-term
     :display `(,(add-props
                  "default line"
                  '((0 . 12)
                    :underline-type line))
                "normal")
     :cursor '(3 . 1))
    (output "\e[4:1mdefault line\n")
    (should-term
     :display `(,(add-props
                  "default line"
                  '((0 . 12)
                    :underline-type line))
                "normal"
                ,(add-props
                  "default line"
                  '((0 . 12)
                    :underline-type line)))
     :cursor '(4 . 1))
    (output "\e[4:2mdefault line\n")
    (should-term
     :display `(,(add-props
                  "default line"
                  '((0 . 12)
                    :underline-type line))
                "normal"
                ,(add-props
                  "default line"
                  '((0 . 12)
                    :underline-type line))
                ,(add-props
                  "default line"
                  '((0 . 12)
                    :underline-type line)))
     :cursor '(5 . 1))
    (output "\e[4:3mdefault wave\n")
    (should-term
     :display `(,(add-props
                  "default line"
                  '((0 . 12)
                    :underline-type line))
                "normal"
                ,(add-props
                  "default line"
                  '((0 . 12)
                    :underline-type line))
                ,(add-props
                  "default line"
                  '((0 . 12)
                    :underline-type line))
                ,(add-props
                  "default wave"
                  '((0 . 12)
                    :underline-type wave)))
     :cursor '(6 . 1))
    (output "\e[4:4mdefault wave\n")
    (should-term
     :scrollback `(,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line)))
     :display `("normal"
                ,(add-props
                  "default line"
                  '((0 . 12)
                    :underline-type line))
                ,(add-props
                  "default line"
                  '((0 . 12)
                    :underline-type line))
                ,(add-props
                  "default wave"
                  '((0 . 12)
                    :underline-type wave))
                ,(add-props
                  "default wave"
                  '((0 . 12)
                    :underline-type wave)))
     :cursor '(6 . 1))
    (output "\e[4:5mdefault wave\n")
    (should-term
     :scrollback `(,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   "normal")
     :display `(,(add-props
                  "default line"
                  '((0 . 12)
                    :underline-type line))
                ,(add-props
                  "default line"
                  '((0 . 12)
                    :underline-type line))
                ,(add-props
                  "default wave"
                  '((0 . 12)
                    :underline-type wave))
                ,(add-props
                  "default wave"
                  '((0 . 12)
                    :underline-type wave))
                ,(add-props
                  "default wave"
                  '((0 . 12)
                    :underline-type wave)))
     :cursor '(6 . 1))
    (output "\e[4;58;5;6mcyan line\n")
    (should-term
     :scrollback `(,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   "normal"
                   ,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line)))
     :display `(,(add-props
                  "default line"
                  '((0 . 12)
                    :underline-type line))
                ,(add-props
                  "default wave"
                  '((0 . 12)
                    :underline-type wave))
                ,(add-props
                  "default wave"
                  '((0 . 12)
                    :underline-type wave))
                ,(add-props
                  "default wave"
                  '((0 . 12)
                    :underline-type wave))
                ,(add-props
                  "cyan line"
                  `((0 . 9)
                    :underline-type line
                    :underline-color ,(face-foreground
                                       'eat-term-color-6 nil t))))
     :cursor '(6 . 1))
    (output "\e[4:3;58;5;3myellow wave\n")
    (should-term
     :scrollback `(,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   "normal"
                   ,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   ,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line)))
     :display `(,(add-props
                  "default wave"
                  '((0 . 12)
                    :underline-type wave))
                ,(add-props
                  "default wave"
                  '((0 . 12)
                    :underline-type wave))
                ,(add-props
                  "default wave"
                  '((0 . 12)
                    :underline-type wave))
                ,(add-props
                  "cyan line"
                  `((0 . 9)
                    :underline-type line
                    :underline-color ,(face-foreground
                                       'eat-term-color-6 nil t)))
                ,(add-props
                  "yellow wave"
                  `((0 . 11)
                    :underline-type wave
                    :underline-color ,(face-foreground
                                       'eat-term-color-3 nil t))))
     :cursor '(6 . 1))
    (output "\e[4:1;58;5;13mbright magenta line\n")
    (should-term
     :scrollback `(,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   "normal"
                   ,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   ,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   ,(add-props
                     "default wave"
                     '((0 . 12)
                       :underline-type wave)))
     :display `(,(add-props
                  "default wave"
                  '((0 . 12)
                    :underline-type wave))
                ,(add-props
                  "default wave"
                  '((0 . 12)
                    :underline-type wave))
                ,(add-props
                  "cyan line"
                  `((0 . 9)
                    :underline-type line
                    :underline-color ,(face-foreground
                                       'eat-term-color-6 nil t)))
                ,(add-props
                  "yellow wave"
                  `((0 . 11)
                    :underline-type wave
                    :underline-color ,(face-foreground
                                       'eat-term-color-3 nil t)))
                ,(add-props
                  "bright magenta line"
                  `((0 . 19)
                    :underline-type line
                    :underline-color ,(face-foreground
                                       'eat-term-color-13 nil t))))
     :cursor '(6 . 1))
    (output "\e[4:4;58;5;133mcolor-133 wave\n")
    (should-term
     :scrollback `(,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   "normal"
                   ,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   ,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   ,(add-props
                     "default wave"
                     '((0 . 12)
                       :underline-type wave))
                   ,(add-props
                     "default wave"
                     '((0 . 12)
                       :underline-type wave)))
     :display `(,(add-props
                  "default wave"
                  '((0 . 12)
                    :underline-type wave))
                ,(add-props
                  "cyan line"
                  `((0 . 9)
                    :underline-type line
                    :underline-color ,(face-foreground
                                       'eat-term-color-6 nil t)))
                ,(add-props
                  "yellow wave"
                  `((0 . 11)
                    :underline-type wave
                    :underline-color ,(face-foreground
                                       'eat-term-color-3 nil t)))
                ,(add-props
                  "bright magenta line"
                  `((0 . 19)
                    :underline-type line
                    :underline-color ,(face-foreground
                                       'eat-term-color-13 nil t)))
                ,(add-props
                  "color-133 wave"
                  `((0 . 14)
                    :underline-type wave
                    :underline-color ,(face-foreground
                                       'eat-term-color-133 nil t))))
     :cursor '(6 . 1))
    (output "\e[4:2;58;2;160;32;240mpurple line\n")
    (should-term
     :scrollback `(,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   "normal"
                   ,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   ,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   ,(add-props
                     "default wave"
                     '((0 . 12)
                       :underline-type wave))
                   ,(add-props
                     "default wave"
                     '((0 . 12)
                       :underline-type wave))
                   ,(add-props
                     "default wave"
                     '((0 . 12)
                       :underline-type wave)))
     :display `(,(add-props
                  "cyan line"
                  `((0 . 9)
                    :underline-type line
                    :underline-color ,(face-foreground
                                       'eat-term-color-6 nil t)))
                ,(add-props
                  "yellow wave"
                  `((0 . 11)
                    :underline-type wave
                    :underline-color ,(face-foreground
                                       'eat-term-color-3 nil t)))
                ,(add-props
                  "bright magenta line"
                  `((0 . 19)
                    :underline-type line
                    :underline-color ,(face-foreground
                                       'eat-term-color-13 nil t)))
                ,(add-props
                  "color-133 wave"
                  `((0 . 14)
                    :underline-type wave
                    :underline-color ,(face-foreground
                                       'eat-term-color-133 nil t)))
                ,(add-props
                  "purple line"
                  '((0 . 11)
                    :underline-type line
                    :underline-color "#a020f0")))
     :cursor '(6 . 1))
    (output "\e[4:5;58;2;0;0;139mdark blue wave\n")
    (should-term
     :scrollback `(,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   "normal"
                   ,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   ,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   ,(add-props
                     "default wave"
                     '((0 . 12)
                       :underline-type wave))
                   ,(add-props
                     "default wave"
                     '((0 . 12)
                       :underline-type wave))
                   ,(add-props
                     "default wave"
                     '((0 . 12)
                       :underline-type wave))
                   ,(add-props
                     "cyan line"
                     `((0 . 9)
                       :underline-type line
                       :underline-color ,(face-foreground
                                          'eat-term-color-6 nil t))))
     :display `(,(add-props
                  "yellow wave"
                  `((0 . 11)
                    :underline-type wave
                    :underline-color ,(face-foreground
                                       'eat-term-color-3 nil t)))
                ,(add-props
                  "bright magenta line"
                  `((0 . 19)
                    :underline-type line
                    :underline-color ,(face-foreground
                                       'eat-term-color-13 nil t)))
                ,(add-props
                  "color-133 wave"
                  `((0 . 14)
                    :underline-type wave
                    :underline-color ,(face-foreground
                                       'eat-term-color-133 nil t)))
                ,(add-props
                  "purple line"
                  '((0 . 11)
                    :underline-type line
                    :underline-color "#a020f0"))
                ,(add-props
                  "dark blue wave"
                  '((0 . 14)
                    :underline-type wave
                    :underline-color "#00008b")))
     :cursor '(6 . 1))
    (output "\e[59mdefault wave\n")
    (should-term
     :scrollback `(,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   "normal"
                   ,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   ,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   ,(add-props
                     "default wave"
                     '((0 . 12)
                       :underline-type wave))
                   ,(add-props
                     "default wave"
                     '((0 . 12)
                       :underline-type wave))
                   ,(add-props
                     "default wave"
                     '((0 . 12)
                       :underline-type wave))
                   ,(add-props
                     "cyan line"
                     `((0 . 9)
                       :underline-type line
                       :underline-color ,(face-foreground
                                          'eat-term-color-6 nil t)))
                   ,(add-props
                     "yellow wave"
                     `((0 . 11)
                       :underline-type wave
                       :underline-color ,(face-foreground
                                          'eat-term-color-3 nil t))))
     :display `(,(add-props
                  "bright magenta line"
                  `((0 . 19)
                    :underline-type line
                    :underline-color ,(face-foreground
                                       'eat-term-color-13 nil t)))
                ,(add-props
                  "color-133 wave"
                  `((0 . 14)
                    :underline-type wave
                    :underline-color ,(face-foreground
                                       'eat-term-color-133 nil t)))
                ,(add-props
                  "purple line"
                  '((0 . 11)
                    :underline-type line
                    :underline-color "#a020f0"))
                ,(add-props
                  "dark blue wave"
                  '((0 . 14)
                    :underline-type wave
                    :underline-color "#00008b"))
                ,(add-props
                  "default wave"
                  '((0 . 12)
                    :underline-type wave)))
     :cursor '(6 . 1))
    (output "\e[21mdefault line\n")
    (should-term
     :scrollback `(,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   "normal"
                   ,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   ,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   ,(add-props
                     "default wave"
                     '((0 . 12)
                       :underline-type wave))
                   ,(add-props
                     "default wave"
                     '((0 . 12)
                       :underline-type wave))
                   ,(add-props
                     "default wave"
                     '((0 . 12)
                       :underline-type wave))
                   ,(add-props
                     "cyan line"
                     `((0 . 9)
                       :underline-type line
                       :underline-color ,(face-foreground
                                          'eat-term-color-6 nil t)))
                   ,(add-props
                     "yellow wave"
                     `((0 . 11)
                       :underline-type wave
                       :underline-color ,(face-foreground
                                          'eat-term-color-3 nil t)))
                   ,(add-props
                     "bright magenta line"
                     `((0 . 19)
                       :underline-type line
                       :underline-color ,(face-foreground
                                          'eat-term-color-13 nil t))))
     :display `(,(add-props
                  "color-133 wave"
                  `((0 . 14)
                    :underline-type wave
                    :underline-color ,(face-foreground
                                       'eat-term-color-133 nil t)))
                ,(add-props
                  "purple line"
                  '((0 . 11)
                    :underline-type line
                    :underline-color "#a020f0"))
                ,(add-props
                  "dark blue wave"
                  '((0 . 14)
                    :underline-type wave
                    :underline-color "#00008b"))
                ,(add-props
                  "default wave"
                  '((0 . 12)
                    :underline-type wave))
                ,(add-props
                  "default line"
                  '((0 . 12)
                    :underline-type line)))
     :cursor '(6 . 1))
    (output "\e[24mnormal\n")
    (should-term
     :scrollback `(,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   "normal"
                   ,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   ,(add-props
                     "default line"
                     '((0 . 12)
                       :underline-type line))
                   ,(add-props
                     "default wave"
                     '((0 . 12)
                       :underline-type wave))
                   ,(add-props
                     "default wave"
                     '((0 . 12)
                       :underline-type wave))
                   ,(add-props
                     "default wave"
                     '((0 . 12)
                       :underline-type wave))
                   ,(add-props
                     "cyan line"
                     `((0 . 9)
                       :underline-type line
                       :underline-color ,(face-foreground
                                          'eat-term-color-6 nil t)))
                   ,(add-props
                     "yellow wave"
                     `((0 . 11)
                       :underline-type wave
                       :underline-color ,(face-foreground
                                          'eat-term-color-3 nil t)))
                   ,(add-props
                     "bright magenta line"
                     `((0 . 19)
                       :underline-type line
                       :underline-color ,(face-foreground
                                          'eat-term-color-13 nil t)))
                   ,(add-props
                     "color-133 wave"
                     `((0 . 14)
                       :underline-type wave
                       :underline-color ,(face-foreground
                                          'eat-term-color-133 nil t))))
     :display `(,(add-props
                  "purple line"
                  '((0 . 11)
                    :underline-type line
                    :underline-color "#a020f0"))
                ,(add-props
                  "dark blue wave"
                  '((0 . 14)
                    :underline-type wave
                    :underline-color "#00008b"))
                ,(add-props
                  "default wave"
                  '((0 . 12)
                    :underline-type wave))
                ,(add-props
                  "default line"
                  '((0 . 12)
                    :underline-type line))
                "normal")
     :cursor '(6 . 1))))

(ert-deftest eat-test-sgr-crossed ()
  "Test SGR crossed attribute."
  (eat--tests-with-term '()
    (output "\e[9mcrossed\n")
    (should-term
     :display `(,(add-props "crossed" `((0 . 7) :crossed t)))
     :cursor '(2 . 1))
    (output "\e[29mnormal\n")
    (should-term
     :display `(,(add-props "crossed" `((0 . 7) :crossed t))
                "normal")
     :cursor '(3 . 1))))

(ert-deftest eat-test-sgr-inverse ()
  "Test SGR inverse attributes."
  (eat--tests-with-term '()
    (output "\e[7mdefault\n")
    (should-term
     :display `(,(add-props
                  "default"
                  `((0 . 7)
                    :foreground ,(face-background
                                  'default nil t)
                    :background ,(face-foreground
                                  'default nil t))))
     :cursor '(2 . 1))
    (output "\e[31mred fg\n")
    (should-term
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
                                  'eat-term-color-1 nil t))))
     :cursor '(3 . 1))
    (output "\e[42mred fg green bg\n")
    (should-term
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
                                  'eat-term-color-1 nil t)))
                ,(add-props
                  "red fg green bg"
                  `((0 . 15)
                    :foreground ,(face-foreground
                                  'eat-term-color-2 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-1 nil t))))
     :cursor '(4 . 1))
    (output "\e[39mgreen bg\n")
    (should-term
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
                                  'eat-term-color-1 nil t)))
                ,(add-props
                  "red fg green bg"
                  `((0 . 15)
                    :foreground ,(face-foreground
                                  'eat-term-color-2 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-1 nil t)))
                ,(add-props
                  "green bg"
                  `((0 . 8)
                    :foreground ,(face-foreground
                                  'eat-term-color-2 nil t)
                    :background ,(face-foreground
                                  'default nil t))))
     :cursor '(5 . 1))
    (output "\e[27;49mnormal\n")
    (should-term
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
                                  'eat-term-color-1 nil t)))
                ,(add-props
                  "red fg green bg"
                  `((0 . 15)
                    :foreground ,(face-foreground
                                  'eat-term-color-2 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-1 nil t)))
                ,(add-props
                  "green bg"
                  `((0 . 8)
                    :foreground ,(face-foreground
                                  'eat-term-color-2 nil t)
                    :background ,(face-foreground
                                  'default nil t)))
                "normal")
     :cursor '(6 . 1))))

(ert-deftest eat-test-sgr-blink ()
  "Test SGR blink attributes (both slow and fast blink)."
  (eat--tests-with-term '()
    (output "\e[5mslow\n")
    (should-term
     :display `(,(add-props "slow" `((0 . 4) :blink slow)))
     :cursor '(2 . 1))
    (output "\e[6mfast\n")
    (should-term
     :display `(,(add-props "slow" `((0 . 4) :blink slow))
                ,(add-props "fast" `((0 . 4) :blink fast)))
     :cursor '(3 . 1))
    (output "\e[25mnormal\n")
    (should-term
     :display `(,(add-props "slow" `((0 . 4) :blink slow))
                ,(add-props "fast" `((0 . 4) :blink fast))
                "normal")
     :cursor '(4 . 1))))

(ert-deftest eat-test-sgr-conceal ()
  (eat--tests-with-term '()
    (output "\e[8mdefault\n")
    (should-term
     :display `(,(add-props
                  "default"
                  `((0 . 7)
                    :foreground ,(face-background
                                  'default nil t))))
     :cursor '(2 . 1))
    (output "\e[31mdefault with fg\n")
    (should-term
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
     :cursor '(3 . 1))
    (output "\e[41mred\n")
    (should-term
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
                                  'eat-term-color-1 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-1 nil t))))
     :cursor '(4 . 1))
    (output "\e[31;42mgreen\n")
    (should-term
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
                                  'eat-term-color-1 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-1 nil t)))
                ,(add-props
                  "green"
                  `((0 . 5)
                    :foreground ,(face-foreground
                                  'eat-term-color-2 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-2 nil t))))
     :cursor '(5 . 1))
    (output "\e[28mred on green\n")
    (should-term
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
                                  'eat-term-color-1 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-1 nil t)))
                ,(add-props
                  "green"
                  `((0 . 5)
                    :foreground ,(face-foreground
                                  'eat-term-color-2 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-2 nil t)))
                ,(add-props
                  "red on green"
                  `((0 . 12)
                    :foreground ,(face-foreground
                                  'eat-term-color-1 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-2 nil t))))
     :cursor '(6 . 1))))

(ert-deftest eat-test-sgr-font ()
  "Test SGR font attributes."
  (eat--tests-with-term '()
    (output "font 0\n")
    (should-term
     :display '("font 0")
     :cursor '(2 . 1))
    (should-term
     :display `(,(add-props "font 0" `((0 . 6) :font 0)))
     :cursor '(2 . 1))
    (output "\e[13mfont 3\n")
    (should-term
     :display `("font 0"
                ,(add-props "font 3" `((0 . 6) :font 3)))
     :cursor '(3 . 1))
    (output "\e[19mfont 9\n")
    (should-term
     :display `("font 0"
                ,(add-props "font 3" `((0 . 6) :font 3))
                ,(add-props "font 9" `((0 . 6) :font 9)))
     :cursor '(4 . 1))
    (output "\e[12mfont 2\n")
    (should-term
     :display `("font 0"
                ,(add-props "font 3" `((0 . 6) :font 3))
                ,(add-props "font 9" `((0 . 6) :font 9))
                ,(add-props "font 2" `((0 . 6) :font 2)))
     :cursor '(5 . 1))
    (output "\e[10mfont 0, normal\n")
    (should-term
     :display `("font 0"
                ,(add-props "font 3" `((0 . 6) :font 3))
                ,(add-props "font 9" `((0 . 6) :font 9))
                ,(add-props "font 2" `((0 . 6) :font 2))
                "font 0, normal")
     :cursor '(6 . 1))))

(ert-deftest eat-test-sgr-reset ()
  "Test SGR attributes reset sequence."
  (eat--tests-with-term '(:width 30 :height 10)
    (output "\e[1;3;4:3;6;7;9;15;33;105;"
            "58;5;10mcrazy text 1\n")
    (should-term
     :display `(,(add-props
                  "crazy text 1"
                  `((0 . 12)
                    :foreground ,(face-foreground
                                  'eat-term-color-13 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-3 nil t)
                    :intensity bold
                    :italic t
                    :underline-type wave
                    :underline-color ,(face-foreground
                                       'eat-term-color-10 nil t)
                    :blink fast
                    :crossed t
                    :font 5)))
     :cursor '(2 . 1))
    (output "\e[0mnormal text 1\r\n")
    (should-term
     :display `(,(add-props
                  "crazy text 1"
                  `((0 . 12)
                    :foreground ,(face-foreground
                                  'eat-term-color-13 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-3 nil t)
                    :intensity bold
                    :italic t
                    :underline-type wave
                    :underline-color ,(face-foreground
                                       'eat-term-color-10 nil t)
                    :blink fast
                    :crossed t
                    :font 5))
                "normal text 1")
     :cursor '(3 . 1))
    (output "\e[2;3;4:1;5;7;8;9;18;"
            "38;2;50;90;100;48;2;100;50;9;"
            "58;2;10;90;45mcrazy text 2\n")
    (should-term
     :display `(,(add-props
                  "crazy text 1"
                  `((0 . 12)
                    :foreground ,(face-foreground
                                  'eat-term-color-13 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-3 nil t)
                    :intensity bold
                    :italic t
                    :underline-type wave
                    :underline-color ,(face-foreground
                                       'eat-term-color-10 nil t)
                    :blink fast
                    :crossed t
                    :font 5))
                "normal text 1"
                ,(add-props
                  "crazy text 2"
                  '((0 . 12)
                    :foreground "#643209"
                    :background "#643209"
                    :intensity faint
                    :italic t
                    :underline-type line
                    :underline-color "#0a5a2d"
                    :blink slow
                    :crossed t
                    :font 8)))
     :cursor '(4 . 1))
    (output "\e[mnormal text 2\n")
    (should-term
     :display `(,(add-props
                  "crazy text 1"
                  `((0 . 12)
                    :foreground ,(face-foreground
                                  'eat-term-color-13 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-3 nil t)
                    :intensity bold
                    :italic t
                    :underline-type wave
                    :underline-color ,(face-foreground
                                       'eat-term-color-10 nil t)
                    :blink fast
                    :crossed t
                    :font 5))
                "normal text 1"
                ,(add-props
                  "crazy text 2"
                  '((0 . 12)
                    :foreground "#643209"
                    :background "#643209"
                    :intensity faint
                    :italic t
                    :underline-type line
                    :underline-color "#0a5a2d"
                    :blink slow
                    :crossed t
                    :font 8))
                "normal text 2")
     :cursor '(5 . 1))))


;;;;; Text Manipulation Tests.

(ert-deftest eat-test-insert-character ()
  "Test insert character control function."
  (eat--tests-with-term '()
    ;; Without background.
    (output "in the universe,\nthere are\nbugs and antibugs\n"
            "iwritebothoftheme")
    (should-term :display '("in the universe,"
                            "there are"
                            "bugs and antibugs"
                            "iwritebothoftheme")
                 :cursor '(4 . 18))
    (output "\e[2G\e[@")
    (should-term :display '("in the universe,"
                            "there are"
                            "bugs and antibugs"
                            "i writebothoftheme")
                 :cursor '(4 . 2))
    (output "\e[6C\e[1@")
    (should-term :display '("in the universe,"
                            "there are"
                            "bugs and antibugs"
                            "i write bothoftheme")
                 :cursor '(4 . 8))
    (output "\e[5C\e[@")
    (should-term :display '("in the universe,"
                            "there are"
                            "bugs and antibugs"
                            "i write both oftheme")
                 :cursor '(4 . 13))
    (output "\e[3C\e[0@")
    (should-term :display '("in the universe,"
                            "there are"
                            "bugs and antibugs"
                            "i write both of them")
                 :cursor '(4 . 16))
    ;; With background.
    (output "\nfoobar\e[3D")
    (should-term :display '("in the universe,"
                            "there are"
                            "bugs and antibugs"
                            "i write both of them"
                            "foobar")
                 :cursor '(5 . 4))
    (output "\e[40m\e[@")
    (should-term
     :display `("in the universe,"
                "there are"
                "bugs and antibugs"
                "i write both of them"
                ,(add-props
                  "foo bar"
                  `((3 . 4)
                    :background ,(face-foreground
                                  'eat-term-color-0 nil t))))
     :cursor '(5 . 4))
    (output "\e[107m\e[0@")
    (should-term
     :display `("in the universe,"
                "there are"
                "bugs and antibugs"
                "i write both of them"
                ,(add-props
                  "foo  bar"
                  `((3 . 4)
                    :background ,(face-foreground
                                  'eat-term-color-15 nil t))
                  `((4 . 5)
                    :background ,(face-foreground
                                  'eat-term-color-0 nil t))))
     :cursor '(5 . 4))
    (output "\e[48;5;133m\e[1@")
    (should-term
     :display `("in the universe,"
                "there are"
                "bugs and antibugs"
                "i write both of them"
                ,(add-props
                  "foo   bar"
                  `((3 . 4)
                    :background ,(face-foreground
                                  'eat-term-color-133 nil t))
                  `((4 . 5)
                    :background ,(face-foreground
                                  'eat-term-color-15 nil t))
                  `((5 . 6)
                    :background ,(face-foreground
                                  'eat-term-color-0 nil t))))
     :cursor '(5 . 4))
    (output "\e[48;2;50;255;62m\e[5@")
    (should-term
     :display `("in the universe,"
                "there are"
                "bugs and antibugs"
                "i write both of them"
                ,(add-props
                  "foo        bar"
                  '((3 . 8)
                    :background "#32ff3e")
                  `((8 . 9)
                    :background ,(face-foreground
                                  'eat-term-color-133 nil t))
                  `((9 . 10)
                    :background ,(face-foreground
                                  'eat-term-color-15 nil t))
                  `((10 . 11)
                    :background ,(face-foreground
                                  'eat-term-color-0 nil t))))
     :cursor '(5 . 4))
    (output "\e[49m\e[3@")
    (should-term
     :display `("in the universe,"
                "there are"
                "bugs and antibugs"
                "i write both of them"
                ,(add-props
                  "foo           bar"
                  '((6 . 11)
                    :background "#32ff3e")
                  `((11 . 12)
                    :background ,(face-foreground
                                  'eat-term-color-133 nil t))
                  `((12 . 13)
                    :background ,(face-foreground
                                  'eat-term-color-15 nil t))
                  `((13 . 14)
                    :background ,(face-foreground
                                  'eat-term-color-0 nil t))))
     :cursor '(5 . 4))))

(ert-deftest eat-test-delete-character ()
  "Test delete character control function."
  (eat--tests-with-term '()
    ;; Without background.
    (output "sun is an star")
    (should-term :display '("sun is an star")
                 :cursor '(1 . 15))
    (output "\e[6D\e[P")
    (should-term :display '("sun is a star")
                 :cursor '(1 . 9))
    (output "\nmars is an exoplanet")
    (should-term :display '("sun is a star"
                            "mars is an exoplanet")
                 :cursor '(2 . 20))
    (output "\e[9D\e[3P")
    (should-term :display '("sun is a star"
                            "mars is an planet")
                 :cursor '(2 . 12))
    (output "\e[2D\e[0P")
    (should-term :display '("sun is a star"
                            "mars is a planet")
                 :cursor '(2 . 10))
    ;; With background.
    (output "\nnill isn't false")
    (should-term :display '("sun is a star"
                            "mars is a planet"
                            "nill isn't false")
                 :cursor '(3 . 17))
    (output "\e[3G\e[46m\e[P\e[49m")
    (should-term
     :display `("sun is a star"
                "mars is a planet"
                ,(add-props
                  "nil isn't false     "
                  `((19 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-6 nil t))))
     :cursor '(3 . 3))
    (output "\e[7G\e[48;5;14m\e[3P\e[49m")
    (should-term
     :display `("sun is a star"
                "mars is a planet"
                ,(add-props
                  "nil is false        "
                  `((16 . 17)
                    :background ,(face-foreground
                                  'eat-term-color-6 nil t))
                  `((17 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-14 nil t))))
     :cursor '(3 . 7))
    (output "\nemacs is awesomes")
    (should-term
     :display `("sun is a star"
                "mars is a planet"
                ,(add-props
                  "nil is false        "
                  `((16 . 17)
                    :background ,(face-foreground
                                  'eat-term-color-6 nil t))
                  `((17 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-14 nil t)))
                "emacs is awesomes")
     :cursor '(4 . 18))
    (output "\e[D\e[48;2;226;43;93m\e[0P\e[49m")
    (should-term
     :display `("sun is a star"
                "mars is a planet"
                ,(add-props
                  "nil is false        "
                  `((16 . 17)
                    :background ,(face-foreground
                                  'eat-term-color-6 nil t))
                  `((17 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-14 nil t)))
                ,(add-props
                  "emacs is awesome    "
                  '((19 . 20)
                    :background "#e22b5d")))
     :cursor '(4 . 17))))

(ert-deftest eat-test-erase-character ()
  "Test erase character control function."
  (eat--tests-with-term '()
    ;; Without background.
    (output "abbcccddddee")
    (should-term :display '("abbcccddddee")
                 :cursor '(1 . 13))
    (output "\e[2`\e[X")
    (should-term :display '("a bcccddddee")
                 :cursor '(1 . 2))
    (output "\e[2C\e[1X")
    (should-term :display '("a b ccddddee")
                 :cursor '(1 . 4))
    (output "\e[2C\e[2X")
    (should-term :display '("a b c  dddee")
                 :cursor '(1 . 6))
    (output "\e[3C\e[2X")
    (should-term :display '("a b c  d  ee")
                 :cursor '(1 . 9))
    (output "\e[3C\e[0X")
    (should-term :display '("a b c  d  e")
                 :cursor '(1 . 12))
    ;; With background.
    (output "\nabbcccddddee")
    (should-term :display '("a b c  d  e"
                            "abbcccddddee")
                 :cursor '(2 . 13))
    (output "\e[2`\e[42m\e[X")
    (should-term
     :display `("a b c  d  e"
                ,(add-props
                  "a bcccddddee"
                  `((1 . 2)
                    :background ,(face-foreground
                                  'eat-term-color-2 nil t))))
     :cursor '(2 . 2))
    (output "\e[2C\e[48;5;42m\e[1X")
    (should-term
     :display `("a b c  d  e"
                ,(add-props
                  "a b ccddddee"
                  `((1 . 2)
                    :background ,(face-foreground
                                  'eat-term-color-2 nil t))
                  `((3 . 4)
                    :background ,(face-foreground
                                  'eat-term-color-42 nil t))))
     :cursor '(2 . 4))
    (output "\e[2C\e[48;2;0;46;160m\e[2X")
    (should-term
     :display `("a b c  d  e"
                ,(add-props
                  "a b c  dddee"
                  `((1 . 2)
                    :background ,(face-foreground
                                  'eat-term-color-2 nil t))
                  `((3 . 4)
                    :background ,(face-foreground
                                  'eat-term-color-42 nil t))
                  '((5 . 7)
                    :background "#002ea0")))
     :cursor '(2 . 6))
    (output "\e[3C\e[103m\e[2X")
    (should-term
     :display `("a b c  d  e"
                ,(add-props
                  "a b c  d  ee"
                  `((1 . 2)
                    :background ,(face-foreground
                                  'eat-term-color-2 nil t))
                  `((3 . 4)
                    :background ,(face-foreground
                                  'eat-term-color-42 nil t))
                  '((5 . 7)
                    :background "#002ea0")
                  `((8 . 10)
                    :background ,(face-foreground
                                  'eat-term-color-11 nil t))))
     :cursor '(2 . 9))
    (output "\e[3C\e[48;2;162;96;198m\e[0X")
    (should-term
     :display `("a b c  d  e"
                ,(add-props
                  "a b c  d  e "
                  `((1 . 2)
                    :background ,(face-foreground
                                  'eat-term-color-2 nil t))
                  `((3 . 4)
                    :background ,(face-foreground
                                  'eat-term-color-42 nil t))
                  '((5 . 7)
                    :background "#002ea0")
                  `((8 . 10)
                    :background ,(face-foreground
                                  'eat-term-color-11 nil t))
                  '((11 . 12)
                    :background "#a260c6")))
     :cursor '(2 . 12))))

(ert-deftest eat-test-repeat ()
  "Test repeat control function."
  (eat--tests-with-term '()
    ;; Without SGR attributes.
    (output "a\e[b")
    (should-term :display '("aa")
                 :cursor '(1 . 3))
    (output "\nb\e[2b")
    (should-term :display '("aa"
                            "bbb")
                 :cursor '(2 . 4))
    (output "\nc\e[0b")
    (should-term :display '("aa"
                            "bbb"
                            "cc")
                 :cursor '(3 . 3))
    ;; With SGR attributes.
    (output "\n\e[34;43md\e[b")
    (should-term
     :display `("aa"
                "bbb"
                "cc"
                ,(add-props
                  "dd"
                  `((0 . 2)
                    :foreground ,(face-foreground
                                  'eat-term-color-4 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-3 nil t))))
     :cursor '(4 . 3))
    (output "\n\e[;1;11me\e[5b")
    (should-term
     :display `("aa"
                "bbb"
                "cc"
                ,(add-props
                  "dd"
                  `((0 . 2)
                    :foreground ,(face-foreground
                                  'eat-term-color-4 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-3 nil t)))
                ,(add-props
                  "eeeeee"
                  '((0 . 6)
                    :intensity bold
                    :font 1)))
     :cursor '(5 . 7))
    (output "\n\e[;2;5mf\e[0b")
    (should-term
     :display `("aa"
                "bbb"
                "cc"
                ,(add-props
                  "dd"
                  `((0 . 2)
                    :foreground ,(face-foreground
                                  'eat-term-color-4 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-3 nil t)))
                ,(add-props
                  "eeeeee"
                  '((0 . 6)
                    :intensity bold
                    :font 1))
                ,(add-props
                  "ff"
                  '((0 . 2)
                    :intensity faint
                    :blink slow)))
     :cursor '(6 . 3))))

(ert-deftest eat-test-insert-line ()
  "Test insert line control function."
  (eat--tests-with-term '()
    ;; Without background.
    (output "early to bed and\nearly to rise,\nmakes a man\n"
            "healthy, wealthy,\nand wise")
    (should-term :display '("early to bed and"
                            "early to rise,"
                            "makes a man"
                            "healthy, wealthy,"
                            "and wise")
                 :cursor '(5 . 9))
    (output "\e[L")
    (should-term :display '("early to bed and"
                            "early to rise,"
                            "makes a man"
                            "healthy, wealthy,"
                            ""
                            "and wise")
                 :cursor '(5 . 9))
    (output "\e[2;15H\e[3L")
    (should-term :display '("early to bed and"
                            ""
                            ""
                            ""
                            "early to rise,"
                            "makes a man")
                 :cursor '(2 . 15))
    (output "\e[0L")
    (should-term :display '("early to bed and"
                            ""
                            ""
                            ""
                            ""
                            "early to rise,")
                 :cursor '(2 . 15))
    ;; With background.
    (output "\e[2Jearly to bed and\nearly to rise,\nmakes a man\n"
            "healthy, wealthy,\nand wise")
    (should-term :display '("early to bed and"
                            "early to rise,"
                            "makes a man"
                            "healthy, wealthy,"
                            "and wise")
                 :cursor '(5 . 9))
    (output "\e[44m\e[L")
    (should-term
     :display `("early to bed and"
                "early to rise,"
                "makes a man"
                "healthy, wealthy,"
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-4 nil t)))
                "and wise")
     :cursor '(5 . 9))
    (output "\e[2;15H\e[100m\e[3L")
    (should-term
     :display `("early to bed and"
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                "early to rise,"
                "makes a man")
     :cursor '(2 . 15))
    (output "\e[48;2;100;100;50m\e[0L")
    (should-term
     :display `("early to bed and"
                ,(add-props
                  "                    "
                  '((0 . 20)
                    :background "#646432"))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                "early to rise,")
     :cursor '(2 . 15))
    (output "\e[49m\e[1L")
    (should-term
     :display `("early to bed and"
                ""
                ,(add-props
                  "                    "
                  '((0 . 20)
                    :background "#646432"))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t))))
     :cursor '(2 . 15))))

(ert-deftest eat-test-delete-line ()
  "Test insert line control function."
  (eat--tests-with-term '()
    ;; Without background.
    (output "early to bed and\nearly to rise,\nmakes a man\n"
            "healthy, wealthy,\nand wise")
    (should-term :display '("early to bed and"
                            "early to rise,"
                            "makes a man"
                            "healthy, wealthy,"
                            "and wise")
                 :cursor '(5 . 9))
    (output "\e[M")
    (should-term :display '("early to bed and"
                            "early to rise,"
                            "makes a man"
                            "healthy, wealthy,")
                 :cursor '(5 . 9))
    (output "\e[2;15H\e[3M")
    (should-term :display '("early to bed and")
                 :cursor '(2 . 15))
    (output "\e[;5H\e[0M")
    (should-term :cursor '(1 . 5))
    ;; With background.
    (output "\e[Hearly to bed and\nearly to rise,\nmakes a man\n"
            "healthy, wealthy,\nand wise")
    (should-term :display '("early to bed and"
                            "early to rise,"
                            "makes a man"
                            "healthy, wealthy,"
                            "and wise")
                 :cursor '(5 . 9))
    (output "\e[44m\e[M")
    (should-term
     :display `("early to bed and"
                "early to rise,"
                "makes a man"
                "healthy, wealthy,"
                ""
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-4 nil t))))
     :cursor '(5 . 9))
    (output "\e[2;15H\e[100m\e[3M")
    (should-term
     :display `("early to bed and"
                ""
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-4 nil t)))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t))))
     :cursor '(2 . 15))
    (output "\e[H\e[48;2;100;100;50m\e[0M")
    (should-term
     :display `(""
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-4 nil t)))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                ,(add-props
                  "                    "
                  '((0 . 20)
                    :background "#646432")))
     :cursor '(1 . 1))
    (output "\e[49m\e[1M")
    (should-term
     :display `(,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-4 nil t)))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                ,(add-props
                  "                    "
                  '((0 . 20)
                    :background "#646432")))
     :cursor '(1 . 1))))

(ert-deftest eat-test-erase-in-line ()
  "Test erase in line control function."
  (eat--tests-with-term '()
    ;; Without background.
    (output "foo bar baz\e[6G")
    (should-term :display '("foo bar baz")
                 :cursor '(1 . 6))
    (output "\e[K")
    (should-term :display '("foo b")
                 :cursor '(1 . 6))
    (output "\nbar baz foo\e[6G")
    (should-term :display '("foo b"
                            "bar baz foo")
                 :cursor '(2 . 6))
    (output "\e[1K")
    (should-term :display '("foo b"
                            "      z foo")
                 :cursor '(2 . 6))
    (output "\nbaz foo bar\e[6G")
    (should-term :display '("foo b"
                            "      z foo"
                            "baz foo bar")
                 :cursor '(3 . 6))
    (output "\e[0K")
    (should-term :display '("foo b"
                            "      z foo"
                            "baz f")
                 :cursor '(3 . 6))
    (output "\nfoo bar baz\e[6G")
    (should-term :display '("foo b"
                            "      z foo"
                            "baz f"
                            "foo bar baz")
                 :cursor '(4 . 6))
    (output "\e[2K")
    (should-term :display '("foo b"
                            "      z foo"
                            "baz f")
                 :cursor '(4 . 6))
    ;; With background.
    (output "\nfoo bar baz\e[6G")
    (should-term :display '("foo b"
                            "      z foo"
                            "baz f"
                            ""
                            "foo bar baz")
                 :cursor '(5 . 6))
    (output "\e[47m\e[K\e[m")
    (should-term
     :display `("foo b"
                "      z foo"
                "baz f"
                ""
                ,(add-props
                  "foo b               "
                  `((5 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-7 nil t))))
     :cursor '(5 . 6))
    (output "\nbar baz foo\e[6G")
    (should-term
     :display `("foo b"
                "      z foo"
                "baz f"
                ""
                ,(add-props
                  "foo b               "
                  `((5 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-7 nil t)))
                "bar baz foo")
     :cursor '(6 . 6))
    (output "\e[100m\e[1K\e[m")
    (should-term
     :display `("foo b"
                "      z foo"
                "baz f"
                ""
                ,(add-props
                  "foo b               "
                  `((5 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-7 nil t)))
                ,(add-props
                  "      z foo"
                  `((0 . 6)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t))))
     :cursor '(6 . 6))
    (output "\nbaz foo bar\e[6G")
    (should-term
     :scrollback '("foo b")
     :display `("      z foo"
                "baz f"
                ""
                ,(add-props
                  "foo b               "
                  `((5 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-7 nil t)))
                ,(add-props
                  "      z foo"
                  `((0 . 6)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                "baz foo bar")
     :cursor '(6 . 6))
    (output "\e[48;5;55m\e[0K\e[m")
    (should-term
     :scrollback '("foo b")
     :display `("      z foo"
                "baz f"
                ""
                ,(add-props
                  "foo b               "
                  `((5 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-7 nil t)))
                ,(add-props
                  "      z foo"
                  `((0 . 6)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                ,(add-props
                  "baz f                 "
                  `((5 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-55 nil t))))
     :cursor '(6 . 6))
    (output "\nfoo bar baz\e[6G")
    (should-term
     :scrollback '("foo b"
                   "      z foo")
     :display `("baz f"
                ""
                ,(add-props
                  "foo b               "
                  `((5 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-7 nil t)))
                ,(add-props
                  "      z foo"
                  `((0 . 6)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                ,(add-props
                  "baz f                 "
                  `((5 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-55 nil t)))
                "foo bar baz")
     :cursor '(6 . 6))
    (output "\e[48;2;255;255;255m\e[2K")
    (should-term
     :scrollback '("foo b"
                   "      z foo")
     :display `("baz f"
                ""
                ,(add-props
                  "foo b               "
                  `((5 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-7 nil t)))
                ,(add-props
                  "      z foo"
                  `((0 . 6)
                    :background ,(face-foreground
                                  'eat-term-color-8 nil t)))
                ,(add-props
                  "baz f                 "
                  `((5 . 20)
                    :background ,(face-foreground
                                  'eat-term-color-55 nil t)))
                ,(add-props
                  "                      "
                  '((0 . 20)
                    :background "#ffffff")))
     :cursor '(6 . 6))))

(ert-deftest eat-test-erase-in-display ()
  "Test erase in display control function."
  (eat--tests-with-term '()
    ;; Without background.
    (output "foo bar baz\nbar baz foo\nbaz foo bar\e[2;6H")
    (should-term :display '("foo bar baz"
                            "bar baz foo"
                            "baz foo bar")
                 :cursor '(2 . 6))
    (output "\e[J")
    (should-term :display '("foo bar baz"
                            "bar b")
                 :cursor '(2 . 6))
    (output "\e[Hfoo bar baz\nbar baz foo\nbaz foo bar\e[2;6H")
    (should-term :display '("foo bar baz"
                            "bar baz foo"
                            "baz foo bar")
                 :cursor '(2 . 6))
    (output "\e[1J")
    (should-term :display '(""
                            "      z foo"
                            "baz foo bar")
                 :cursor '(2 . 6))
    (output "\e[Hfoo bar baz\nbar baz foo\nbaz foo bar\e[2;6H")
    (should-term :display '("foo bar baz"
                            "bar baz foo"
                            "baz foo bar")
                 :cursor '(2 . 6))
    (output "\e[0J")
    (should-term :display '("foo bar baz"
                            "bar b")
                 :cursor '(2 . 6))
    (output "\e[Hfoo bar baz\nbar baz foo\nbaz foo bar\e[2;6H")
    (should-term :display '("foo bar baz"
                            "bar baz foo"
                            "baz foo bar")
                 :cursor '(2 . 6))
    (output "\e[2J")
    (should-term :cursor '(1 . 1))
    (output "foo bar baz\nbar baz foo\nbaz foo bar\n"
            "foo bar baz\nbar baz foo\nbaz foo bar\n")
    (should-term :scrollback '("foo bar baz")
                 :display '("bar baz foo"
                            "baz foo bar"
                            "foo bar baz"
                            "bar baz foo"
                            "baz foo bar")
                 :cursor '(6 . 1))
    (output "\e[3J")
    (should-term :cursor '(1 . 1))
    ;; With background.
    (output "foo bar baz\nbar baz foo\nbaz foo bar\e[2;6H")
    (should-term :display '("foo bar baz"
                            "bar baz foo"
                            "baz foo bar")
                 :cursor '(2 . 6))
    (output "\e[11;33;44m\e[J\e[m")
    (should-term
     :display `("foo bar baz"
                ,(add-props
                  "bar b               "
                  `((5 . 20)
                    :foreground ,(face-foreground
                                  'eat-term-color-3 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-4 nil t)
                    :font 1))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :foreground ,(face-foreground
                                  'eat-term-color-3 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-4 nil t)
                    :font 1))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :foreground ,(face-foreground
                                  'eat-term-color-3 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-4 nil t)
                    :font 1))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :foreground ,(face-foreground
                                  'eat-term-color-3 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-4 nil t)
                    :font 1))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :foreground ,(face-foreground
                                  'eat-term-color-3 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-4 nil t)
                    :font 1)))
     :cursor '(2 . 6))
    (output "\e[2Jfoo bar baz\nbar baz foo\nbaz foo bar\e[2;6H")
    (should-term :display '("foo bar baz"
                            "bar baz foo"
                            "baz foo bar")
                 :cursor '(2 . 6))
    (output "\e[1;97;103m\e[1J\e[m")
    (should-term
     :display `(,(add-props
                  "                    "
                  `((0 . 20)
                    :foreground ,(face-foreground
                                  'eat-term-color-15 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-11 nil t)
                    :intensity bold))
                ,(add-props
                  "      z foo"
                  `((0 . 6)
                    :foreground ,(face-foreground
                                  'eat-term-color-15 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-11 nil t)
                    :intensity bold))
                "baz foo bar")
     :cursor '(2 . 6))
    (output "\e[2Jfoo bar baz\nbar baz foo\nbaz foo bar\e[2;6H")
    (should-term :display '("foo bar baz"
                            "bar baz foo"
                            "baz foo bar")
                 :cursor '(2 . 6))
    (output "\e[11;34;43m\e[0J\e[m")
    (should-term
     :display `("foo bar baz"
                ,(add-props
                  "bar b               "
                  `((5 . 20)
                    :foreground ,(face-foreground
                                  'eat-term-color-4 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-3 nil t)
                    :font 1))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :foreground ,(face-foreground
                                  'eat-term-color-4 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-3 nil t)
                    :font 1))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :foreground ,(face-foreground
                                  'eat-term-color-4 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-3 nil t)
                    :font 1))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :foreground ,(face-foreground
                                  'eat-term-color-4 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-3 nil t)
                    :font 1))
                ,(add-props
                  "                    "
                  `((0 . 20)
                    :foreground ,(face-foreground
                                  'eat-term-color-4 nil t)
                    :background ,(face-foreground
                                  'eat-term-color-3 nil t)
                    :font 1)))
     :cursor '(2 . 6))
    (output "\e[2Jfoo bar baz\nbar baz foo\nbaz foo bar\e[2;6H")
    (should-term :display '("foo bar baz"
                            "bar baz foo"
                            "baz foo bar")
                 :cursor '(2 . 6))
    (output "\e[48;2;50;200;100m\e[2J\e[m")
    (should-term
     :display `(,(add-props
                  "                    "
                  '((0 . 20) :background "#32c864"))
                ,(add-props
                  "                    "
                  '((0 . 20) :background "#32c864"))
                ,(add-props
                  "                    "
                  '((0 . 20) :background "#32c864"))
                ,(add-props
                  "                    "
                  '((0 . 20) :background "#32c864"))
                ,(add-props
                  "                    "
                  '((0 . 20) :background "#32c864"))
                ,(add-props
                  "                    "
                  '((0 . 20) :background "#32c864")))
     :cursor '(1 . 1))
    (output "\e[2Jfoo bar baz\nbar baz foo\nbaz foo bar\n"
            "foo bar baz\nbar baz foo\nbaz foo bar\n")
    (should-term :scrollback '("foo bar baz")
                 :display '("bar baz foo"
                            "baz foo bar"
                            "foo bar baz"
                            "bar baz foo"
                            "baz foo bar")
                 :cursor '(6 . 1))
    (output "\e[48;2;20;5;200m\e[3J\e[m")
    (should-term
     :display `(,(add-props
                  "                    "
                  '((0 . 20) :background "#1405c8"))
                ,(add-props
                  "                    "
                  '((0 . 20) :background "#1405c8"))
                ,(add-props
                  "                    "
                  '((0 . 20) :background "#1405c8"))
                ,(add-props
                  "                    "
                  '((0 . 20) :background "#1405c8"))
                ,(add-props
                  "                    "
                  '((0 . 20) :background "#1405c8"))
                ,(add-props
                  "                    "
                  '((0 . 20) :background "#1405c8")))
     :cursor '(1 . 1))))


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

(ert-deftest eat-test-character-sets ()
  "Test character sets."
  (eat--tests-with-term '()
    (output "some text")
    (should-term :display '("some text")
                 :cursor '(1 . 10))
    (output "\n\e(0some text")
    (should-term :display '("some text"
                            "⎽⎺└␊ ├␊│├")
                 :cursor '(2 . 10))
    (output "\n\e(Bsome text")
    (should-term :display '("some text"
                            "⎽⎺└␊ ├␊│├"
                            "some text")
                 :cursor '(3 . 10))
    (output "\n\C-nsome text")
    (should-term :display '("some text"
                            "⎽⎺└␊ ├␊│├"
                            "some text"
                            "some text")
                 :cursor '(4 . 10))
    (output "\n\e)0some text")
    (should-term :display '("some text"
                            "⎽⎺└␊ ├␊│├"
                            "some text"
                            "some text"
                            "⎽⎺└␊ ├␊│├")
                 :cursor '(5 . 10))
    (output "\n\e)Bsome text")
    (should-term :display '("some text"
                            "⎽⎺└␊ ├␊│├"
                            "some text"
                            "some text"
                            "⎽⎺└␊ ├␊│├"
                            "some text")
                 :cursor '(6 . 10))
    (output "\n\ensome text")
    (should-term :scrollback '("some text")
                 :display '("⎽⎺└␊ ├␊│├"
                            "some text"
                            "some text"
                            "⎽⎺└␊ ├␊│├"
                            "some text"
                            "some text")
                 :cursor '(6 . 10))
    (output "\n\e*0some text")
    (should-term :scrollback '("some text"
                               "⎽⎺└␊ ├␊│├")
                 :display '("some text"
                            "some text"
                            "⎽⎺└␊ ├␊│├"
                            "some text"
                            "some text"
                            "⎽⎺└␊ ├␊│├")
                 :cursor '(6 . 10))
    (output "\n\e*Bsome text")
    (should-term :scrollback '("some text"
                               "⎽⎺└␊ ├␊│├"
                               "some text")
                 :display '("some text"
                            "⎽⎺└␊ ├␊│├"
                            "some text"
                            "some text"
                            "⎽⎺└␊ ├␊│├"
                            "some text")
                 :cursor '(6 . 10))
    (output "\n\eosome text")
    (should-term :scrollback '("some text"
                               "⎽⎺└␊ ├␊│├"
                               "some text"
                               "some text")
                 :display '("⎽⎺└␊ ├␊│├"
                            "some text"
                            "some text"
                            "⎽⎺└␊ ├␊│├"
                            "some text"
                            "some text")
                 :cursor '(6 . 10))
    (output "\n\e+0some text")
    (should-term :scrollback '("some text"
                               "⎽⎺└␊ ├␊│├"
                               "some text"
                               "some text"
                               "⎽⎺└␊ ├␊│├")
                 :display '("some text"
                            "some text"
                            "⎽⎺└␊ ├␊│├"
                            "some text"
                            "some text"
                            "⎽⎺└␊ ├␊│├")
                 :cursor '(6 . 10))
    (output "\n\e+Bsome text")
    (should-term :scrollback '("some text"
                               "⎽⎺└␊ ├␊│├"
                               "some text"
                               "some text"
                               "⎽⎺└␊ ├␊│├"
                               "some text")
                 :display '("some text"
                            "⎽⎺└␊ ├␊│├"
                            "some text"
                            "some text"
                            "⎽⎺└␊ ├␊│├"
                            "some text")
                 :cursor '(6 . 10))
    (output "\n\C-osome text")
    (should-term :scrollback '("some text"
                               "⎽⎺└␊ ├␊│├"
                               "some text"
                               "some text"
                               "⎽⎺└␊ ├␊│├"
                               "some text"
                               "some text")
                 :display '("⎽⎺└␊ ├␊│├"
                            "some text"
                            "some text"
                            "⎽⎺└␊ ├␊│├"
                            "some text"
                            "some text")
                 :cursor '(6 . 10))
    (output "\n\e(0some text")
    (should-term :scrollback '("some text"
                               "⎽⎺└␊ ├␊│├"
                               "some text"
                               "some text"
                               "⎽⎺└␊ ├␊│├"
                               "some text"
                               "some text"
                               "⎽⎺└␊ ├␊│├")
                 :display '("some text"
                            "some text"
                            "⎽⎺└␊ ├␊│├"
                            "some text"
                            "some text"
                            "⎽⎺└␊ ├␊│├")
                 :cursor '(6 . 10))
    (output "\n\e(Bsome text")
    (should-term :scrollback '("some text"
                               "⎽⎺└␊ ├␊│├"
                               "some text"
                               "some text"
                               "⎽⎺└␊ ├␊│├"
                               "some text"
                               "some text"
                               "⎽⎺└␊ ├␊│├"
                               "some text")
                 :display '("some text"
                            "⎽⎺└␊ ├␊│├"
                            "some text"
                            "some text"
                            "⎽⎺└␊ ├␊│├"
                            "some text")
                 :cursor '(6 . 10))
    (output "\n\e(0+,-.0`abcdefghijklmnopqrstuvwxyz{|}~")
    (should-term :scrollback '("some text"
                               "⎽⎺└␊ ├␊│├"
                               "some text"
                               "some text"
                               "⎽⎺└␊ ├␊│├"
                               "some text"
                               "some text"
                               "⎽⎺└␊ ├␊│├"
                               "some text"
                               "some text"
                               "⎽⎺└␊ ├␊│├")
                 :display '("some text"
                            "some text"
                            "⎽⎺└␊ ├␊│├"
                            "some text"
                            "→←↑↓█�▒␉␌␍␊°±░#┘┐┌└┼"
                            "⎺⎻─⎼⎽├┤┴┬│≤≥π≠£•")
                 :cursor '(6 . 17))))

(ert-deftest eat-test-save-and-restore-cursor ()
  "Test saving and restoring cursor position.

Write plain text and newline to move cursor."
  (eat--tests-with-term '()
    (output "foo")
    (should-term :display '("foo")
                 :cursor '(1 . 4))
    (output "\e7")
    (should-term :display '("foo")
                 :cursor '(1 . 4))
    (output "bar\nfrob")
    (should-term :display '("foobar"
                            "frob")
                 :cursor '(2 . 5))
    (output "\e8")
    (should-term :display '("foobar"
                            "frob")
                 :cursor '(1 . 4))))


;;;;; Input Event Tests.

(ert-deftest eat-test-input-character ()
  "Test character input events.

This includes all events to which `self-insert-command' is bound to by
default."
  (eat--tests-with-term '()
    (dolist (c '(?a ?E ?b ?D ?অ ?আ ?ক ?খ ?ম ?া ?π ?θ ?μ ?Δ))
      (input-event c)
      (should (string= (input) (string c))))))

(ert-deftest eat-test-input-character-with-modifier ()
  "Test character input events with modifiers (`control' and `meta').

This includes all events to which `self-insert-command' is bound to by
default."
  (eat--tests-with-term '()
    (dolist (p '((?\C-\  . "\C-@")
                 (?\C-a . "\C-a")
                 (?\C-E . "\C-e")
                 (?\C-b . "\C-b")
                 (?\C-D . "\C-d")
                 (?\M-x . "\ex")
                 (?\M-O . "\e")
                 (?\M-\[ . "\e")
                 (?\M-C . "\eC")
                 (?\C-\M-U . "\e\C-u")
                 (?\M-ম . "\eম")
                 (?\M-া . "\eা")
                 (?\M-π . "\eπ")
                 (?\M-θ . "\eθ")))
      (input-event (car p))
      (should (string= (input) (cdr p))))))

(ert-deftest eat-test-input-special-keys ()
  "Test special key input events like arrow keys, backspace, etc."
  (eat--tests-with-term '()
    (cl-labels ((check (alist)
                  (dolist (p alist)
                    (input-event (car p))
                    (should (string= (input) (cdr p))))))
      (check '((backspace . "\C-?")
               (C-backspace . "\C-h")
               (left . "\e[D")
               (C-right . "\e[1;5C")
               (M-up . "\e[1;3A")
               (S-down . "\e[1;2B")
               (C-M-insert . "\e[2;7~")
               (C-S-delete . "\e[3;6~")
               (M-S-deletechar . "\e[3;4~")
               (C-M-S-home . "\e[1;8H")
               (C-S-end . "\e[1;6F")
               (M-S-prior . "\e[5;4~")
               (next . "\e[6~")))
      (output "\e[?1h")
      (check '((left . "\eOD")
               (up . "\eOA")
               (C-right . "\e[1;5C")
               (M-up . "\e[1;3A")
               (S-down . "\e[1;2B")))
      (output "\e[?1l")
      (check '((up . "\e[A"))))))

(ert-deftest eat-test-input-focus-events ()
  "Test focus events."
  (eat--tests-with-term '()
    (let ((focus-event-enabled nil))
      (setf (eat-term-grab-focus-events-function (terminal))
            (lambda (term value)
              (should (eq term (terminal)))
              (setq focus-event-enabled value)))
      (cl-labels ((check (alist)
                    (dolist (p alist)
                      (input-event (car p))
                      (should (string= (input) (cdr p))))))
        (check '(((eat-focus-in) . "")
                 ((eat-focus-out) . "")))
        (output "\e[?1004h")
        ;; The mode is MUST BE t, not non-nil.
        (should (eq focus-event-enabled t))
        (check '(((eat-focus-in) . "\e[I")
                 ((eat-focus-out) . "\e[O")))
        (output "\e[?1004l")
        (should (eq focus-event-enabled nil))
        (check '(((eat-focus-in) . "")
                 ((eat-focus-out) . "")))))))

(ert-deftest eat-test-input-mouse-events ()
  "Test focus events."
  (eat--tests-with-term '(:width 200 :height 200)
    (cl-letf* ((mouse-mode nil)
               ((eat-term-grab-mouse-function (terminal))
                (lambda (term mouse)
                  (should (eq term (terminal)))
                  (setq mouse-mode mouse)))
               ((symbol-function #'posn-col-row)
                (lambda (posn &optional _use-window)
                  (nth 6 posn))))
      (cl-labels ((check (alist)
                    (dolist (p alist)
                      (input-event (car p) (cadr p))
                      (should (string= (input) (caddr p)))))
                  (make-posn (&key (window (selected-window))
                                   pos-or-area (timestamp 0)
                                   (object (point-min)) text-pos
                                   (col 0) (row 0) (x col) (y row)
                                   image (dx x) (dy y) (width 1)
                                   (height 1))
                    `( ,window ,pos-or-area (,x . ,y) ,timestamp
                       ,object ,text-pos (,col . ,row) ,image
                       (,dx . ,dy) (,width . ,height))))

        ;; Default mouse event encoding.
        ;; Mouse mode disabled.
        (check
         `(((mouse-1 ,(make-posn)) nil "")
           ((mouse-2 ,(make-posn)) ,(make-posn) "")
           ((mouse-3 ,(make-posn :col 8 :row 3))
            ,(make-posn :col 1 :row 1) "")
           ((mouse-4 ,(make-posn :col 13 :row 4))
            ,(make-posn :col 5 :row 1) "")
           ((mouse-movement ,(make-posn :col 90 :row 40))
            ,(make-posn :col 82 :row 33) "")))

        ;; X10 mouse mode.
        (output "\e[?9h")
        (should (eq mouse-mode :click))
        (check
         `(((mouse-1 ,(make-posn)) nil "\e[M !!")
           ((mouse-2 ,(make-posn)) ,(make-posn) "\e[M!!!")
           ((mouse-3 ,(make-posn :col 10 :row 5))
            ,(make-posn :col 6 :row 3) "\e[M\"%#")
           ;; Position out of range of default mouse event encoding.
           ((mouse-1 ,(make-posn :col 95 :row 5)) nil "")
           ;; Position not inside terminal.
           ((mouse-2 ,(make-posn :col 94 :row 5))
            ,(make-posn :col 96 :row 5) "")
           ((mouse-2 ,(make-posn :col 95 :row 4))
            ,(make-posn :col 96 :row 8) "")
           ((mouse-4 ,(make-posn :col 24 :row 9))
            ,(make-posn :col 8 :row 4) "")
           ((mouse-movement ,(make-posn :col 30 :row 45))
            ,(make-posn :col 28 :row 42) "")))

        ;; Normal mouse mode.
        (output "\e[?1000h")
        (should (eq mouse-mode :modifier-click))
        (check
         `(((down-mouse-1 ,(make-posn)) nil "\e[M !!")
           ((mouse-1 ,(make-posn)) nil "\e[M#!!")
           ((down-mouse-2 ,(make-posn)) ,(make-posn) "\e[M!!!")
           ((mouse-2 ,(make-posn)) ,(make-posn) "\e[M#!!")
           ((down-mouse-3 ,(make-posn :col 10 :row 5))
            ,(make-posn) "\e[M\"+&")
           ;; Note that, although the mouse position has changed, it's
           ;; still unmoved relative to the reference position, so the
           ;; mouse position inputted should be same.
           ((drag-mouse-3 ,(make-posn :col 10 :row 5)
                          ,(make-posn :col 20 :row 50))
            ,(make-posn :col 10 :row 45) "\e[M#+&")
           ((mouse-4 ,(make-posn :col 33 :row 10))
            ,(make-posn :col 30 :row 5) "\e[M\`$&")
           ((C-mouse-5 ,(make-posn :col 4250 :row 3145))
            ,(make-posn :col 4242 :row 3141) "\e[Mq)%")
           ((M-mouse-6 ,(make-posn :col 425 :row 314))
            ,(make-posn :col 424 :row 314) "\e[Mj\"!")
           ((S-mouse-7 ,(make-posn :col 85 :row 20))
            ,(make-posn :col 32 :row 16) "\e[MgV%")
           ((C-M-down-mouse-1 ,(make-posn :col 58 :row 32))
            ,(make-posn :col 23 :row 31) "\e[M8D\"")
           ((C-M-mouse-1 ,(make-posn :col 58 :row 32))
            ,(make-posn :col 23 :row 31) "\e[M;D\"")
           ((C-S-down-mouse-2 ,(make-posn :col 40 :row 39))
            ,(make-posn :col 33 :row 13) "\e[M5(;")
           ;; Modifier changed!
           ((M-S-mouse-2 ,(make-posn :col 40 :row 39))
            ,(make-posn :col 33 :row 13) "\e[M/(;")
           ((C-M-S-down-mouse-3 ,(make-posn :col 48 :row 29))
            ,(make-posn :col 39 :row 23) "\e[M>*'")
           ;; Everything changed!
           ((mouse-3 ,(make-posn :col 92 :row 83))
            ,(make-posn :col 10 :row 8) "\e[M#sl")
           ;; Button out of range of default mouse event encoding.
           ((mouse-8 ,(make-posn :col 1 :row 1))
            ,(make-posn :col 1 :row 1) "")
           ((mouse-movement ,(make-posn :col 49 :row 85))
            ,(make-posn :col 45 :row 82) "")))

        ;; Button event mouse mode.
        (output "\e[?1002h")
        (should (eq mouse-mode :drag))
        (check
         `(((mouse-movement ,(make-posn :col 84 :row 10))
            ,(make-posn :col 48 :row 8) "")
           ((down-mouse-1 ,(make-posn :col 44 :row 88))
            ,(make-posn :col 44 :row 88) "\e[M !!")
           ((mouse-movement ,(make-posn :col 48 :row 1))
            ,(make-posn :col 19 :row 0) "\e[M@>\"")
           ((down-mouse-2 ,(make-posn :col 29 :row 21))
            ,(make-posn :col 18 :row 8) "\e[M!,.")
           ((mouse-movement ,(make-posn :col 93 :row 54))
            ,(make-posn :col 29 :row 38) "\e[M@a1")
           ((down-mouse-3 ,(make-posn :col 92 :row 63))
            ,(make-posn :col 32 :row 38) "\e[M\"]:")
           ((mouse-movement ,(make-posn :col 8 :row 92))
            ,(make-posn :col 3 :row 34) "\e[M@&[")
           ((mouse-1 ,(make-posn :col 93 :row 21))
            ,(make-posn :col 0 :row 0) "\e[M#~6")
           ((mouse-movement ,(make-posn :col 29 :row 74))
            ,(make-posn :col 7 :row 64) "\e[MA7+")
           ((mouse-2 ,(make-posn :col 28 :row 92))
            ,(make-posn :col 23 :row 29) "\e[M#&`")
           ((mouse-movement ,(make-posn :col 75 :row 36))
            ,(make-posn :col 75 :row 19) "\e[MB!2")
           ((mouse-3 ,(make-posn :col 36 :row 76))
            ,(make-posn :col 17 :row 67) "\e[M#4*")
           ((mouse-movement ,(make-posn :col 94 :row 58))
            ,(make-posn :col 54 :row 28) "")))

        ;; All event mouse mode.
        (output "\e[?1003h")
        (should (eq mouse-mode :all))
        (check
         `(((mouse-movement ,(make-posn :col 28 :row 38))
            ,(make-posn :col 4 :row 2) "\e[MC9E")
           ((mouse-movement ,(make-posn :col 49 :row 85))
            ,(make-posn :col 45 :row 82) "\e[MC%$")
           ((mouse-movement ,(make-posn :col 84 :row 10))
            ,(make-posn :col 48 :row 8) "\e[MCE#")
           ((down-mouse-1 ,(make-posn :col 44 :row 88))
            ,(make-posn :col 44 :row 88) "\e[M !!")
           ((mouse-movement ,(make-posn :col 48 :row 1))
            ,(make-posn :col 19 :row 0) "\e[M@>\"")
           ((down-mouse-2 ,(make-posn :col 29 :row 21))
            ,(make-posn :col 18 :row 8) "\e[M!,.")
           ((mouse-movement ,(make-posn :col 93 :row 54))
            ,(make-posn :col 29 :row 38) "\e[M@a1")
           ((down-mouse-3 ,(make-posn :col 92 :row 63))
            ,(make-posn :col 32 :row 38) "\e[M\"]:")
           ((mouse-movement ,(make-posn :col 8 :row 92))
            ,(make-posn :col 3 :row 34) "\e[M@&[")
           ((mouse-1 ,(make-posn :col 93 :row 21))
            ,(make-posn :col 0 :row 0) "\e[M#~6")
           ((mouse-movement ,(make-posn :col 29 :row 74))
            ,(make-posn :col 7 :row 64) "\e[MA7+")
           ((mouse-2 ,(make-posn :col 28 :row 92))
            ,(make-posn :col 23 :row 29) "\e[M#&`")
           ((mouse-movement ,(make-posn :col 75 :row 36))
            ,(make-posn :col 75 :row 19) "\e[MB!2")
           ((mouse-3 ,(make-posn :col 36 :row 76))
            ,(make-posn :col 17 :row 67) "\e[M#4*")
           ((mouse-movement ,(make-posn :col 94 :row 58))
            ,(make-posn :col 54 :row 28) "\e[MCI?")
           ((mouse-movement ,(make-posn :col 97 :row 79))
            ,(make-posn :col 46 :row 69) "\e[MCT+")
           ((mouse-movement ,(make-posn :col 34 :row 76))
            ,(make-posn :col 28 :row 29) "\e[MC'P")))

        ;; Mouse mode disabled.
        (output "\e[?1003l")
        (should (eq mouse-mode nil))
        (check
         `(((mouse-1 ,(make-posn)) nil "")
           ((mouse-2 ,(make-posn)) ,(make-posn) "")
           ((mouse-3 ,(make-posn :col 8 :row 3))
            ,(make-posn :col 1 :row 1) "")
           ((mouse-4 ,(make-posn :col 13 :row 4))
            ,(make-posn :col 5 :row 1) "")
           ((mouse-movement ,(make-posn :col 90 :row 40))
            ,(make-posn :col 82 :row 33) "")))

        ;; SGR mouse event encoding.
        (output "\e[?1006h")

        ;; Mouse mode disabled.
        (check
         `(((mouse-1 ,(make-posn)) nil "")
           ((mouse-2 ,(make-posn)) ,(make-posn) "")
           ((mouse-3 ,(make-posn :col 8 :row 3))
            ,(make-posn :col 1 :row 1) "")
           ((mouse-4 ,(make-posn :col 13 :row 4))
            ,(make-posn :col 5 :row 1) "")
           ((mouse-movement ,(make-posn :col 90 :row 40))
            ,(make-posn :col 82 :row 33) "")))

        ;; X10 mouse mode.
        (output "\e[?9h")
        (should (eq mouse-mode :click))
        (check
         `(((mouse-1 ,(make-posn)) nil "\e[<0;1;1M")
           ((mouse-2 ,(make-posn)) ,(make-posn) "\e[<1;1;1M")
           ((mouse-3 ,(make-posn :col 10 :row 5))
            ,(make-posn :col 6 :row 3) "\e[<2;5;3M")
           ((mouse-1 ,(make-posn :col 95 :row 5)) nil "\e[<0;96;6M")
           ;; Position not inside terminal.
           ((mouse-2 ,(make-posn :col 94 :row 5))
            ,(make-posn :col 96 :row 5) "")
           ((mouse-2 ,(make-posn :col 95 :row 4))
            ,(make-posn :col 96 :row 8) "")
           ((mouse-4 ,(make-posn :col 24 :row 9))
            ,(make-posn :col 8 :row 4) "")
           ((mouse-movement ,(make-posn :col 30 :row 45))
            ,(make-posn :col 28 :row 42) "")))

        ;; Normal mouse mode.
        (output "\e[?1000h")
        (should (eq mouse-mode :modifier-click))
        (check
         `(((down-mouse-1 ,(make-posn)) nil "\e[<0;1;1M")
           ((mouse-1 ,(make-posn)) nil "\e[<0;1;1m")
           ((down-mouse-2 ,(make-posn)) ,(make-posn) "\e[<1;1;1M")
           ((mouse-2 ,(make-posn)) ,(make-posn) "\e[<1;1;1m")
           ((down-mouse-3 ,(make-posn :col 10 :row 5))
            ,(make-posn) "\e[<2;11;6M")
           ;; Note that, although the mouse position has changed, it's
           ;; still unmoved relative to the reference position, so the
           ;; mouse position inputted should be same.
           ((drag-mouse-3 ,(make-posn :col 10 :row 5)
                          ,(make-posn :col 20 :row 50))
            ,(make-posn :col 10 :row 45) "\e[<2;11;6m")
           ((mouse-4 ,(make-posn :col 33 :row 10))
            ,(make-posn :col 30 :row 5) "\e[<64;4;6M")
           ((C-mouse-5 ,(make-posn :col 4250 :row 3145))
            ,(make-posn :col 4242 :row 3141) "\e[<81;9;5M")
           ((M-mouse-6 ,(make-posn :col 425 :row 314))
            ,(make-posn :col 424 :row 314) "\e[<74;2;1M")
           ((S-mouse-7 ,(make-posn :col 85 :row 20))
            ,(make-posn :col 32 :row 16) "\e[<71;54;5M")
           ((mouse-8 ,(make-posn :col 91 :row 92))
            ,(make-posn :col 75 :row 18) "\e[<128;17;75M")
           ((mouse-9 ,(make-posn :col 71 :row 81))
            ,(make-posn :col 37 :row 72) "\e[<129;35;10M")
           ((mouse-10 ,(make-posn :col 38 :row 92))
            ,(make-posn :col 29 :row 90) "\e[<130;10;3M")
           ((mouse-11 ,(make-posn :col 29 :row 14))
            ,(make-posn :col 28 :row 8) "\e[<131;2;7M")
           ((C-M-down-mouse-1 ,(make-posn :col 58 :row 32))
            ,(make-posn :col 23 :row 31) "\e[<24;36;2M")
           ((C-M-mouse-1 ,(make-posn :col 58 :row 32))
            ,(make-posn :col 23 :row 31) "\e[<24;36;2m")
           ((C-S-down-mouse-2 ,(make-posn :col 40 :row 39))
            ,(make-posn :col 33 :row 13) "\e[<21;8;27M")
           ;; Modifier changed!
           ((M-S-mouse-2 ,(make-posn :col 40 :row 39))
            ,(make-posn :col 33 :row 13) "\e[<13;8;27m")
           ((C-M-S-down-mouse-3 ,(make-posn :col 48 :row 29))
            ,(make-posn :col 39 :row 23) "\e[<30;10;7M")
           ;; Everything changed!
           ((mouse-3 ,(make-posn :col 92 :row 83))
            ,(make-posn :col 10 :row 8) "\e[<2;83;76m")
           ((mouse-movement ,(make-posn :col 49 :row 85))
            ,(make-posn :col 45 :row 82) "")))

        ;; Button event mouse mode.
        (output "\e[?1002h")
        (should (eq mouse-mode :drag))
        (check
         `(((mouse-movement ,(make-posn :col 84 :row 10))
            ,(make-posn :col 48 :row 8) "")
           ((down-mouse-1 ,(make-posn :col 44 :row 88))
            ,(make-posn :col 44 :row 88) "\e[<0;1;1M")
           ((mouse-movement ,(make-posn :col 48 :row 1))
            ,(make-posn :col 19 :row 0) "\e[<32;30;2M")
           ((down-mouse-2 ,(make-posn :col 29 :row 21))
            ,(make-posn :col 18 :row 8) "\e[<1;12;14M")
           ((mouse-movement ,(make-posn :col 93 :row 54))
            ,(make-posn :col 29 :row 38) "\e[<32;65;17M")
           ((down-mouse-3 ,(make-posn :col 92 :row 63))
            ,(make-posn :col 32 :row 38) "\e[<2;61;26M")
           ((mouse-movement ,(make-posn :col 8 :row 92))
            ,(make-posn :col 3 :row 34) "\e[<32;6;59M")
           ((mouse-1 ,(make-posn :col 93 :row 21))
            ,(make-posn :col 0 :row 0) "\e[<0;94;22m")
           ((mouse-movement ,(make-posn :col 29 :row 74))
            ,(make-posn :col 7 :row 64) "\e[<33;23;11M")
           ((mouse-2 ,(make-posn :col 28 :row 92))
            ,(make-posn :col 23 :row 29) "\e[<1;6;64m")
           ((mouse-movement ,(make-posn :col 75 :row 36))
            ,(make-posn :col 75 :row 19) "\e[<34;1;18M")
           ((mouse-3 ,(make-posn :col 36 :row 76))
            ,(make-posn :col 17 :row 67) "\e[<2;20;10m")
           ((mouse-movement ,(make-posn :col 94 :row 58))
            ,(make-posn :col 54 :row 28) "")))

        ;; All event mouse mode.
        (output "\e[?1003h")
        (should (eq mouse-mode :all))
        (check
         `(((mouse-movement ,(make-posn :col 28 :row 38))
            ,(make-posn :col 4 :row 2) "\e[<35;25;37M")
           ((mouse-movement ,(make-posn :col 49 :row 85))
            ,(make-posn :col 45 :row 82) "\e[<35;5;4M")
           ((mouse-movement ,(make-posn :col 84 :row 10))
            ,(make-posn :col 48 :row 8) "\e[<35;37;3M")
           ((down-mouse-1 ,(make-posn :col 44 :row 88))
            ,(make-posn :col 44 :row 88) "\e[<0;1;1M")
           ((mouse-movement ,(make-posn :col 48 :row 1))
            ,(make-posn :col 19 :row 0) "\e[<32;30;2M")
           ((down-mouse-2 ,(make-posn :col 29 :row 21))
            ,(make-posn :col 18 :row 8) "\e[<1;12;14M")
           ((mouse-movement ,(make-posn :col 93 :row 54))
            ,(make-posn :col 29 :row 38) "\e[<32;65;17M")
           ((down-mouse-3 ,(make-posn :col 92 :row 63))
            ,(make-posn :col 32 :row 38) "\e[<2;61;26M")
           ((mouse-movement ,(make-posn :col 8 :row 92))
            ,(make-posn :col 3 :row 34) "\e[<32;6;59M")
           ((mouse-1 ,(make-posn :col 93 :row 21))
            ,(make-posn :col 0 :row 0) "\e[<0;94;22m")
           ((mouse-movement ,(make-posn :col 29 :row 74))
            ,(make-posn :col 7 :row 64) "\e[<33;23;11M")
           ((mouse-2 ,(make-posn :col 28 :row 92))
            ,(make-posn :col 23 :row 29) "\e[<1;6;64m")
           ((mouse-movement ,(make-posn :col 75 :row 36))
            ,(make-posn :col 75 :row 19) "\e[<34;1;18M")
           ((mouse-3 ,(make-posn :col 36 :row 76))
            ,(make-posn :col 17 :row 67) "\e[<2;20;10m")
           ((mouse-movement ,(make-posn :col 94 :row 58))
            ,(make-posn :col 54 :row 28) "\e[<35;41;31M")
           ((mouse-movement ,(make-posn :col 97 :row 79))
            ,(make-posn :col 46 :row 69) "\e[<35;52;11M")
           ((mouse-movement ,(make-posn :col 34 :row 76))
            ,(make-posn :col 28 :row 29) "\e[<35;7;48M")))

        ;; Default mouse encoding.
        (output "\e[?1006l")
        (check
         `(((mouse-movement ,(make-posn :col 28 :row 38))
            ,(make-posn :col 4 :row 2) "\e[MC9E")
           ((mouse-movement ,(make-posn :col 49 :row 85))
            ,(make-posn :col 45 :row 82) "\e[MC%$")
           ((mouse-movement ,(make-posn :col 84 :row 10))
            ,(make-posn :col 48 :row 8) "\e[MCE#")
           ((down-mouse-1 ,(make-posn :col 44 :row 88))
            ,(make-posn :col 44 :row 88) "\e[M !!")
           ((mouse-movement ,(make-posn :col 48 :row 1))
            ,(make-posn :col 19 :row 0) "\e[M@>\"")
           ((down-mouse-2 ,(make-posn :col 29 :row 21))
            ,(make-posn :col 18 :row 8) "\e[M!,.")
           ((mouse-movement ,(make-posn :col 93 :row 54))
            ,(make-posn :col 29 :row 38) "\e[M@a1")
           ((down-mouse-3 ,(make-posn :col 92 :row 63))
            ,(make-posn :col 32 :row 38) "\e[M\"]:")
           ((mouse-movement ,(make-posn :col 8 :row 92))
            ,(make-posn :col 3 :row 34) "\e[M@&[")
           ((mouse-1 ,(make-posn :col 93 :row 21))
            ,(make-posn :col 0 :row 0) "\e[M#~6")
           ((mouse-movement ,(make-posn :col 29 :row 74))
            ,(make-posn :col 7 :row 64) "\e[MA7+")
           ((mouse-2 ,(make-posn :col 28 :row 92))
            ,(make-posn :col 23 :row 29) "\e[M#&`")
           ((mouse-movement ,(make-posn :col 75 :row 36))
            ,(make-posn :col 75 :row 19) "\e[MB!2")
           ((mouse-3 ,(make-posn :col 36 :row 76))
            ,(make-posn :col 17 :row 67) "\e[M#4*")
           ((mouse-movement ,(make-posn :col 94 :row 58))
            ,(make-posn :col 54 :row 28) "\e[MCI?")
           ((mouse-movement ,(make-posn :col 97 :row 79))
            ,(make-posn :col 46 :row 69) "\e[MCT+")
           ((mouse-movement ,(make-posn :col 34 :row 76))
            ,(make-posn :col 28 :row 29) "\e[MC'P")))

        ;; Mouse mode disabled.
        ;; Disabling any mouse mode, either enabled or not, should
        ;; disable mouse.
        (output "\e[?9l")
        (should (eq mouse-mode nil))
        (check
         `(((mouse-1 ,(make-posn)) nil "")
           ((mouse-2 ,(make-posn)) ,(make-posn) "")
           ((mouse-3 ,(make-posn :col 8 :row 3))
            ,(make-posn :col 1 :row 1) "")
           ((mouse-4 ,(make-posn :col 13 :row 4))
            ,(make-posn :col 5 :row 1) "")
           ((mouse-movement ,(make-posn :col 90 :row 40))
            ,(make-posn :col 82 :row 33) "")))))))

(provide 'eat-tests)
;;; eat-tests.el ends here
