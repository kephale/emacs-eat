;;; eat.el --- Emulate A Terminal, in a region, in a buffer and in Eshell -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-08-15
;; Version: 0.2.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: terminals processes
;; Homepage: https://codeberg.org/akib/emacs-eat

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

;; Eat's name self-explainary, it stands for "Emulate A Terminal".
;; Eat is a terminal emulator.  It can run most (if not all)
;; full-screen terminal programs, including Emacs.

;; It is pretty fast, more than three times faster than Term, despite
;; being implemented entirely in Emacs Lisp.  So fast that you can
;; comfortably run Emacs inside Eat, or even use your Emacs as a
;; terminal multiplexer.

;; It has many feature that other Emacs terminal emulator still don't
;; have, for example complete mouse support.

;; It flickers less than other Emacs terminal emulator, so you get
;; more performance and a smooth experience.

;; To start Eat, run M-x eat.  Eat has three keybinding modes:

;;   * "emacs" mode: No special keybinding, except the following:

;;       * `C-c' `C-s': Switch to semi-char mode.
;;       * `C-c' `C-j': Switch to char mode.
;;       * `C-c' `C-k': Kill process.

;;   * "semi-char" mode: Most keys are bound to send the key to the
;;     terminal, except the following keys: `C-\', `C-c', `C-x',
;;     `C-g', `C-h', `C-M-c', `C-u', `M-x', `M-:', `M-!', `M-&'.  The
;;     following special keybinding are available:

;;       * `C-q': Send next key to the terminal.
;;       * `C-y': Like `yank', but send the text to the terminal.
;;       * `M-y': Like `yank-pop', but send the text to the terminal.
;;       * `C-c' `C-k': Kill process.

;;   * "char" mode: All supported keys are bound to send the key to
;;     the terminal, except `C-M-m' or `M-RET', which is bound to
;;     switch to semi-char mode.

;; If you like Eshell, then there is a good news for you.  Eat
;; integrates with Eshell.  Eat has two global minor modes for Eshell:

;;   * `eat-eshell-visual-command-mode': Run visual commands with Eat
;;     instead of Term.

;;   * `eat-eshell-mode': Run Eat inside Eshell.  After enabling this,
;;     you can run full-screen terminal programs directly in Eshell.
;;     You have three keybinding modes here too, except that `C-c'
;;     `C-k' is not special (i.e. not bound by Eat) in "emacs" mode
;;     and "line" mode.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'ansi-color)
(require 'shell)
(require 'url)


;;;; User Options.

(defgroup eat nil
  "Emulate A Terminal."
  :group 'processes
  :group 'terminals
  :link '(url-link "https://codeberg.org/akib/emacs-eat"))

(defgroup eat-term nil
  "Eat terminal emulator."
  :group 'eat)

(defgroup eat-ui nil
  "Eat user interface."
  :group 'eat)

(defgroup eat-eshell nil
  "Eat Eshell integration."
  :group 'eat)

(defcustom eat-buffer-name "*eat*"
  "The basename used for Eat buffers.

This is the default name used when running Eat."
  :type 'string
  :group 'eat-ui)

(defcustom eat-kill-buffer-on-exit nil
  "Non-nil means automatically kill Eat buffer when process exits."
  :type 'boolean
  :group 'eat-ui)

(defcustom eat-term-scrollback-size 131072 ; 128 K
  "Size of scrollback area in characters.  nil means unlimited."
  :type '(choice integer (const nil))
  :group 'eat-term
  :group 'eat-ui)

(defcustom eat-enable-kill-from-terminal t
  "Non-nil means allow terminal program to add text to `kill-ring'.

When non-nil, terminal program can send special escape sequence to add
some text to `kill-ring'."
  :type 'boolean
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-enable-yank-to-terminal nil
  "Non-nil means allow terminal program to get text from `kill-ring'.

When non-nil, terminal program can get killed text from `kill-ring'.
This is left disabled for security reasons."
  :type 'boolean
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-enable-directory-tracking t
  "Non-nil means do directory tracking.

When non-nil, Eat will track the working directory of program.  You
need to configure the program to send current working directory
information.  See Info node `(eat)Directory Tracking' for instructions
to setup your shell."
  :type 'boolean
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-enable-shell-command-history t
  "Non-nil means add shell commands to Emacs history.

When non-nil, any command you run in your shell will also appear in
the history of commands like `eat', `shell-command' and
`async-shell-command'."
  :type 'boolean
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-enable-shell-prompt-annotation t
  "Non-nil means annotate shell prompt with the status of command.

When non-nil, display a mark in front of shell prompt describing the
status of the command executed in that prompt."
  :type 'boolean
  :group 'eat-ui)

(defcustom eat-shell-prompt-annotation-position 'left-margin
  "The position where to display shell prompt annotation.

The value can be one of the following:

`left-margin'   Use the left margin.
`right-margin'  Use the right margin."
  :type '(choice (const :tag "Left margin" left-margin)
                 (const :tag "Right margin" right-margin))
  :group 'eat-ui)

(defcustom eat-shell-prompt-annotation-running-margin-indicator "-"
  "String in margin annotation to indicate the command is running."
  :type 'string
  :group 'eat-ui)

(defface eat-shell-prompt-annotation-running
  '((t :inherit compilation-info))
  "Face used in annotation to indicate the command is running."
  :group 'eat-ui)

(defcustom eat-shell-prompt-annotation-success-margin-indicator "0"
  "String in margin annotation to indicate the command has succeeded."
  :type 'string
  :group 'eat-ui)

(defface eat-shell-prompt-annotation-success
  '((t :inherit success))
  "Face used in annotation to indicate the command has succeeded."
  :group 'eat-ui)

(defcustom eat-shell-prompt-annotation-failure-margin-indicator "X"
  "String in margin annotation to indicate the command has failed."
  :type 'string
  :group 'eat-ui)

(defface eat-shell-prompt-annotation-failure
  '((t :inherit error))
  "Face used in annotation to indicate the command has failed."
  :group 'eat-ui)

(defcustom eat-shell-prompt-annotation-correction-delay 0.1
  "Seconds to wait before correcting shell prompt annotations.

Wait this many second after terminal update before correcting shell
prompt annotation."
  :type 'number
  :group 'eat-ui)

(defconst eat--cursor-type-value-type
  (let ((cur-type
         '(choice
           (const :tag "Frame default" t)
           (const :tag "Filled box" box)
           (cons :tag "Box with specified size" (const box) integer)
           (const :tag "Hollow cursor" hollow)
           (const :tag "Vertical bar" bar)
           (cons :tag "Vertical bar with specified height" (const bar)
                 integer)
           (const :tag "Horizontal bar" hbar)
           (cons :tag "Horizontal bar with specified width"
                 (const hbar) integer)
           (const :tag "None " nil))))
    `(list
      ,cur-type
      (choice
       (const :tag "No blinking" nil)
       (number :tag "Blinking frequency"))
      ,cur-type))
  "Custom type specification for Eat's cursor type variables.")

(defcustom eat-default-cursor-type
  `(,(default-value 'cursor-type) nil nil)
  "Cursor to use in Eat buffer.

The value is a list of form (CURSOR-ON BLINKING-FREQUENCY CURSOR-OFF).

When the cursor is on, CURSOR-ON is used as `cursor-type', which see.
BLINKING-FREQUENCY is the blinking frequency of cursor's blinking.
When the cursor is off, CURSOR-OFF is used as `cursor-type'.  This
should be nil when cursor is not blinking."
  :type eat--cursor-type-value-type
  :group 'eat-ui
  :group 'eat-ehell)

(defcustom eat-invisible-cursor-type '(nil nil nil)
  "Invisible cursor to use in Eat buffer.

The value is a list of form (CURSOR-ON BLINKING-FREQUENCY CURSOR-OFF).

When the cursor is on, CURSOR-ON is used as `cursor-type', which see.
BLINKING-FREQUENCY is the blinking frequency of cursor's blinking.
When the cursor is off, CURSOR-OFF is used as `cursor-type'.  This
should be nil when cursor is not blinking."
  :type eat--cursor-type-value-type
  :group 'eat-ui
  :group 'eat-ehell)

(defcustom eat-very-visible-cursor-type
  `(,(default-value 'cursor-type) 2 hollow)
  "Very visible cursor to use in Eat buffer.

When the cursor is invisible, the car of the value is used as
`cursor-type', which see.  The cdr of the value, when non-nil, is the
blinking frequency of cursor."
  :type eat--cursor-type-value-type
  :group 'eat-ui
  :group 'eat-ehell)

(defcustom eat-minimum-latency 0.008
  "Minimum display latency in seconds.

Lowering it too much may cause (or increase) flickering and decrease
performance due to too many redisplays.  Increasing it too much will
cause the terminal to feel less responsive.  Try to increase this
value if the terminal flickers."
  :type 'number
  :group 'eat-ui
  :group 'eat-ehell)

(defcustom eat-maximum-latency 0.033
  "Minimum display latency in seconds.

Increasing it too much may make the terminal feel less responsive in
case of huge burst of output.  Try to increase this value if the
terminal flickers.  Try to lower the value if the terminal feels less
responsive."
  :type 'number
  :group 'eat-ui
  :group 'eat-ehell)

(defcustom eat-term-name #'eat-term-get-suitable-term-name
  "Value for the `TERM' environment variable.

The value can also be a function.  In that case, the function is
called without any argument and the return value is used as the value.
For example, this can set to `eat-term-get-suitable-term-name' to set
the value according to the number of colors supported by the current
display.

This value is used by terminal programs to identify the terminal."
  :type '(choice
          (string :tag "Value")
          (const :tag "Automatic" eat-term-get-suitable-term-name)
          (function :tag "Function"))
  :group 'eat-term)

;; Upgrading Eat causes `eat-term-terminfo-directory' and
;; `eat-term-shell-integration-directory' to be outdated, so update it
;; if not modified by user (or something else).
(defvar eat--install-path nil
  "Path to directory where Eat is installed.")

(defvar eat--shell-integration-path nil
  "Path to directory where shell integration scripts are installed.")

(defvar eat-term-terminfo-directory)
(defvar eat-term-shell-integration-directory)
(let ((old-install-path eat--install-path)
      (old-shell-integration-path eat--shell-integration-path))
  (setq eat--install-path
        (copy-sequence (file-name-directory
                        (or load-file-name buffer-file-name))))
  (setq eat--shell-integration-path
        (expand-file-name "integration" eat--install-path))

  (defcustom eat-term-terminfo-directory eat--install-path
    "Directory where required terminfo databases can be found.

This value is used by terminal programs to find the terminfo databases
that describe the capabilities of the terminal."
    :type 'directory
    :group 'eat-term)

  (defvar eat--term-shell-integration-directory)

  (defcustom eat-term-shell-integration-directory
    eat--shell-integration-path
    "Directory where Eat shell integration scripts can be found.

This value is exposed to terminal programs as
`EAT_SHELL_INTEGRATION_DIR' environment variable."
    :type 'directory
    :group 'eat-ui
    :group 'eat-eshell)

  (when (eq eat-term-terminfo-directory old-install-path)
    (setq eat-term-terminfo-directory eat--install-path))
  (when (eq eat-term-shell-integration-directory
            old-shell-integration-path)
    (setq eat-term-shell-integration-directory
          eat--shell-integration-path)))

(defcustom eat-term-inside-emacs (format "%s,eat" emacs-version)
  "Value for the `INSIDE_EMACS' environment variable."
  :type 'string
  :group 'eat-term)

(defcustom eat-enable-blinking-text nil
  "Non-nil means enable blinking of text with blink attribute.

When non-nil, enable `eat-blink-mode' to enable blinking of text with
blink attribute by default.  You manually toggle `eat-blink-mode' to
toggle this behavior buffer-locally."
  :type 'boolean
  :group 'eat-ui
  :group 'eat-eshell)

(defcustom eat-slow-blink-frequency 2
  "Frequency of blinking of slowly text.

This has an effect only if `eat-blink-mode' is enabled."
  :type 'number
  :group 'eat-ui)

(defcustom eat-fast-blink-frequency 3
  "Frequency of blinking of rapidly text.

This has an effect only if `eat-blink-mode' is enabled."
  :type 'number
  :group 'eat-ui)

(defcustom eat-enable-alternative-display t
  "Non-nil means enable alternative display.

Full screen programs often use alternative display to keep old
contents on display unaltered."
  :type 'boolean
  :group 'eat-term)

(defcustom eat-enable-mouse t
  "Non-nil means enable mouse support.

When non-nil, terminal programs can receive mouse events from Emacs."
  :type 'boolean
  :group 'eat-ui)

(defcustom eat-input-chunk-size 1024
  "Maximum size of chunk of data send at once.

Long inputs send to Eat processes are broken up into chunks of this
size.

If your process is choking on big inputs, try lowering the value."
  :type 'integer
  :group 'eat-ui)

(defface eat-term-bold '((t :inherit bold))
  "Face used to render bold text."
  :group 'eat-term)

(defface eat-term-faint '((t :weight light))
  "Face used to render faint text."
  :group 'eat-term)

(defface eat-term-italic '((t :inherit italic))
  "Face used to render italic text."
  :group 'eat-term)

(defface eat-term-slow-blink '((t :inverse-video t))
  "Face used to render slowly blinking text."
  :group 'eat-term)

(defface eat-term-fast-blink '((t :inverse-video t))
  "Face used to render rapidly blinking text."
  :group 'eat-term)

;; Define color faces.
(let ((face-counter 0))
  (let ((colors '("black" "red" "green" "yellow" "blue" "magenta"
                  "cyan" "white")))
    ;; Basic colors.
    (dolist (color colors)
      (let ((face (intern (format "eat-term-color-%i" face-counter))))
        (custom-declare-face
         face `((t :inherit ,(intern (format "ansi-color-%s" color))))
         (format "Face used to render %s color text." color)
         :group 'eat-term)
        (put (intern (format "eat-term-color-%s" color))
             'face-alias face))
      (cl-incf face-counter))
    ;; Bright colors.
    (dolist (color colors)
      (let ((face (intern (format "eat-term-color-%i" face-counter))))
        (custom-declare-face
         face `((t :inherit ,(intern (format "ansi-color-bright-%s"
                                             color))))
         (format "Face used to render bright %s color text." color)
         :group 'eat-term)
        (put (intern (format "eat-term-color-bright-%s" color))
             'face-alias face))
      (cl-incf face-counter)))
  ;; 256-colors.
  (while (< face-counter 256)
    (let ((color
           (if (>= face-counter 232)
               (format "#%06X"
                       (* #x010101
                          (+ 8 (* 10 (- face-counter 232)))))
             (let ((col (- face-counter 16))
                   (res 0)
                   (frac (* 6 6)))
               (while (<= 1 frac)
                 (setq res (* res #x000100))
                 (let ((color-num (mod (/ col frac) 6)))
                   (unless (zerop color-num)
                     (setq res (+ res #x37 (* #x28 color-num)))))
                 (setq frac (/ frac 6)))
               (format "#%06X" res)))))
      (custom-declare-face
       (intern (format "eat-term-color-%i" face-counter))
       `((t :foreground ,color :background ,color))
       (format "Face used to render text with %i%s color of 256 color\
 palette."
               face-counter
               (or (and (not (<= 11 (% face-counter 100) 13))
                        (nth (% face-counter 10)
                             '(nil "st" "nd" "rd")))
                   "th"))
       :group 'eat-term))
    (cl-incf face-counter)))

(defface eat-term-font-0 '((t))
  "Default font."
  :group 'eat-term)

(put 'eat-term-font-default 'face-alias 'eat-term-font-0)

;; Font faces, 1 to 9 (inclusive).
(cl-loop for counter from 1 to 9
         do (custom-declare-face
             (intern (format "eat-term-font-%i" counter)) '((t))
             (format "Alternative font %i." counter)
             :group 'eat-term))


;;;; Utility Functions.

(defun eat--t-goto-bol (&optional n)
  "Go to the beginning of current line.

With optional argument N, go to the beginning of Nth next line if N is
positive, otherwise go to the beginning of -Nth previous line.  If the
specified position is before `point-min' or after `point-max', go to
that point.

Return the number of lines moved.

Treat LINE FEED (?\\n) as the line delimiter."
  ;; TODO: Comment.
  (setq n (or n 0))
  (cond ((> n 0)
         (let ((moved 0))
           (while (and (< (point) (point-max))
                       (< moved n))
             (and (search-forward "\n" nil 'move)
                  (cl-incf moved)))
           moved))
        ((<= n 0)
         (let ((moved 1))
           (while (and (or (= moved 1)
                           (< (point-min) (point)))
                       (< n moved))
             (cl-decf moved)
             (and (search-backward "\n" nil 'move)
                  (= moved n)
                  (goto-char (match-end 0))))
           moved))))

(defun eat--t-goto-eol (&optional n)
  "Go to the end of current line.

With optional argument N, go to the end of Nth next line if N is
positive, otherwise go to the end of -Nth previous line.  If the
specified position is before `point-min' or after `point-max', go to
that point.

Return the number of lines moved.

Treat LINE FEED (?\\n) as the line delimiter."
  ;; TODO: Comment.
  (setq n (or n 0))
  (cond ((>= n 0)
         (let ((moved -1))
           (while (and (or (= moved -1)
                           (< (point) (point-max)))
                       (< moved n))
             (cl-incf moved)
             (and (search-forward "\n" nil 'move)
                  (= moved n)
                  (goto-char (match-beginning 0))))
           moved))
        ((< n 0)
         (let ((moved 0))
           (while (and (< (point-min) (point))
                       (< n moved))
             (and (search-backward "\n" nil 'move)
                  (cl-decf moved)))
           moved))))

(defun eat--t-bol (&optional n)
  "Return the beginning of current line.

With optional argument N, return a cons cell whose car is the
beginning of Nth next line and cdr is N, if N is positive, otherwise
return a cons cell whose car is the beginning of -Nth previous line
and cdr is N.  If the specified position is before `point-min' or
after `point-max', return a cons cell whose car is that point and cdr
is number of lines that point is away from current line.

Treat LINE FEED (?\\n) as the line delimiter."
  ;; Move to the beginning of line, record the point, and return that
  ;; point and the distance of that point from current line in lines.
  (save-excursion
    ;; `let' is neccessary, we need to evaluate (point) after going to
    ;; `(eat--t-goto-bol N)'.
    (let ((moved (eat--t-goto-bol n)))
      (cons (point) moved))))

(defun eat--t-eol (&optional n)
  "Return the end of current line.

With optional argument N, return a cons cell whose car the end of Nth
next line and cdr is N, if N is positive, otherwise return a cons cell
whose car is the end of -Nth previous line and cdr in N.  If the
specified position is before `point-min' or after `point-max', return
a cons cell whose car is that point and cdr is number of lines that
point is away from current line.

Treat LINE FEED (?\\n) as the line delimiter."
  ;; Move to the beginning of line, record the point, and return that
  ;; point and the distance of that point from current line in lines.
  (save-excursion
    ;; `let' is neccessary, we need to evaluate (point) after going to
    ;; (eat--t-goto-eol N).
    (let ((moved (eat--t-goto-eol n)))
      (cons (point) moved))))

(defun eat--t-col-motion (n)
  "Move to Nth next column.

Go to Nth next column if N is positive, otherwise go to -Nth previous
column.  If the specified position is before `point-min' or after
`point-max', go to that point.

Return the number of columns moved.

Assume all characters occupy a single column."
  ;; Record the current position.
  (let ((start-pos (point)))
    ;; Move to the new position.
    (cond ((> n 0)
           (let ((eol (car (eat--t-eol)))
                 (pos (+ (point) n)))
             (goto-char (min pos eol))))
          ((< n 0)
           (let ((bol (car (eat--t-bol)))
                 (pos (+ (point) n)))
             (goto-char (max pos bol)))))
    ;; Return the distance from the previous position.
    (- (point) start-pos)))

(defun eat--t-current-col ()
  "Return the current column.

Assume all characters occupy a single column."
  ;; We assume that that all characters occupy a single column, so a
  ;; subtraction should work.  For multi-column characters, we add
  ;; extra invisible spaces before the character to make it occupy as
  ;; many character is its width.
  (- (point) (car (eat--t-bol))))

(defun eat--t-goto-col (n)
  "Go to column N.

Return the current column after moving point.

Assume all characters occupy a single column."
  ;; Move to column 0.
  (eat--t-goto-bol)
  ;; Now the target column is N characters away.
  (eat--t-col-motion n))

(defun eat--t-repeated-insert (c n &optional face)
  "Insert character C, N times, using face FACE, if given."
  (insert (if face
              (let ((str (make-string n c)))
                (put-text-property 0 n 'face face str)
                str)
            ;; Avoid the `let'.
            (make-string n c))))

(defun eat--t-join-long-line (&optional limit)
  "Join long line once, but don't try to go beyond LIMIT.

For example: \"*foo\\nbar\\nbaz\" is converted to \"foo*bar\\nbaz\",
where `*' indicates point."
  ;; Are we already at the end a part of a long line?
  (unless (get-char-property (point) 'eat--t-wrap-line)
    ;; Find the next end of a part of a long line.
    (goto-char (next-single-char-property-change
                (point) 'eat--t-wrap-line nil limit)))
  ;; Remove the newline.
  (when (< (point) (or limit (point-max)))
    (1value (cl-assert (1value (= (1value (char-after)) ?\n))))
    (delete-char 1)))

(defun eat--t-break-long-line (threshold)
  "Break a line longer than THRESHOLD once.

For example: when THRESHOLD is 3, \"*foobarbaz\" is converted to
\"foo\\n*barbaz\", where `*' indicates point."
  (let ((loop t))
    ;; Find a too long line.
    (while (and loop (< (point) (point-max)))
      ;; Go to the threshold column.
      (eat--t-goto-col threshold)
      ;; Are we at the end of line?
      (if (eq (char-after) ?\n)
          ;; We are already at the end of line, so move to the next
          ;; line and start from the beginning.
          (forward-char)
        ;; The next character is not a newline, so we must be at a
        ;; long line, or we are the end of the accessible part of the
        ;; buffer.  Whatever the case, we break the loop, and if it is
        ;; a long line, we break the line.
        (setq loop nil)
        (unless (= (point) (point-max))
          (insert-before-markers
           #("\n" 0 1 (eat--t-wrap-line t))))))))


;;;; Emulator.

(cl-defstruct (eat--t-cur
               (:constructor eat--t-make-cur)
               (:copier eat--t-copy-cur))
  "Structure describing cursor position."
  (position nil :documentation "Position of cursor.")
  (y 1 :documentation "Y coordinate of cursor.")
  (x 1 :documentation "X coordinate of cursor."))

(cl-defstruct (eat--t-disp
               (:constructor eat--t-make-disp)
               (:copier eat--t-copy-disp))
  "Structure describing the display."
  (begin nil :documentation "Beginning of visible display.")
  (width 80 :documentation "Width of display.")
  (height 24 :documentation "Height of display.")
  (cursor nil :documentation "Cursor.")
  (saved-cursor
   (1value (eat--t-make-cur))
   :documentation "Saved cursor.")
  (old-begin
   nil
   :documentation
   "Beginning of visible display during last Eat redisplay."))

(cl-defstruct (eat--t-face
               (:constructor eat--t-make-face)
               (:copier eat--t-copy-face))
  "Structure describing the display attributes to use."
  (face nil :documentation "Face to use.")
  (fg nil :documentation "Foreground color.")
  (bg nil :documentation "Background color.")
  (intensity nil :documentation "Intensity face, or nil.")
  (italic nil :documentation "Non-nil means use italic face.")
  (underline nil :documentation "Non-nil means underline text.")
  (underline-color nil :documentation "Underline color.")
  (crossed nil :documentation "Non-nil means strike-through text.")
  (conceal nil :documentation "Non-nil means invisible text.")
  (inverse nil :documentation "Non-nil means inverse colors.")
  (blink nil :documentation "Blink face, or nil.")
  (font 'eat-term-font-0 :documentation "Current font face."))

(cl-defstruct (eat--t-term
               (:constructor eat--t-make-term)
               (:copier eat--t-copy-term))
  "Structure describing a terminal."
  (buffer nil :documentation "The buffer of terminal.")
  (begin nil :documentation "Beginning of terminal.")
  (end nil :documentation "End of terminal area.")
  (title "" :documentation "The title of the terminal.")
  (cwd
   (cons (system-name) default-directory)
   :documentation "The working directory of the terminal.")
  (bell-fn
   (1value #'ignore)
   :documentation "Function to ring the bell.")
  (input-fn
   (1value #'ignore)
   :documentation "Function to send input.")
  (set-cursor-fn
   (1value #'ignore)
   :documentation "Function to set cursor.")
  (manipulate-selection-fn
   (1value #'ignore)
   :documentation "Function to manipulate selection.")
  (grab-mouse-fn
   (1value #'ignore)
   :documentation "Function to grab mouse.")
  (set-focus-ev-mode-fn
   (1value #'ignore)
   :documentation "Function to set focus event mode.")
  (set-title-fn
   (1value #'ignore)
   :documentation "Function to set the title.")
  (set-cwd-fn
   (1value #'ignore)
   :documentation "Function to set the current working directory.")
  (prompt-start-fn
   (1value #'ignore)
   :documentation "Function to call when prompt starts.")
  (prompt-end-fn
   (1value #'ignore)
   :documentation "Function to call when prompt ends.")
  (cont-prompt-start-fn
   (1value #'ignore)
   :documentation "Function to call when prompt starts.")
  (cont-prompt-end-fn
   (1value #'ignore)
   :documentation "Function to call when prompt ends.")
  (set-cmd-fn
   (1value #'ignore)
   :documentation "Function to set the command being executed.")
  (cmd-start-fn
   (1value #'ignore)
   :documentation "Function to call just before a command is run.")
  (cmd-finish-fn
   (1value #'ignore)
   :documentation "Function to call after a command has finished.")
  (parser-state nil :documentation "State of parser.")
  (scroll-begin 1 :documentation "First line of scroll region.")
  (scroll-end 24 :documentation "Last line of scroll region.")
  (display nil :documentation "The display.")
  (main-display nil :documentation "Main display.

Nil when not in alternative display mode.")
  (face
   (1value (eat--t-make-face))
   :documentation "Display attributes.")
  (auto-margin t :documentation "State of auto margin mode.")
  (ins-mode nil :documentation "State of insert mode.")
  (charset
   (copy-tree '(g0 . ((g0 . us-ascii)
                      (g1 . us-ascii)
                      (g2 . us-ascii)
                      (g3 . us-ascii))))
   :documentation "Current character set.")
  (cur-state :default :documentation "Current state of cursor.")
  (cur-blinking-p nil :documentation "Is the cursor blinking?")
  (saved-face
   (1value (eat--t-make-face))
   :documentation "Saved SGR attributes.")
  (bracketed-yank nil :documentation "State of bracketed yank mode.")
  (keypad-mode nil :documentation "State of keypad mode.")
  (mouse-mode nil :documentation "Current mouse mode.")
  (mouse-pressed nil :documentation "Pressed mouse buttons.")
  (mouse-encoding nil :documentation "Current mouse event encoding.")
  (focus-event-mode nil :documentation "Whether to send focus event.")
  (cut-buffers
   (1value (make-vector 10 nil))
   :documentation "Cut buffers.")
  (params nil :documentation "Alist of terminal parameters."))

(defvar eat--t-term nil
  "The current terminal.

Don't `set' it, bind it to a value with `let'.")

(defun eat--t-reset ()
  "Reset terminal."
  (let ((disp (eat--t-term-display eat--t-term)))
    ;; Reset most of the things to their respective default values.
    (setf (eat--t-term-parser-state eat--t-term) nil)
    (setf (eat--t-disp-begin disp) (point-min-marker))
    (setf (eat--t-disp-old-begin disp) (point-min-marker))
    (setf (eat--t-disp-cursor disp)
          (eat--t-make-cur :position (point-min-marker)))
    (setf (eat--t-disp-saved-cursor disp) (eat--t-make-cur))
    (setf (eat--t-term-scroll-begin eat--t-term) 1)
    (setf (eat--t-term-scroll-end eat--t-term)
          (eat--t-disp-height disp))
    (setf (eat--t-term-main-display eat--t-term) nil)
    (setf (eat--t-term-face eat--t-term) (eat--t-make-face))
    (setf (eat--t-term-auto-margin eat--t-term) t)
    (setf (eat--t-term-ins-mode eat--t-term) nil)
    (setf (eat--t-term-charset eat--t-term)
          '(g0 (g0 . us-ascii)
               (g1 . dec-line-drawing)
               (g2 . dec-line-drawing)
               (g3 . dec-line-drawing)))
    (setf (eat--t-term-saved-face eat--t-term) (eat--t-make-face))
    (setf (eat--t-term-bracketed-yank eat--t-term) nil)
    (setf (eat--t-term-cur-state eat--t-term) :default)
    (setf (eat--t-term-cur-blinking-p eat--t-term) nil)
    (setf (eat--t-term-title eat--t-term) "")
    (setf (eat--t-term-keypad-mode eat--t-term) nil)
    (setf (eat--t-term-mouse-mode eat--t-term) nil)
    (setf (eat--t-term-mouse-encoding eat--t-term) nil)
    (setf (eat--t-term-focus-event-mode eat--t-term) nil)
    ;; Clear everything.
    (delete-region (point-min) (point-max))
    ;; Inform the UI about our new state.
    (funcall (eat--t-term-grab-mouse-fn eat--t-term) eat--t-term nil)
    (funcall (eat--t-term-set-focus-ev-mode-fn eat--t-term)
             eat--t-term nil)
    (funcall (eat--t-term-set-title-fn eat--t-term) eat--t-term "")
    (funcall (eat--t-term-set-cursor-fn eat--t-term) eat--t-term
             :default)))

(defun eat--t-cur-right (&optional n)
  "Move cursor N columns right.

N default to 1.  If N is out of range, place cursor at the edge of
display."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; If N is less than 1, set N to 1.  If N is more than the number
    ;; of available columns on the right side, set N to the maximum
    ;; possible value.
    (setq n (min (- (eat--t-disp-width disp) (eat--t-cur-x cursor))
                 (max (or n 1) 1)))
    ;; N is non-zero in most cases, except at the edge of display.
    (unless (zerop n)
      ;; Move to the Nth next column, use spaces to reach that column
      ;; if needed.
      (eat--t-repeated-insert ?\s (- n (eat--t-col-motion n)))
      (cl-incf (eat--t-cur-x cursor) n))))

(defun eat--t-cur-left (&optional n)
  "Move cursor N columns left.

N default to 1.  If N is out of range, place cursor at the edge of
display."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; If N is less than 1, set N to 1.  If N is more than the number
    ;; of available columns on the left side, set N to the maximum
    ;; possible value.
    (setq n (min (1- (eat--t-cur-x cursor)) (max (or n 1) 1)))
    ;; N is non-zero in most cases, except at the edge of display.
    (unless (zerop n)
      ;; Move to the Nth previous column.
      (cl-assert (1value (>= (eat--t-current-col) n)))
      (backward-char n)
      (cl-decf (eat--t-cur-x cursor) n))))

(defun eat--t-cur-horizontal-abs (&optional n)
  "Move cursor to Nth column on current line.

N default to 1.  If N is out of range, place cursor at the edge of
display."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; If N is out of range, bring it within the bounds of range.
    (setq n (min (max (or n 1) 1) (eat--t-disp-width disp)))
    ;; Depending on the current position of cursor, move right or
    ;; left.
    (cond ((< (eat--t-cur-x cursor) n)
           (eat--t-cur-right (- n (eat--t-cur-x cursor))))
          ((< n (eat--t-cur-x cursor))
           (eat--t-cur-left (- (eat--t-cur-x cursor) n))))))

(defun eat--t-beg-of-next-line (n)
  "Move to beginning of Nth next line."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; If N is less than 1, set N to 1.  If N is more than the number
    ;; of available lines below, set N to the maximum possible value.
    (setq n (min (- (eat--t-disp-height disp) (eat--t-cur-y cursor))
                 (max (or n 1) 1)))
    ;; N is non-zero in most cases, except at the edge of display.
    ;; Whatever the case, we move to the beginning of line.
    (if (zerop n)
        (1value (eat--t-goto-bol))
      ;; Move to the Nth next line, use newlines to reach that line if
      ;; needed.
      (eat--t-repeated-insert ?\n (- n (eat--t-goto-bol n)))
      (cl-incf (eat--t-cur-y cursor) n))
    (1value (setf (eat--t-cur-x cursor) 1))))

(defun eat--t-beg-of-prev-line (n)
  "Move to beginning of Nth previous line."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; If N is less than 1, set N to 1.  If N is more than the number
    ;; of available lines above, set N to the maximum possible value.
    (setq n (min (1- (eat--t-cur-y cursor)) (max (or n 1) 1)))
    ;; Move to the beginning Nth previous line.  Even if there are no
    ;; line above, move to the beginning of the line.
    (eat--t-goto-bol (- n))
    (cl-decf (eat--t-cur-y cursor) n)
    (1value (setf (eat--t-cur-x cursor) 1))))

(defun eat--t-cur-down (&optional n)
  "Move cursor N lines down.

N default to 1.  If N is out of range, place cursor at the edge of
display."
  (let ((x (eat--t-cur-x (eat--t-disp-cursor
                          (eat--t-term-display eat--t-term)))))
    ;; Move to the beginning of target line.
    (eat--t-beg-of-next-line n)
    ;; If the cursor wasn't at column one, move the cursor to the
    ;; cursor to that column.
    (unless (= x 1)
      (eat--t-cur-right (1- x)))))

(defun eat--t-cur-up (&optional n)
  "Move cursor N lines up.

N default to 1.  If N is out of range, place cursor at the edge of
display."
  (let ((x (eat--t-cur-x (eat--t-disp-cursor
                          (eat--t-term-display eat--t-term)))))
    ;; Move to the beginning of target line.
    (eat--t-beg-of-prev-line n)
    ;; If the cursor wasn't at column one, move the cursor to the
    ;; cursor to that column.
    (unless (= x 1)
      (eat--t-cur-right (1- x)))))

(defun eat--t-cur-vertical-abs (&optional n)
  "Move cursor to Nth line on display.

N default to 1.  If N is out of range, place cursor at the edge of
display."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; If N is out of range, bring it within the bounds of range.
    (setq n (min (max (or n 1) 1) (eat--t-disp-height disp)))
    ;; Depending on the current position of cursor, move downward or
    ;; upward.
    (cond ((< (eat--t-cur-y cursor) n)
           (eat--t-cur-down (- n (eat--t-cur-y cursor))))
          ((< n (eat--t-cur-y cursor))
           (eat--t-cur-up (- (eat--t-cur-y cursor) n))))))

(defun eat--t-scroll-up (&optional n as-side-effect)
  "Scroll up N lines, preserving cursor position.

N default to 1.  By default, don't change current line and current
column, but if AS-SIDE-EFFECT is given and non-nil, assume that
scrolling is triggered as a side effect of some other control function
and don't move the point relative to the text and change current line
accordingly."
  (let ((disp (eat--t-term-display eat--t-term))
        (scroll-begin (eat--t-term-scroll-begin eat--t-term))
        (scroll-end (eat--t-term-scroll-end eat--t-term)))
    ;; N shouldn't be more more than the number of lines in scroll
    ;; region.
    (setq n (min (max (or n 1) 0) (1+ (- scroll-end scroll-begin))))
    ;; Make sure that N is positive.
    (unless (zerop n)
      ;; Try to not point relative to the text.
      (save-excursion
        (goto-char (eat--t-disp-begin disp))
        ;; Move to the beginning of scroll region.
        (eat--t-goto-bol (1- scroll-begin))
        ;; If the first line on display isn't in scroll region or
        ;; if this is the alternative display, delete text.
        (if (or (eat--t-term-main-display eat--t-term)
                (> scroll-begin 1))
            (delete-region (point) (car (eat--t-bol n)))
          ;; Otherwise, send the text to the scrollback area by
          ;; advancing the display beginning marker.
          (eat--t-goto-bol n)
          ;; Make sure we're at the beginning of a line, because we
          ;; might be at `point-max'.
          (unless (or (= (point) (point-min))
                      (= (char-before) ?\n))
            (insert ?\n))
          (set-marker (eat--t-disp-begin disp) (point)))
        ;; Is the last line on display in scroll region?
        (when (< scroll-begin (eat--t-disp-width disp))
          ;; Go to the end of scroll region (before deleting or moving
          ;; texts).
          (eat--t-goto-bol (- (1+ (- scroll-end scroll-begin)) n))
          ;; If there is anything after the scroll region, insert
          ;; newlines to keep that text unmoved.
          (when (< (point) (point-max))
            (eat--t-repeated-insert ?\n n))))
      ;; Recalculate point if needed.
      (let* ((cursor (eat--t-disp-cursor disp))
             (recalc-point
              (<= scroll-begin (eat--t-cur-y cursor) scroll-end)))
        ;; If recalc-point is non-nil, and AS-SIDE-EFFECT is non-nil,
        ;; update cursor position so that it is unmoved relative to
        ;; surrounding text and reconsider point recalculation.
        (when (and recalc-point as-side-effect)
          (setq recalc-point (< (- (eat--t-cur-y cursor) n)
                                scroll-begin))
          (setf (eat--t-cur-y cursor)
                (max (- (eat--t-cur-y cursor) n) scroll-begin)))
        (when recalc-point
          ;; Recalculate point.
          (let ((y (eat--t-cur-y cursor))
                (x (eat--t-cur-x cursor)))
            (eat--t-goto 1 1)
            (eat--t-goto y x)))))))

(defun eat--t-scroll-down (&optional n)
  "Scroll down N lines, preserving cursor position.

N defaults to 1."
  (let ((disp (eat--t-term-display eat--t-term))
        (scroll-begin (eat--t-term-scroll-begin eat--t-term))
        (scroll-end (eat--t-term-scroll-end eat--t-term)))
    ;; N shouldn't be more more than the number of lines in scroll
    ;; region.
    (setq n (min (max (or n 1) 0) (1+ (- scroll-end scroll-begin))))
    ;; Make sure that N is positive.
    (unless (zerop n)
      ;; Move to the beginning of scroll region.
      (goto-char (eat--t-disp-begin disp))
      (eat--t-goto-bol (1- scroll-begin))
      ;; Insert newlines to push text downwards.
      (eat--t-repeated-insert ?\n n)
      ;; Go to the end scroll region (after inserting newlines).
      (eat--t-goto-eol (- (1+ (- scroll-end scroll-begin)) (1+ n)))
      ;; Delete the text that was pushed out of scroll region.
      (when (< (point) (point-max))
        (delete-region (point) (car (eat--t-eol n))))
      ;; The cursor mustn't move, so we have to recalculate point.
      (let* ((cursor (eat--t-disp-cursor disp))
             (y (eat--t-cur-y cursor))
             (x (eat--t-cur-x cursor)))
        (eat--t-goto 1 1)
        (eat--t-goto y x)))))

(defun eat--t-goto (&optional y x)
  "Go to Xth column of Yth line of display.

Y and X default to 1.  Y and X are one-based.  If Y and/or X are out
of range, place cursor at the edge of display."
  ;; Important special case: if Y and X are both one, move to the
  ;; display beginning.
  (if (and (or (not y) (eql y 1))
           (or (not x) (eql x 1)))
      (let* ((disp (eat--t-term-display eat--t-term))
             (cursor (eat--t-disp-cursor disp)))
        (goto-char (eat--t-disp-begin disp))
        (1value (setf (eat--t-cur-y cursor) 1
                      (eat--t-cur-x cursor) 1)))
    ;; Move to column one, go to Yth line and move to Xth column.
    ;; REVIEW: We move relative to cursor position, which faster for
    ;; positions near the point (usually the case), but slower for
    ;; positions far away from the point.  There are only two cursor
    ;; positions whose exact position is known beforehand, the cursor
    ;; (whose position is (point)) and (1, 1) (the display beginning).
    ;; There are almost always some points which are at more distance
    ;; from current position than from the display beginning (the only
    ;; exception is when the cursor is at the display beginning).  So
    ;; first moving to the display beginning and then moving to those
    ;; point will be faster than moving from cursor (except a tiny
    ;; (perhaps negligible) overhead of `goto-char').  What we don't
    ;; have is a formula the calculate the distance between two
    ;; positions.
    (eat--t-cur-horizontal-abs 1)
    (eat--t-cur-vertical-abs y)
    (eat--t-cur-horizontal-abs x)))

(defun eat--t-enable-auto-margin ()
  "Enable automatic margin."
  ;; Set the automatic margin flag to t, the rest of code will take
  ;; care of the effects.
  (1value (setf (eat--t-term-auto-margin eat--t-term) t)))

(defun eat--t-disable-auto-margin ()
  "Disable automatic margin."
  ;; Set the automatic margin flag to nil, the rest of code will take
  ;; care of the effects.
  (1value (setf (eat--t-term-auto-margin eat--t-term) nil)))

(defun eat--t-set-charset (slot charset)
  "SLOT's character set to CHARSET."
  (setf (alist-get slot (cdr (eat--t-term-charset eat--t-term)))
        charset))

(defun eat--t-change-charset (charset)
  "Change character set to CHARSET.

CHARSET should be one of `g0', `g1', `g2' and `g3'."
  (cl-assert (memq charset '(g0 g1 g2 g3)))
  (setf (car (eat--t-term-charset eat--t-term)) charset))

(defun eat--t-move-before-to-safe ()
  "Move to a safe position before point.  Return how much moved.

If the current position is safe, do nothing and return 0.

Safe position is the position that's not on a multi-column wide
character or its the internal invisible spaces."
  (if (and (not (bobp))
           ;; Is the current position unsafe?
           (get-text-property (1- (point)) 'eat--t-invisible-space))
      (let ((start-pos (point)))
        ;; Move to the safe position.
        (goto-char (previous-single-char-property-change
                    (point) 'eat--t-invisible-space))
        (cl-assert
         (1value (or (bobp)
                     (null (get-text-property
                            (1- (point)) 'eat--t-invisible-space)))))
        (- start-pos (point)))
    0))

(defun eat--t-make-pos-safe ()
  "If the position isn't safe, make it safe by replacing with spaces."
  (let ((moved (eat--t-move-before-to-safe)))
    (unless (zerop moved)
      (let ((width (get-text-property
                    (point) 'eat--t-char-width)))
        (cl-assert width)
        (delete-region (point) (+ (point) width))
        (eat--t-repeated-insert
         ?\s width (eat--t-face-face
                    (eat--t-term-face eat--t-term)))
        (backward-char (- width moved))))))

(defun eat--t-fix-partial-multi-col-char ()
  "Replace any partial multi-column character with spaces."
  (let ((face (eat--t-face-face
               (eat--t-term-face eat--t-term))))
    (if (get-text-property (point) 'eat--t-invisible-space)
        (let ((start-pos (point))
              (count nil))
          (goto-char (next-single-char-property-change
                      (point) 'eat--t-invisible-space))
          (setq count (- (1+ (point)) start-pos))
          ;; Make sure we really overwrote the character
          ;; partially.
          (when (< count (get-text-property
                          (point) 'eat--t-char-width))
            (delete-region start-pos (1+ (point)))
            (eat--t-repeated-insert ?\s count face))
          (goto-char start-pos))
      ;; Detect the case where we have deleted all the invisible
      ;; spaces before, but not the multi-column character itself.
      (when-let* (((not (eobp)))
                  (w (get-text-property (point) 'eat--t-char-width))
                  ((> w 1)))
        ;; `delete-char' also works, but it does more checks, so
        ;; hopefully this will be faster.
        (delete-region (point) (1+ (point)))
        (insert (propertize " " 'face face))
        (backward-char)))))

(defconst eat--t-dec-line-drawing-chars
  (eval-and-compile
    (let ((alist '((?+ . ?→)
                   (?, . ?←)
                   (?- . ?↑)
                   (?. . ?↓)
                   (?0 . ?█)
                   (?\` . ?�)
                   (?a . ?▒)
                   (?b . ?␉)
                   (?c . ?␌)
                   (?d . ?␍)
                   (?e . ?␊)
                   (?f . ?°)
                   (?g . ?±)
                   (?h . ?░)
                   (?i . ?#)
                   (?j . ?┘)
                   (?k . ?┐)
                   (?l . ?┌)
                   (?m . ?└)
                   (?n . ?┼)
                   (?o . ?⎺)
                   (?p . ?⎻)
                   (?q . ?─)
                   (?r . ?⎼)
                   (?s . ?⎽)
                   (?t . ?├)
                   (?u . ?┤)
                   (?v . ?┴)
                   (?w . ?┬)
                   (?x . ?│)
                   (?y . ?≤)
                   (?z . ?≥)
                   (?{ . ?π)
                   (?| . ?≠)
                   (?} . ?£)
                   (?~ . ?•)))
          (table (make-hash-table :purecopy t)))
      (dolist (pair alist)
        (puthash (car pair) (cdr pair) table))
      table))
  "Hash table for DEC Line Drawing charset.

The key is the output character from client, and value of the
character to actually show.")

(defun eat--t-write (str &optional beg end)
  "Write STR from BEG to END on display."
  (setq beg (or beg 0))
  (setq end (or end (length str)))
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (scroll-end (eat--t-term-scroll-end eat--t-term))
         (charset
          (alist-get (car (eat--t-term-charset eat--t-term))
                     (cdr (eat--t-term-charset eat--t-term))))
         (face (eat--t-face-face (eat--t-term-face eat--t-term)))
         ;; Alist of indices and width of multi-column characters.
         (multi-col-char-indices nil)
         (inserted-till beg))
    (cl-assert charset)
    ;; Find all the multi-column wide characters in ST; hopefully it
    ;; won't slow down showing plain ASCII.
    (setq multi-col-char-indices
          (cl-loop for i from beg to (1- end)
                   when (/= (char-width (aref str i)) 1)
                   collect (cons i (char-width (aref str i)))))
    ;; If the position isn't safe, replace the multi-column
    ;; character with spaces to make it safe.
    (eat--t-make-pos-safe)
    ;; TODO: Comment.
    ;; REVIEW: This probably needs to be updated.
    (while (< inserted-till end)
      ;; Insert STR, and record the width of STR inserted
      ;; successfully.
      (let ((ins-count
             (named-let write
                 ((max (min (- (eat--t-disp-width disp)
                               (1- (eat--t-cur-x cursor)))
                            (+ (- end inserted-till)
                               (cl-loop
                                for p in multi-col-char-indices
                                sum (1- (cdr p))))))
                  (written 0))
               (let* ((next-multi-col (car multi-col-char-indices))
                      (end (+ inserted-till max))
                      (e (if next-multi-col
                             ;; Exclude the multi-column character.
                             (min (car next-multi-col) end)
                           end))
                      (wrote (- e inserted-till)))
                 (cl-assert (>= wrote 0))
                 (let ((s (substring str inserted-till e)))
                   ;; Convert STR to Unicode according to the
                   ;; current character set.
                   (pcase-exhaustive charset
                     ;; For `us-ascii', the default, no conversion
                     ;; is necessary.
                     ('us-ascii)
                     ;; `dec-line-drawing' contains various
                     ;; characters useful for drawing line diagram,
                     ;; so it is a must.  This is also possible
                     ;; with `us-ascii', thanks to Unicode, but the
                     ;; character set `dec-line-drawing' is usually
                     ;; less expensive in terms of bytes needed to
                     ;; transfer than `us-ascii'.
                     ('dec-line-drawing
                      (dotimes (i (length s))
                        (when-let*
                            ((r (gethash
                                 (aref s i)
                                 eat--t-dec-line-drawing-chars)))
                          (aset s i r)))))
                   ;; Add face.
                   (put-text-property 0 (length s) 'face face s)
                   (insert s))
                 (setq inserted-till e)
                 (if (or (null next-multi-col)
                         (< (- end e) (cdr next-multi-col)))
                     ;; Either everything is done, or we reached
                     ;; the limit.
                     (+ written wrote)
                   ;; There are many characters which are too
                   ;; narrow for `char-width' to return 1.  XTerm,
                   ;; Kitty and St seems to ignore them, so we too.
                   (if (zerop (cdr next-multi-col))
                       (cl-incf inserted-till)
                     (insert
                      ;; Make sure the multi-column character
                      ;; occupies the same number of characters as
                      ;; its width.
                      (propertize
                       (make-string (1- (cdr next-multi-col)) ?\s)
                       'invisible t 'face face
                       'eat--t-invisible-space t
                       'eat--t-char-width (cdr next-multi-col))
                      ;; Now insert the multi-column character.
                      (propertize
                       (substring str inserted-till
                                  (cl-incf inserted-till))
                       'face face
                       'eat--t-char-width (cdr next-multi-col))))
                   (setf multi-col-char-indices
                         (cdr multi-col-char-indices))
                   (write (- max wrote (cdr next-multi-col))
                          (+ written wrote
                             (cdr next-multi-col))))))))
        (cl-incf (eat--t-cur-x cursor) ins-count)
        (if (eat--t-term-ins-mode eat--t-term)
            (delete-region
             (save-excursion
               (eat--t-col-motion (- (eat--t-disp-width disp)
                                     (1- (eat--t-cur-x cursor))))
               ;; Make sure the point is safe.
               (eat--t-move-before-to-safe)
               (point))
             (car (eat--t-eol)))
          (delete-region (point) (min (+ ins-count (point))
                                      (car (eat--t-eol))))
          ;; Replace any partially-overwritten character with
          ;; spaces.
          (eat--t-fix-partial-multi-col-char))
        (when (> (eat--t-cur-x cursor) (eat--t-disp-width disp))
          (if (not (eat--t-term-auto-margin eat--t-term))
              (eat--t-cur-left 1)
            (when (< inserted-till end)
              (when (= (eat--t-cur-y cursor) scroll-end)
                (eat--t-scroll-up 1 'as-side-effect))
              (if (= (eat--t-cur-y cursor) scroll-end)
                  (eat--t-carriage-return)
                (if (= (point) (point-max))
                    (insert #("\n" 0 1 (eat--t-wrap-line t)))
                  (put-text-property (point) (1+ (point))
                                     'eat--t-wrap-line t)
                  (forward-char))
                (1value (setf (eat--t-cur-x cursor) 1))
                (cl-incf (eat--t-cur-y cursor))))))))))

(defun eat--t-horizontal-tab (&optional n)
  "Go to the Nth next tabulation stop.

N default to 1."
  ;; N must be positive.
  (setq n (max (or n 1) 1))
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; Do some math calculate the distance of the Nth next tabulation
    ;; stop from cursor, and go there.
    (eat--t-cur-right (+ (- 8 (mod (1- (eat--t-cur-x cursor)) 8))
                         (* (1- n) 8)))))

(defun eat--t-horizontal-backtab (&optional n)
  "Go to the Nth previous tabulation stop.

N default to 1."
  ;; N must be positive.
  (setq n (max (or n 1) 1))
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; Do some math calculate the distance of the Nth next tabulation
    ;; stop from cursor, and go there.
    (eat--t-cur-left (+ (1+ (mod (- (eat--t-cur-x cursor) 2) 8))
                        (* (1- n) 8)))))

(defun eat--t-index ()
  "Go to the next line preserving column, scrolling if necessary."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (scroll-end (eat--t-term-scroll-end eat--t-term))
         ;; Are we inside scroll region?
         (in-scroll-region (<= (eat--t-cur-y cursor) scroll-end)))
    ;; If this is the last line (of the scroll region or the display),
    ;; scroll up, otherwise move cursor downward.
    (if (= (if in-scroll-region scroll-end (eat--t-disp-height disp))
           (eat--t-cur-y cursor))
        (eat--t-scroll-up 1)
      (eat--t-cur-down 1))))

(defun eat--t-carriage-return ()
  "Go to column one."
  (eat--t-cur-horizontal-abs 1))

(defun eat--t-line-feed ()
  "Go to the first column of the next line, scrolling if necessary."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (scroll-end (eat--t-term-scroll-end eat--t-term))
         ;; Are we inside scroll region?
         (in-scroll-region (<= (eat--t-cur-y cursor) scroll-end)))
    ;; If we are at the very end of the terminal, we might have some
    ;; optimizations.
    (if (= (point) (point-max))
        ;; If the cursor is above the last line of the scroll region
        ;; (or the display, if we are outside the scroll region), we
        ;; can simply insert a newline and update the cursor position.
        (if (/= (if in-scroll-region
                    scroll-end
                  (eat--t-disp-height disp))
                (eat--t-cur-y cursor))
            (progn
              (insert ?\n)
              (setf (eat--t-cur-x cursor) 1)
              (cl-incf (eat--t-cur-y cursor)))
          ;; This is the last line.  We need to scroll up.
          (eat--t-scroll-up 1 'as-side-effect)
          ;; If we're still at the last line (only happens when the
          ;; display has only a single line), go to column one of it.
          (if (= (if in-scroll-region
                     scroll-end
                   (eat--t-disp-height disp))
                 (eat--t-cur-y cursor))
              (eat--t-carriage-return)
            ;; If we are somehow moved from the end of terminal,
            ;; `eat--t-beg-of-next-line' is the best option.
            (if (/= (point) (point-max))
                (eat--t-beg-of-next-line 1)
              ;; We are still at the end!  We can can simply insert a
              ;; newline and update the cursor position.
              (insert ?\n)
              (setf (eat--t-cur-x cursor) 1)
              (cl-incf (eat--t-cur-y cursor)))))
      ;; We are not at the end of terminal.  But we still have a last
      ;; chance.  `eat--t-beg-of-next-line' is usually faster than
      ;; `eat--t-carriage-return' followed by `eat--t-index', so if
      ;; there is at least a single line (in the scroll region, if the
      ;; cursor in the scroll region, otherwise in the display)
      ;; underneath the cursor, we can use `eat--t-beg-of-next-line'.
      (if (/= (if in-scroll-region
                  scroll-end
                (eat--t-disp-height disp))
              (eat--t-cur-y cursor))
          (eat--t-beg-of-next-line 1)
        ;; We don't have any other option, so we must use the most
        ;; time-expensive option.
        (eat--t-carriage-return)
        (eat--t-index)))))

(defun eat--t-reverse-index ()
  "Go to the previous line preserving column, scrolling if needed."
  (let* ((cursor (eat--t-disp-cursor
                  (eat--t-term-display eat--t-term)))
         (scroll-begin (eat--t-term-scroll-begin eat--t-term))
         ;; Are we in the scroll region?
         (in-scroll-region (<= scroll-begin (eat--t-cur-y cursor))))
    ;; If this is the first line (of the scroll region or the
    ;; display), scroll down, otherwise move cursor upward.
    (if (= (if in-scroll-region scroll-begin 1)
           (eat--t-cur-y cursor))
        (eat--t-scroll-down 1)
      (eat--t-cur-up 1))))

(defun eat--t-bell ()
  "Ring the bell."
  ;; Call the UI's bell handler.
  (funcall (eat--t-term-bell-fn eat--t-term) eat--t-term))

(defun eat--t-form-feed ()
  "Insert a vertical tab."
  ;; Form feed is same as `eat--t-index'.
  (eat--t-index))

(defun eat--t-save-cur ()
  "Save current cursor position."
  (let ((disp (eat--t-term-display eat--t-term))
        (saved-face (eat--t-copy-face
                     (eat--t-term-face eat--t-term))))
    ;; Save cursor position.
    (setf (eat--t-disp-saved-cursor disp)
          (eat--t-copy-cur (eat--t-disp-cursor disp)))
    ;; Save SGR attributes.
    (setf (eat--t-term-saved-face eat--t-term) saved-face)
    ;; We use side-effects, so make sure the saved face doesn't share
    ;; structure with the current face.
    (setf (eat--t-face-face saved-face)
          (copy-tree (eat--t-face-face saved-face)))
    (setf (eat--t-face-underline-color saved-face)
          (copy-tree (eat--t-face-underline-color saved-face)))))

(defun eat--t-restore-cur ()
  "Restore previously save cursor position."
  (let ((saved (eat--t-disp-saved-cursor
                (eat--t-term-display eat--t-term))))
    ;; Restore cursor position.
    (eat--t-goto (eat--t-cur-y saved) (eat--t-cur-x saved))
    ;; Restore SGR attributes.
    (setf (eat--t-term-face eat--t-term)
          (copy-tree (eat--t-term-saved-face eat--t-term)))
    (setf (eat--t-face-underline-color (eat--t-term-face eat--t-term))
          (copy-tree (eat--t-face-underline-color
                      (eat--t-term-face eat--t-term))))))

(defun eat--t-erase-in-line (&optional n)
  "Erase part of current line, but don't move cursor.

N defaults to 0.  When N is 0, erase cursor to end of line.  When N is
1, erase beginning of line to cursor.  When N is 2, erase whole line."
  (let ((face (eat--t-term-face eat--t-term)))
    (pcase-exhaustive n
      ((or 0 'nil (pred (< 2)))
       ;; Delete cursor position (inclusive) to end of line.
       (delete-region (point) (car (eat--t-eol)))
       ;; If the SGR background attribute is set, we need to fill the
       ;; erased area with that background.
       (when (eat--t-face-bg face)
         (save-excursion
           (let* ((disp (eat--t-term-display eat--t-term))
                  (cursor (eat--t-disp-cursor disp)))
             (eat--t-repeated-insert
              ?\s (1+ (- (eat--t-disp-width disp)
                         (eat--t-cur-x cursor)))
              (and (eat--t-face-bg face)
                   (eat--t-face-face face)))))))
      (1
       ;; Delete beginning of line to cursor position (inclusive).
       (delete-region (car (eat--t-bol))
                      (if (or (= (point) (point-max))
                              (= (char-after) ?\n))
                          (point)
                        (1+ (point))))
       ;; Fill the region with spaces, use SGR background attribute
       ;; if set.
       (let ((cursor (eat--t-disp-cursor
                      (eat--t-term-display eat--t-term))))
         (eat--t-repeated-insert ?\s (eat--t-cur-x cursor)
                                 (and (eat--t-face-bg face)
                                      (eat--t-face-face face))))
       ;; We erased the character at the cursor position, so after
       ;; fill with spaces we are still off by one column; so move a
       ;; column backward.
       (backward-char))
      (2
       ;; Delete whole line.
       (delete-region (car (eat--t-bol)) (car (eat--t-eol)))
       (let* ((disp (eat--t-term-display eat--t-term))
              (cursor (eat--t-disp-cursor disp)))
         ;; Fill the region before cursor position with spaces, use
         ;; SGR background attribute if set.
         (eat--t-repeated-insert ?\s (1- (eat--t-cur-x cursor))
                                 (and (eat--t-face-bg face)
                                      (eat--t-face-face face)))
         ;; If the SGR background attribute is set, we need to fill
         ;; the erased area including and after cursor position with
         ;; that background.
         (when (eat--t-face-bg face)
           (save-excursion
             (eat--t-repeated-insert
              ?\s (1+ (- (eat--t-disp-width disp)
                         (eat--t-cur-x cursor)))
              (and (eat--t-face-bg face)
                   (eat--t-face-face face))))))))))

(defun eat--t-erase-in-disp (&optional n)
  "Erase part of display.

N defaults to 0.  When N is 0, erase cursor to end of display.  When N
is 1, erase beginning of display to cursor.  In both on the previous
cases, don't move cursor.  When N is 2, erase display and reset cursor
to (1, 1).  When N is 3, also erase the scrollback."
  (let ((face (eat--t-term-face eat--t-term)))
    (pcase-exhaustive n
      ((or 0 'nil (pred (< 3)))
       ;; Delete from cursor position (inclusive) to end of terminal.
       (delete-region (point) (point-max))
       ;; If the SGR background attribute is set, we need to fill the
       ;; erased area with that background.
       (when (eat--t-face-bg face)
         ;; `save-excursion' probably uses marker to save point, which
         ;; doesn't work in this case.  So we the store the point as a
         ;; integer.
         (let* ((pos (point))
                (disp (eat--t-term-display eat--t-term))
                (cursor (eat--t-disp-cursor disp)))
           ;; Fill current line.
           (eat--t-repeated-insert ?\s (1+ (- (eat--t-disp-width disp)
                                              (eat--t-cur-x cursor)))
                                   (eat--t-face-face face))
           ;; Fill the following lines.
           (dotimes (_ (- (eat--t-disp-height disp)
                          (eat--t-cur-y cursor)))
             (insert ?\n)
             (eat--t-repeated-insert ?\s (eat--t-disp-width disp)
                                     (eat--t-face-face face)))
           ;; Restore position.
           (goto-char pos))))
      (1
       (let* ((disp (eat--t-term-display eat--t-term))
              (cursor (eat--t-disp-cursor disp))
              (y (eat--t-cur-y cursor))
              (x (eat--t-cur-x cursor))
              ;; Should we erase including the cursor position?
              (incl-point (/= (point) (point-max))))
         ;; Delete the region to be erased.
         (delete-region (eat--t-disp-begin disp)
                        (if incl-point (1+ (point)) (point)))
         ;; If the SGR background attribute isn't set, insert
         ;; newlines, otherwise fill the erased area above the current
         ;; line with background color.
         (if (not (eat--t-face-bg face))
             (eat--t-repeated-insert ?\n (1- y))
           (dotimes (_ (1- y))
             (eat--t-repeated-insert ?\s (eat--t-disp-width disp)
                                     (eat--t-face-face face))
             (insert ?\n)))
         ;; Fill the current line to keep the cursor unmoved.  Use
         ;; background if the corresponding SGR attribute is set.
         (eat--t-repeated-insert ?\s x (and (eat--t-face-bg face)
                                            (eat--t-face-face face)))
         ;; We are off by one column; so move a column backward.
         (when incl-point
           (backward-char))))
      ((or 2 3)
       ;; Move to the display beginning.
       (eat--t-goto 1 1)
       ;; Delete everything in the display, and if N is 3, also delete
       ;; everything in the scrollback area.
       (delete-region (if (= n 2) (point) (point-min))
                      (point-max))
       ;; If the SGR background attribute is set, fill the display
       ;; with that background.
       (when (eat--t-face-bg face)
         ;; `save-excursion' probably uses marker to save point, which
         ;; doesn't work in this case.  So we the store the point as a
         ;; integer.
         (let ((pos (point))
               (disp (eat--t-term-display eat--t-term)))
           (dotimes (i (eat--t-disp-height disp))
             (unless (zerop i)
               (insert ?\n))
             (eat--t-repeated-insert ?\s (eat--t-disp-width disp)
                                     (eat--t-face-face face)))
           ;; Restore point.
           (goto-char pos)))))))

(defun eat--t-device-status-report ()
  "Send the current Y and X coordinate to client."
  ;; TODO: Is this really device status report function?
  (let ((cursor (eat--t-disp-cursor
                 (eat--t-term-display eat--t-term))))
    (funcall (eat--t-term-input-fn eat--t-term) eat--t-term
             (format "\e[%i;%iR" (eat--t-cur-y cursor)
                     (eat--t-cur-x cursor)))))

(defun eat--t-set-cursor-state (state)
  "Set cursor state to STATE.

STATE one of the `:default', `:invisible', `:very-visible'."
  (unless (eq (eat--t-term-cur-state eat--t-term) state)
    ;; Update state.
    (setf (eat--t-term-cur-state eat--t-term) state)
    ;; Inform the UI.
    (funcall (eat--t-term-set-cursor-fn eat--t-term) eat--t-term
             state)))

(defun eat--t-default-cursor ()
  "Set the cursor to its default state."
  (eat--t-set-cursor-state
   (if (eat--t-term-cur-blinking-p eat--t-term)
       :very-visible
     :default)))

(defun eat--t-invisible-cursor ()
  "Make the cursor invisible."
  (eat--t-set-cursor-state :invisible))

(defun eat--t-blinking-cursor ()
  "Make the cursor blink."
  (setf (eat--t-term-cur-blinking-p eat--t-term) t)
  (when (eq (eat--t-term-cur-state eat--t-term) :default)
    (eat--t-set-cursor-state :very-visible)))

(defun eat--t-non-blinking-cursor ()
  "Make the cursor not blink."
  (setf (eat--t-term-cur-blinking-p eat--t-term) nil)
  (when (eq (eat--t-term-cur-state eat--t-term) :very-visible)
    (eat--t-set-cursor-state :default)))

(defun eat--t-enable-bracketed-yank ()
  "Enable bracketed yank mode."
  (setf (eat--t-term-bracketed-yank eat--t-term) t))

(defun eat--t-disable-bracketed-yank ()
  "Disable bracketed yank mode."
  (setf (eat--t-term-bracketed-yank eat--t-term) nil))

(defun eat--t-enable-alt-disp ()
  "Enable alternative display."
  ;; Effective only when alternative display is enabled by user.
  (when eat-enable-alternative-display
    ;; Make sure we not already in the alternative display.
    (unless (eat--t-term-main-display eat--t-term)
      ;; Store the current display, including scrollback.
      (let ((main-disp (eat--t-copy-disp
                        (eat--t-term-display eat--t-term))))
        (setf (eat--t-disp-begin main-disp)
              (- (eat--t-disp-begin main-disp) (point-min)))
        (setf (eat--t-disp-old-begin main-disp)
              (- (eat--t-disp-old-begin main-disp) (point-min)))
        (setf (eat--t-disp-cursor main-disp)
              (eat--t-copy-cur (eat--t-disp-cursor main-disp)))
        (setf (eat--t-disp-saved-cursor main-disp)
              (eat--t-copy-cur (eat--t-disp-saved-cursor main-disp)))
        (setf (eat--t-cur-position (eat--t-disp-cursor main-disp))
              (- (point) (point-min)))
        (setf (eat--t-term-main-display eat--t-term)
              (cons main-disp (buffer-string)))
        ;; Delete everything, and move to the beginning of terminal.
        (delete-region (point-min) (point-max))
        (eat--t-goto 1 1)))))

(defun eat--t-disable-alt-disp (&optional dont-move-cursor)
  "Disable alternative display.

If DONT-MOVE-CURSOR is non-nil, don't move cursor from current
position."
  ;; Make sure we in the alternative display.
  (when (eat--t-term-main-display eat--t-term)
    (let ((main-disp (eat--t-term-main-display eat--t-term))
          (old-y (eat--t-cur-y
                  (eat--t-disp-cursor
                   (eat--t-term-display eat--t-term))))
          (old-x (eat--t-cur-x
                  (eat--t-disp-cursor
                   (eat--t-term-display eat--t-term))))
          (width (eat--t-disp-width
                  (eat--t-term-display eat--t-term)))
          (height (eat--t-disp-height
                   (eat--t-term-display eat--t-term))))
      ;; Delete everything.
      (delete-region (point-min) (point-max))
      ;; Restore the main display.
      (insert (cdr main-disp))
      (setf (eat--t-cur-position (eat--t-disp-cursor (car main-disp)))
            (copy-marker (+ (point-min)
                            (eat--t-cur-position
                             (eat--t-disp-cursor (car main-disp))))))
      (setf (eat--t-disp-old-begin (car main-disp))
            (copy-marker (+ (point-min)
                            (eat--t-disp-old-begin (car main-disp)))))
      (setf (eat--t-disp-begin (car main-disp))
            (copy-marker (+ (point-min)
                            (eat--t-disp-begin (car main-disp)))))
      (setf (eat--t-term-display eat--t-term) (car main-disp)
            (eat--t-term-main-display eat--t-term) nil)
      (goto-char (eat--t-cur-position
                  (eat--t-disp-cursor
                   (eat--t-term-display eat--t-term))))
      ;; Maybe the terminal was resized after enabling alternative
      ;; display, so we have to resize again.
      (eat--t-resize width height)
      ;; Restore cursor position if DONT-MOVE-CURSOR is non-nil.
      (when dont-move-cursor
        (eat--t-goto old-y old-x)))))

(defun eat--t-insert-char (n)
  "Insert N empty (space) characters, preserving cursor."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; Make sure N is positive.  If N is more than the number of
    ;; available columns available, set N to the maximum possible
    ;; value.
    (setq n (min (- (eat--t-disp-width disp)
                    (1- (eat--t-cur-x cursor)))
                 (max (or n 1) 1)))
    ;; Return if N is zero.
    (unless (zerop n)
      ;; If the position isn't safe, replace the multi-column
      ;; character with spaces to make it safe.
      (eat--t-make-pos-safe)
      (save-excursion
        (let ((face (eat--t-term-face eat--t-term)))
          ;; Insert N spaces, with SGR background if that attribute is
          ;; set.
          (eat--t-repeated-insert
           ?\s n (and (eat--t-face-bg face) (eat--t-face-face face))))
        ;; Remove the characters that went beyond the edge of
        ;; display.
        (eat--t-col-motion (- (eat--t-disp-width disp)
                              (+ (1- (eat--t-cur-x cursor)) n)))
        ;; Make sure we delete any multi-column character
        ;; completely.
        (eat--t-move-before-to-safe)
        (delete-region (point) (car (eat--t-eol)))))))

(defun eat--t-delete-char (n)
  "Delete N characters, preserving cursor."
  (let* ((disp (eat--t-term-display eat--t-term))
         (face (eat--t-term-face eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; Make sure N is positive.  If N is more than the number of
    ;; available columns available, set N to the maximum possible
    ;; value.
    (setq n (min (- (eat--t-disp-width disp)
                    (1- (eat--t-cur-x cursor)))
                 (max (or n 1) 1)))
    ;; Return if N is zero.
    (unless (zerop n)
      ;; If the position isn't safe, replace the multi-column
      ;; character with spaces to make it safe.
      (eat--t-make-pos-safe)
      (save-excursion
        (let ((m (point)))
          ;; Delete N character on current line.
          (eat--t-col-motion n)
          (delete-region m (point))
          ;; Replace any partially-overwritten character with spaces.
          (eat--t-fix-partial-multi-col-char)
          ;; If SGR background attribute is set, fill N characters at
          ;; the right edge of display with that background.
          (when (eat--t-face-bg face)
            (save-excursion
              (eat--t-goto-eol)
              (let ((empty (1+ (- (eat--t-disp-width disp)
                                  (eat--t-cur-x cursor)
                                  (- (point) m)))))
                ;; Reach the position from where to start filling.
                ;; Use spaces if needed.
                (when (> empty n)
                  (eat--t-repeated-insert ?\s (- empty n)))
                ;; Fill with background.
                (eat--t-repeated-insert
                 ?\s (min empty n) (eat--t-face-face face))))))))))

(defun eat--t-erase-char (n)
  "Make next N character cells empty, preserving cursor."
  (let* ((disp (eat--t-term-display eat--t-term))
         (face (eat--t-term-face eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    ;; Make sure N is positive.  If N is more than the number of
    ;; available columns available, set N to the maximum possible
    ;; value.
    (setq n (min (- (eat--t-disp-width disp)
                    (1- (eat--t-cur-x cursor)))
                 (max (or n 1) 1)))
    ;; Return if N is zero.
    (unless (zerop n)
      ;; If the position isn't safe, replace the multi-column
      ;; character with spaces to make it safe.
      (eat--t-make-pos-safe)
      (save-excursion
        (let ((m (point)))
          ;; Delete N character on current line.
          (eat--t-col-motion n)
          (delete-region m (point))
          ;; Replace any partially-overwritten character with spaces.
          (eat--t-fix-partial-multi-col-char)
          ;; Insert N spaces, with background if SGR background
          ;; attribute is set.
          (eat--t-repeated-insert
           ?\s n (and (eat--t-face-bg face)
                      (eat--t-face-face face))))))))

(defun eat--t-insert-line (n)
  "Insert N empty lines, preserving cursor."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (scroll-begin (eat--t-term-scroll-begin eat--t-term))
         (scroll-end (eat--t-term-scroll-end eat--t-term)))
    ;; N should be positive and shouldn't exceed the number of lines
    ;; below cursor position and inside current scroll region.
    (setq n (min (- (1+ (- scroll-end scroll-begin))
                    (1- (eat--t-cur-y cursor)))
                 (max (or n 1) 1)))
    ;; Make sure we are in the scroll region and N is positive, return
    ;; on failure.
    (when (and (<= scroll-begin (eat--t-cur-y cursor) scroll-end)
               (not (zerop n)))
      ;; This function doesn't move the cursor, but pushes all the
      ;; line below and including current line.  So to keep the cursor
      ;; unmoved, go to the beginning of line and insert enough spaces
      ;; to not move the cursor.
      (eat--t-goto-bol)
      (let ((face (eat--t-term-face eat--t-term)))
        (eat--t-repeated-insert ?\s (1- (eat--t-cur-x cursor))
                                (and (eat--t-face-bg face)
                                     (eat--t-face-face face)))
        (goto-char
         (prog1 (point)
           ;; Insert N lines.
           (if (not (eat--t-face-bg face))
               (eat--t-repeated-insert ?\n n)
             ;; SGR background attribute set, so fill the inserted
             ;; lines with background.
             (dotimes (i n)
               ;; Fill a line.
               (eat--t-repeated-insert
                ?\s (if (not (zerop i))
                        (eat--t-disp-width disp)
                      ;; The first inserted line is already filled
                      ;; partially, so calculate the number columns
                      ;; left to fill.
                      (1+ (- (eat--t-disp-width disp)
                             (eat--t-cur-x cursor))))
                (eat--t-face-face face))
               ;; New line.
               (insert ?\n)))
           ;; Delete the lines that were just pushed beyond the end of
           ;; scroll region.
           (eat--t-goto-eol (- (1+ (- scroll-end scroll-begin))
                               (+ (- (eat--t-cur-y cursor)
                                     (1- scroll-begin))
                                  n)))
           (delete-region (point) (car (eat--t-eol n)))))))))

(defun eat--t-delete-line (n)
  "Delete N lines, preserving cursor."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (x (eat--t-cur-x cursor))
         (scroll-begin (eat--t-term-scroll-begin eat--t-term))
         (scroll-end (eat--t-term-scroll-end eat--t-term)))
    ;; N should be positive and shouldn't exceed the number of
    ;; lines below cursor position and inside current scroll
    ;; region.
    (setq n (min (- (1+ (- scroll-end scroll-begin))
                    (1- (eat--t-cur-y cursor)))
                 (max (or n 1) 1)))
    ;; Make sure we are in the scroll region and N is positive, return
    ;; on failure.
    (when (and (<= scroll-begin (eat--t-cur-y cursor) scroll-end)
               (not (zerop n)))
      ;; Delete N lines (including the current one).
      (eat--t-goto-bol)
      (save-excursion
        (let ((m (point)))
          (eat--t-goto-bol n)
          (delete-region m (point))))
      (let ((face (eat--t-term-face eat--t-term)))
        ;; Keep the lines beyond end of scroll region unmoved.
        (when (or (< scroll-end (eat--t-disp-height disp))
                  (eat--t-face-bg face))
          (let* ((pos (point))
                 (move (- (1+ (- scroll-end scroll-begin))
                          (- (+ (eat--t-cur-y cursor) n)
                             (1- scroll-begin))))
                 (moved (eat--t-goto-eol move)))
            (when (or (/= (point) (point-max))
                      (eat--t-face-bg face))
              ;; Move to the end of scroll region.
              (eat--t-repeated-insert ?\n (- move moved))
              ;; Insert enough new lines, fill them when SGR
              ;; background attribute is set.
              (if (not (eat--t-face-bg face))
                  (eat--t-repeated-insert ?\n n)
                (dotimes (_ n)
                  (insert ?\n)
                  (eat--t-repeated-insert ?\s (eat--t-disp-width disp)
                                          (eat--t-face-face face)))))
            (goto-char pos))))
      ;; Go to column where cursor is to preserve cursor position, use
      ;; spaces if needed to reach the position.
      (eat--t-repeated-insert
       ?\s (- (1- x) (eat--t-col-motion (1- x)))))))

(defun eat--t-repeat-last-char (&optional n)
  "Repeat last character N times."
  ;; N must be at least one.
  (setq n (max (or n 1) 1))
  (let* ((disp (eat--t-term-display eat--t-term))
         (char
          ;; Get the character before cursor.
          (when (< (eat--t-disp-begin disp) (point))
            (if (get-text-property (1- (point)) 'eat--t-wrap-line)
                ;; The character before cursor is a newline to break
                ;; a long line, so use the character before that.
                (when (< (eat--t-disp-begin disp) (1- (point)))
                  (char-before (1- (point))))
              (char-before)))))
    ;; Insert `char' N times.  Make sure `char' is a non-nil and not
    ;; a newline.
    (when (and char (/= char ?\n))
      (eat--t-write (make-string n char)))))

(defun eat--t-change-scroll-region (&optional top bottom)
  "Change the scroll region from lines TOP to BOTTOM (inclusive).

TOP defaults to 1 and BOTTOM defaults to the height of the display."
  (let ((disp (eat--t-term-display eat--t-term)))
    (setq top (or top 1))
    (setq bottom (or bottom (eat--t-disp-height disp)))
    ;; According to DEC's documentation (found somewhere on the
    ;; internet, but can't remember where), TOP and BOTTOM must be
    ;; within display, and BOTTOM must be below TOP.  Otherwise the
    ;; control function is a nop.
    (when (< 0 top bottom (1+ (eat--t-disp-height disp)))
      (setf (eat--t-term-scroll-begin eat--t-term) top
            (eat--t-term-scroll-end eat--t-term) bottom)
      (eat--t-goto 1 1))))

(defun eat--t-insert-mode ()
  "Enable insert mode and disable replace mode."
  (setf (eat--t-term-ins-mode eat--t-term) t))

(defun eat--t-replace-mode ()
  "Enable replace mode and disable insert mode."
  (setf (eat--t-term-ins-mode eat--t-term) nil))

(defun eat--t-set-sgr-params (params)
  "Set SGR parameters PARAMS."
  (let ((face (eat--t-term-face eat--t-term)))
    ;; Set attributes.
    (while params
      (pcase (pop params)
        (`(,(or 0 'nil))
         (1value (setf (eat--t-face-fg face) nil))
         (1value (setf (eat--t-face-bg face) nil))
         (1value (setf (eat--t-face-intensity face) nil))
         (1value (setf (eat--t-face-italic face) nil))
         (1value (setf (eat--t-face-underline face) nil))
         (1value (setf (eat--t-face-underline-color face) nil))
         (1value (setf (eat--t-face-crossed face) nil))
         (1value (setf (eat--t-face-conceal face) nil))
         (1value (setf (eat--t-face-inverse face) nil))
         (1value (setf (eat--t-face-blink face) nil))
         (1value (setf (eat--t-face-font face) 'eat-term-font-0)))
        ('(1)
         (1value (setf (eat--t-face-intensity face) 'eat-term-bold)))
        ('(2)
         (1value (setf (eat--t-face-intensity face) 'eat-term-faint)))
        ('(3)
         (1value (setf (eat--t-face-italic face) 'eat-term-italic)))
        ('(4)
         (1value (setf (eat--t-face-underline face) 'line)))
        ('(4 0)
         (1value (setf (eat--t-face-underline face) nil)))
        ('(4 1)
         (1value (setf (eat--t-face-underline face) 'line)))
        ('(4 2)
         (1value (setf (eat--t-face-underline face) 'line)))
        ('(4 3)
         (1value (setf (eat--t-face-underline face) 'wave)))
        ('(4 4)
         (1value (setf (eat--t-face-underline face) 'wave)))
        ('(4 5)
         (1value (setf (eat--t-face-underline face) 'wave)))
        ('(5)
         (1value (setf (eat--t-face-blink face) 'eat-term-slow-blink)))
        ('(6)
         (setf (eat--t-face-blink face) 'eat-term-fast-blink))
        ('(7)
         (1value (1value (setf (eat--t-face-inverse face) t))))
        ('(8)
         (1value (setf (eat--t-face-conceal face) t)))
        ('(9)
         (1value (setf (eat--t-face-crossed face) t)))
        (`(,(and (pred (lambda (font) (<= 10 font 19)))
                 font))
         (setf (eat--t-face-font face)
               (intern (format "eat-term-font-%i" (- font 10)))))
        ('(21)
         (1value (setf (eat--t-face-underline face) 'line)))
        ('(22)
         (1value (setf (eat--t-face-intensity face) nil)))
        ('(23)
         (1value (setf (eat--t-face-italic face) nil)))
        ('(24)
         (1value (setf (eat--t-face-underline face) nil)))
        ('(25)
         (1value (setf (eat--t-face-blink face) nil)))
        ('(27)
         (1value (setf (eat--t-face-inverse face) nil)))
        ('(28)
         (1value (setf (eat--t-face-conceal face) nil)))
        ('(29)
         (1value (setf (eat--t-face-crossed face) nil)))
        (`(,(and (pred (lambda (color) (<= 30 color 37)))
                 color))
         (setf (eat--t-face-fg face)
               (face-foreground
                (intern (format "eat-term-color-%i" (- color 30)))
                nil t)))
        ('(38)
         (pcase (pop params)
           ('(2)
            (setf (eat--t-face-fg face)
                  (let ((r (car (pop params)))
                        (g (car (pop params)))
                        (b (car (pop params))))
                    (when (and r (<= 0 r 255)
                               g (<= 0 g 255)
                               b (<= 0 b 255))
                      (format "#%02x%02x%02x" r g b)))))
           ('(5)
            (let ((color (car (pop params))))
              (setf (eat--t-face-fg face)
                    (when (and color (<= 0 color 255))
                      (face-foreground
                       (intern (format "eat-term-color-%i" color))
                       nil t)))))))
        ('(39)
         (1value (setf (eat--t-face-fg face) nil)))
        (`(,(and (pred (lambda (color) (<= 40 color 47)))
                 color))
         (setf (eat--t-face-bg face)
               (face-foreground
                (intern (format "eat-term-color-%i" (- color 40)))
                nil t)))
        ('(48)
         (setf (eat--t-face-bg face)
               (pcase (pop params)
                 ('(2)
                  (let ((r (car (pop params)))
                        (g (car (pop params)))
                        (b (car (pop params))))
                    (when (and r (<= 0 r 255)
                               g (<= 0 g 255)
                               b (<= 0 b 255))
                      (format "#%02x%02x%02x" r g b))))
                 ('(5)
                  (let ((color (car (pop params))))
                    (when (and color (<= 0 color 255))
                      (face-foreground
                       (intern (format "eat-term-color-%i" color))
                       nil t)))))))
        ('(49)
         (1value (setf (eat--t-face-bg face) nil)))
        ('(58)
         (setf (eat--t-face-underline-color face)
               (pcase (pop params)
                 ('(2)
                  (let ((r (car (pop params)))
                        (g (car (pop params)))
                        (b (car (pop params))))
                    (when (and r (<= 0 r 255)
                               g (<= 0 g 255)
                               b (<= 0 b 255))
                      (format "#%02x%02x%02x" r g b))))
                 ('(5)
                  (let ((color (car (pop params))))
                    (when (and color (<= 0 color 255))
                      (face-foreground
                       (intern (format "eat-term-color-%i" color))
                       nil t)))))))
        ('(59)
         (1value (setf (eat--t-face-underline-color face) nil)))
        (`(,(and (pred (lambda (color) (<= 90 color 97)))
                 color))
         (setf (eat--t-face-fg face)
               (face-foreground
                (intern (format "eat-term-color-%i" (- color 82)))
                nil t)))
        (`(,(and (pred (lambda (color) (<= 100 color 107)))
                 color))
         (setf (eat--t-face-bg face)
               (face-foreground
                (intern (format "eat-term-color-%i" (- color 92)))
                nil t)))))
    ;; Update face according to the attributes.
    (setf (eat--t-face-face face)
          `(,@(and-let* ((fg (or (if (eat--t-face-conceal face)
                                     (eat--t-face-bg face)
                                   (eat--t-face-fg face))
                                 (cond
                                  ((eat--t-face-inverse face)
                                   (face-foreground 'default))
                                  ((eat--t-face-conceal face)
                                   (face-background 'default))))))
                (list (if (eat--t-face-inverse face)
                          :background
                        :foreground)
                      fg))
            ,@(and-let* ((bg (or (eat--t-face-bg face)
                                 (and (eat--t-face-inverse face)
                                      (face-background 'default)))))
                (list (if (eat--t-face-inverse face)
                          :foreground
                        :background)
                      bg))
            ,@(and-let* ((underline (eat--t-face-underline face)))
                (list
                 :underline
                 (list :color (eat--t-face-underline-color face)
                       :style underline)))
            ,@(and-let* ((crossed (eat--t-face-crossed face)))
                ;; REVIEW: How about colors?  No terminal supports
                ;; crossed attribute with colors, so we'll need to be
                ;; creative to add the feature.
                `(:strike-through t))
            :inherit
            (,@(and-let* ((intensity (eat--t-face-intensity face)))
                 (list intensity))
             ,@(and-let* ((italic (eat--t-face-italic face)))
                 (cl-assert (1value (eq (1value italic)
                                        'eat-term-italic)))
                 (list (1value italic)))
             ,@(and-let* ((blink (eat--t-face-blink face)))
                 (list blink))
             ,(eat--t-face-font face))))))

(defun eat--t-enable-keypad ()
  "Enable keypad."
  (1value (setf (eat--t-term-keypad-mode eat--t-term) t)))

(defun eat--t-disable-keypad ()
  "Disable keypad."
  (1value (setf (eat--t-term-keypad-mode eat--t-term) nil)))

(defun eat--t-enable-sgr-mouse-encoding ()
  "Arrange that the following mouse events will be encoded like SGR."
  (setf (eat--t-term-mouse-encoding eat--t-term) 'sgr))

(defun eat--t-disable-sgr-mouse-encoding ()
  "Arrange that the following mouse events won't be encoded like SGR."
  (setf (eat--t-term-mouse-encoding eat--t-term) nil))

(defun eat--t-set-mouse-mode (mode)
  "Set current mouse mode to MODE.

MODE should be one of nil and `x10', `normal', `button-event',
`any-event'."
  (setf (eat--t-term-mouse-mode eat--t-term) mode)
  ;; When MODE is nil, disable mouse.
  (unless mode
    (eat--t-disable-sgr-mouse-encoding))
  ;; `x10' mouse mode doesn't need to keep track of the mouse buttons
  ;; pressed.
  (when (or (not mode)
            (eq mode 'x10))
    (setf (eat--t-term-mouse-pressed eat--t-term) nil))
  ;; Inform the UI.
  (funcall (eat--t-term-grab-mouse-fn eat--t-term) eat--t-term
           (pcase-exhaustive mode
             ('x10 :click)
             ('normal :modifier-click)
             ('button-event :drag)
             ('any-event :all)
             ('nil nil))))

(defun eat--t-enable-x10-mouse ()
  "Enable X10 mouse tracking."
  (eat--t-set-mouse-mode 'x10))

(defun eat--t-enable-normal-mouse ()
  "Enable normal mouse tracking."
  (eat--t-set-mouse-mode 'normal))

(defun eat--t-enable-button-event-mouse ()
  "Enable button-event mouse tracking."
  (eat--t-set-mouse-mode 'button-event))

(defun eat--t-enable-any-event-mouse ()
  "Enable any-event mouse tracking."
  (eat--t-set-mouse-mode 'any-event))

(defun eat--t-disable-mouse ()
  "Disable mouse tracking."
  (eat--t-set-mouse-mode nil))

(defun eat--t-enable-focus-event ()
  "Enable sending focus events."
  (1value (setf (eat--t-term-focus-event-mode eat--t-term) t))
  (funcall (eat--t-term-set-focus-ev-mode-fn eat--t-term) eat--t-term
           t))

(defun eat--t-disable-focus-event ()
  "Disable sending focus events."
  (1value (setf (eat--t-term-focus-event-mode eat--t-term) nil))
  (funcall (eat--t-term-set-focus-ev-mode-fn eat--t-term) eat--t-term
           nil))

(defun eat--t-set-title (title)
  "Set the title of terminal to TITLE."
  ;; Update title.
  (setf (eat--t-term-title eat--t-term) title)
  ;; Inform the UI.
  (funcall (eat--t-term-set-title-fn eat--t-term) eat--t-term title))

(defun eat--t-set-cwd (format host path)
  "Set the working directory of terminal to URL.

If FORMAT is `base64', HOST should be base64 encoded hostname and PATH
should be base64 encoded working directory path.

If FORMAT is `url', HOST should be nil and PATH should be an URL in
the format \"file://HOST/CWD/\"; HOST can be empty."
  (pcase-exhaustive format
    ('base64
     (let ((dir (ignore-errors (expand-file-name
                                (file-name-as-directory
                                 (base64-decode-string path)))))
           (hostname (ignore-errors (base64-decode-string host))))
       (when (and dir hostname)
         ;; Update working directory.
         (setf (eat--t-term-cwd eat--t-term) (cons hostname dir))
         ;; Inform the UI.
         (funcall (eat--t-term-set-cwd-fn eat--t-term)
                  eat--t-term hostname dir))))
    ('url
     (let ((url (url-generic-parse-url path)))
       (when (string= (url-type url) "file")
         (let ((host (url-host url))
               (dir (expand-file-name
                     (file-name-as-directory
                      (url-unhex-string (url-filename url))))))
           ;; Update working directory.
           (setf (eat--t-term-cwd eat--t-term) (cons host dir))
           ;; Inform the UI.
           (funcall (eat--t-term-set-cwd-fn eat--t-term)
                    eat--t-term host dir)))))))

(defun eat--t-send-device-attrs (params format)
  "Return device attributes.

PARAMS is the parameter list and FORMAT is the format of parameters in
output."
  (setq params (or params '((0))))
  (pcase-exhaustive format
    ('nil
     (when (= (caar params) 0)
       (funcall (eat--t-term-input-fn eat--t-term) eat--t-term
                "\e[?1;2c")))
    (?>
     (when (= (caar params) 0)
       (funcall (eat--t-term-input-fn eat--t-term) eat--t-term
                "\e[>0;242;0c")))))

(defun eat--t-report-foreground-color ()
  "Report the current default foreground color to the client."
  (funcall
   (eat--t-term-input-fn eat--t-term) eat--t-term
   (let ((rgb (or (color-values (face-foreground 'default))
                  ;; On terminals like TTYs the above returns nil.
                  ;; Terminals usually have a white foreground, so...
                  '(255 255 255))))
     (format "\e]10;%04x/%04x/%04x\e\\"
             (pop rgb) (pop rgb) (pop rgb)))))

(defun eat--t-report-background-color ()
  "Report the current default background color to the client."
  (funcall
   (eat--t-term-input-fn eat--t-term) eat--t-term
   (let ((rgb (or (color-values (face-background 'default))
                  ;; On terminals like TTYs the above returns nil.
                  ;; Terminals usually have a black background, so...
                  '(0 0 0))))
     (format "\e]11;%04x/%04x/%04x\e\\"
             (pop rgb) (pop rgb) (pop rgb)))))

(defun eat--t-manipulate-selection (targets data)
  "Set and send current selection.

TARGETS is a string containing zero or more characters from the set
`c', `p', `q', `s', `0', `1', `2', `3', `4', `5', `6', `7', `8', `9'.
DATA is the selection data encoded in base64."
  (when (string-empty-p targets)
    (setq targets "s0"))
  (if (string= data "?")
      ;; The client is requesting for clipboard content, let's try to
      ;; fulfill the request.
      (funcall
       (eat--t-term-input-fn eat--t-term) eat--t-term
       (let ((str nil)
             (source nil)
             (n 0))
         (while (and (not str) (< n (length targets)))
           (setq
            str
            (pcase (aref targets n)
              ;; c, p, q and s targets are handled by the UI, and they
              ;; might refuse to give the clipboard content.
              (?c
               (funcall
                (eat--t-term-manipulate-selection-fn eat--t-term)
                eat--t-term :clipboard t))
              (?p
               (funcall
                (eat--t-term-manipulate-selection-fn eat--t-term)
                eat--t-term :primary t))
              (?q
               (funcall
                (eat--t-term-manipulate-selection-fn eat--t-term)
                eat--t-term :secondary t))
              (?s
               (funcall
                (eat--t-term-manipulate-selection-fn eat--t-term)
                eat--t-term :select t))
              ;; 0 to 9 targets are handled by us, and always work.
              ((and (pred (<= ?0))
                    (pred (>= ?9))
                    i)
               (aref (eat--t-term-cut-buffers eat--t-term)
                     (- i ?0)))))
           ;; If we got a string to send, record the source to inform
           ;; the client.
           (when str
             (setq source (string (aref targets n))))
           (cl-incf n))
         ;; No string to send, so send an empty string and an empty
         ;; target string meaning that we don't have any answer.
         (unless str
           (setq str "")
           (setq source ""))
         (format "\e]52;%s;%s\e\\" source
                 (base64-encode-string str))))
    ;; The client is requesting to set clipboard content, let's try to
    ;; fulfill the request.
    (let ((str (ignore-error error
                 (decode-coding-string (base64-decode-string data)
                                       locale-coding-system))))
      (seq-doseq (target targets)
        (pcase target
          ;; c, p, q and s targets are handled by the UI, and they
          ;; might reject the new clipboard content.
          (?c
           (funcall (eat--t-term-manipulate-selection-fn eat--t-term)
                    eat--t-term :clipboard str))
          (?p
           (funcall (eat--t-term-manipulate-selection-fn eat--t-term)
                    eat--t-term :primary str))
          (?q
           (funcall (eat--t-term-manipulate-selection-fn eat--t-term)
                    eat--t-term :secondary str))
          (?s
           (funcall (eat--t-term-manipulate-selection-fn eat--t-term)
                    eat--t-term :select str))
          ;; 0 to 9 targets are handled by us, and always work.
          ((and (pred (<= ?0))
                (pred (>= ?9))
                i)
           (aset (eat--t-term-cut-buffers eat--t-term) (- i ?0)
                 str)))))))

(defun eat--t-prompt-start ()
  "Call shell prompt start hook."
  (funcall (eat--t-term-prompt-start-fn eat--t-term) eat--t-term))

(defun eat--t-prompt-end ()
  "Call shell prompt end hook."
  (funcall (eat--t-term-prompt-end-fn eat--t-term) eat--t-term))

(defun eat--t-cont-prompt-start ()
  "Call shell prompt start hook."
  (funcall (eat--t-term-cont-prompt-start-fn eat--t-term)
           eat--t-term))

(defun eat--t-cont-prompt-end ()
  "Call shell prompt end hook."
  (funcall (eat--t-term-cont-prompt-end-fn eat--t-term) eat--t-term))

(defun eat--t-set-cmd (cmd)
  "Set the command being executed to CMD."
  (let ((c (ignore-errors (base64-decode-string cmd))))
    (when c
      (funcall (eat--t-term-set-cmd-fn eat--t-term) eat--t-term c))))

(defun eat--t-cmd-start ()
  "Call shell command start hook."
  (funcall (eat--t-term-cmd-start-fn eat--t-term) eat--t-term))

(defun eat--t-cmd-finish (status)
  "Call shell command finish hook with exit status STATUS."
  (funcall (eat--t-term-cmd-finish-fn eat--t-term) eat--t-term
           status))

(defun eat--t-set-modes (params format)
  "Set modes according to PARAMS in format FORMAT."
  ;; Dispatch the request to appropriate function.
  (pcase format
    ('nil
     (while params
       (pcase (pop params)
         ('(4)
          (eat--t-insert-mode)))))
    (??
     (while params
       (pcase (pop params)
         ('(1)
          (eat--t-enable-keypad))
         ('(7)
          (eat--t-enable-auto-margin))
         ('(9)
          (eat--t-enable-x10-mouse))
         ('(12)
          (eat--t-blinking-cursor))
         ('(25)
          (eat--t-default-cursor))
         ('(1000)
          (eat--t-enable-normal-mouse))
         ('(1002)
          (eat--t-enable-button-event-mouse))
         ('(1003)
          (eat--t-enable-any-event-mouse))
         ('(1004)
          (eat--t-enable-focus-event))
         ('(1006)
          (eat--t-enable-sgr-mouse-encoding))
         ('(1048)
          (eat--t-save-cur))
         (`(,(or 1047 1049))
          (eat--t-enable-alt-disp))
         ('(2004)
          (eat--t-enable-bracketed-yank)))))))

(defun eat--t-reset-modes (params format)
  "Reset modes according to PARAMS in format FORMAT."
  ;; Dispatch the request to appropriate function.
  (pcase format
    ('nil
     (while params
       (pcase (pop params)
         ('(4)
          (eat--t-replace-mode)))))
    (??
     (while params
       (pcase (pop params)
         ('(1)
          (eat--t-disable-keypad))
         ('(7)
          (eat--t-disable-auto-margin))
         ('(12)
          (eat--t-non-blinking-cursor))
         ('(25)
          (eat--t-invisible-cursor))
         (`(,(or 9 1000 1002 1003))
          (eat--t-disable-mouse))
         ('(1004)
          (eat--t-disable-focus-event))
         ('(1006)
          (eat--t-disable-sgr-mouse-encoding))
         ('(1047)
          (eat--t-disable-alt-disp 'dont-move-cursor))
         ('(1048)
          (eat--t-restore-cur))
         ('(1049)
          (eat--t-disable-alt-disp))
         ('(2004)
          (eat--t-disable-bracketed-yank)))))))

(defun eat--t-handle-output (output)
  "Parse and evaluate OUTPUT."
  (let ((index 0))
    (while (< index (length output))
      (pcase-exhaustive (eat--t-term-parser-state eat--t-term)
        ('nil
         ;; Regular expression to find the end of plain text.
         (let ((match (string-match
                       (1value (rx (or ?\0 ?\a ?\b ?\t ?\n ?\v
                                       ?\f ?\r ?\C-n ?\C-o ?\e
                                       #x7f)))
                       output index)))
           (if (not match)
               ;; The regex didn't match, so everything left to handle
               ;; is just plain text.
               (progn
                 (eat--t-write output index)
                 (setq index (length output)))
             (when (/= match index)
               ;; The regex matched, and the position is after the
               ;; current position.  Process the plain text between
               ;; them and advance to the control sequence.
               (eat--t-write output index match)
               (setq index match))
             ;; Dispatch control sequence.
             (cl-incf index)
             (pcase-exhaustive (aref output (1- index))
               (?\a
                (eat--t-bell))
               (?\b
                (eat--t-cur-left 1))
               (?\t
                (eat--t-horizontal-tab 1))
               (?\n
                (eat--t-line-feed))
               (?\v
                (eat--t-index))
               (?\f
                (eat--t-form-feed))
               (?\r
                ;; Avoid going to line home just before a line feed,
                ;; we can just insert a new line if we are at the
                ;; end of display.
                (unless (and (/= index (length output))
                             (= (aref output index) ?\n))
                  (eat--t-carriage-return)))
               (?\C-n
                (eat--t-change-charset 'g1))
               (?\C-o
                (eat--t-change-charset 'g0))
               (?\e
                (1value (setf (eat--t-term-parser-state eat--t-term)
                              '(read-esc))))))))
        ('(read-esc)
         (let ((type (aref output index)))
           (cl-incf index)
           (1value (setf (eat--t-term-parser-state eat--t-term) nil))
           ;; Dispatch control sequence.
           (pcase type
             ;; ESC (.
             (?\(
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-charset-standard g0 "")))
             ;; ESC ).
             (?\)
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-charset-standard g1 "")))
             ;; ESC *.
             (?*
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-charset-standard g2 "")))
             ;; ESC +.
             (?+
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-charset-standard g3 "")))
             ;; ESC -.
             (?-
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-charset-vt300 g1 "")))
             ;; ESC ..
             (?.
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-charset-vt300 g2 "")))
             ;; ESC /.
             (?/
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-charset-vt300 g3 "")))
             ;; ESC 7.
             (?7
              (eat--t-save-cur))
             ;; ESC 8.
             (?8
              (eat--t-restore-cur))
             ;; ESC D.
             (?D
              (eat--t-index))
             ;; ESC E.
             (?E
              (eat--t-line-feed))
             ;; ESC M.
             (?M
              (eat--t-reverse-index))
             ;; ESC P, or DCS.
             (?P
              (1value (setf (eat--t-term-parser-state eat--t-term)
                            '(read-dcs ""))))
             ;; ESC X, or SOS.
             (?X
              (1value (setf (eat--t-term-parser-state eat--t-term)
                            '(read-sos ""))))
             ;; ESC [, or CSI.
             (?\[
              (1value (setf (eat--t-term-parser-state eat--t-term)
                            '(read-csi ""))))
             ;; ESC ], or OSC.
             (?\]
              (1value (setf (eat--t-term-parser-state eat--t-term)
                            '(read-osc ""))))
             ;; ESC ^, or PM.
             (?^
              (1value (setf (eat--t-term-parser-state eat--t-term)
                            '(read-pm ""))))
             ;; ESC _, or APC.
             (?_
              (1value (setf (eat--t-term-parser-state eat--t-term)
                            '(read-apc ""))))
             ;; ESC c.
             (?c
              (eat--t-reset))
             ;; ESC n.
             (?n
              (eat--t-change-charset 'g2))
             ;; ESC o.
             (?o
              (eat--t-change-charset 'g3)))))
        (`(read-csi ,buf)
         (let ((match (string-match (rx (any (#x40 . #x7e)))
                                    output index)))
           (if (not match)
               (progn
                 (setf (eat--t-term-parser-state eat--t-term)
                       `(read-csi ,(concat buf (substring output
                                                          index))))
                 (setq index (length output)))
             (setf (eat--t-term-parser-state eat--t-term) nil)
             (pcase
                 (let ((str (concat buf (substring output index
                                                   match)))
                       (format nil)
                       (intermediate-bytes ""))
                   (save-match-data
                     (when (string-match
                            (rx (zero-or-more (any (?\s . ?/)))
                                string-end)
                            str)
                       (setq str (substring
                                  str 0 (match-beginning 0))
                             intermediate-bytes
                             (match-string 0 str))))
                   (when (and (not (string-empty-p str))
                              (= (aref str 0) ??))
                     (setq format ?? str (substring str 1)))
                   (when (and (not (string-empty-p str))
                              (= (aref str 0) ?>))
                     (setq format ?> str (substring str 1)))
                   (when (and (not (string-empty-p str))
                              (= (aref str 0) ?=))
                     (setq format ?= str (substring str 1)))
                   (setq index (match-end 0))
                   (list
                    (concat intermediate-bytes
                            (match-string 0 output))
                    format
                    (cond
                     ((string-empty-p str) '((nil)))
                     ((<= #x30 (aref str 0) #x3b)
                      (mapcar (lambda (p)
                                (mapcar (lambda (s)
                                          (unless (string-empty-p s)
                                            (string-to-number s)))
                                        (split-string p ":")))
                              (split-string str ";")))
                     (t str))))
               ;; CSI <n> @.
               (`("@" nil ,(and (pred listp) params))
                (eat--t-insert-char (caar params)))
               ;; CSI <n> A.
               ;; CSI <n> k.
               (`(,(or "A" "k") nil ,(and (pred listp) params))
                (eat--t-cur-up (caar params)))
               ;; CSI <n> B.
               ;; CSI <n> e.
               (`(,(or "B" "e") nil ,(and (pred listp) params))
                (eat--t-cur-down (caar params)))
               ;; CSI <n> C.
               ;; CSI <n> a.
               (`(,(or "C" "a") nil ,(and (pred listp) params))
                (eat--t-cur-right (caar params)))
               ;; CSI <n> D.
               ;; CSI <n> j.
               (`(,(or "D" "j") nil ,(and (pred listp) params))
                (eat--t-cur-left (caar params)))
               ;; CSI <n> E.
               (`("E" nil ,(and (pred listp) params))
                (eat--t-beg-of-prev-line (caar params)))
               ;; CSI <n> F.
               (`("F" nil ,(and (pred listp) params))
                (eat--t-beg-of-next-line (caar params)))
               ;; CSI <n> G.
               ;; CSI <n> `.
               (`(,(or "G" "`") nil ,(and (pred listp) params))
                (eat--t-cur-horizontal-abs (caar params)))
               ;; CSI <n> ; <m> H
               ;; CSI <n> ; <m> f
               (`(,(or "H" "f") nil ,(and (pred listp) params))
                (eat--t-goto (caar params) (caadr params)))
               ;; CSI <n> I.
               (`("I" nil ,(and (pred listp) params))
                (eat--t-horizontal-tab (caar params)))
               ;; CSI <n> J.
               (`("J" nil ,(and (pred listp) params))
                (eat--t-erase-in-disp (caar params)))
               ;; CSI <n> K.
               (`("K" nil ,(and (pred listp) params))
                (eat--t-erase-in-line (caar params)))
               ;; CSI <n> L.
               (`("L" nil ,(and (pred listp) params))
                (eat--t-insert-line (caar params)))
               ;; CSI <n> M.
               (`("M" nil ,(and (pred listp) params))
                (eat--t-delete-line (caar params)))
               ;; CSI <n> P.
               (`("P" nil ,(and (pred listp) params))
                (eat--t-delete-char (caar params)))
               ;; CSI <n> S.
               (`("S" nil ,(and (pred listp) params))
                (eat--t-scroll-up (caar params)))
               ;; CSI <n> T.
               (`("T" nil ,(and (pred listp) params))
                (eat--t-scroll-down (caar params)))
               ;; CSI <n> X.
               (`("X" nil ,(and (pred listp) params))
                (eat--t-erase-char (caar params)))
               ;; CSI <n> Z.
               (`("Z" nil ,(and (pred listp) params))
                (eat--t-horizontal-backtab (caar params)))
               ;; CSI <n> b.
               (`("b" nil ,(and (pred listp) params))
                (eat--t-repeat-last-char (caar params)))
               ;; CSI <n> c.
               ;; CSI > <n> c.
               (`("c" ,format ,(and (pred listp) params))
                (eat--t-send-device-attrs params format))
               ;; CSI <n> d.
               (`("d" nil ,(and (pred listp) params))
                (eat--t-cur-vertical-abs (caar params)))
               ;; CSI ... h.
               ;; CSI ? ... h.
               (`("h" ,format ,(and (pred listp) params))
                (eat--t-set-modes params format))
               ;; CSI ... l.
               ;; CSI ? ... l.
               (`("l" ,format ,(and (pred listp) params))
                (eat--t-reset-modes params format))
               ;; CSI ... m.
               (`("m" nil ,(and (pred listp) params))
                (eat--t-set-sgr-params params))
               ;; CSI 6 n.
               ('("n" nil ((6)))
                (eat--t-device-status-report))
               ;; CSI <n> ; <n> r.
               (`("r" nil ,(and (pred listp) params))
                (eat--t-change-scroll-region (caar params)
                                             (caadr params)))
               ;; CSI s.
               (`("s" nil nil)
                (eat--t-save-cur))
               ;; CSI u.
               (`("u" nil nil)
                (eat--t-restore-cur))))))
        (`(,(and (or 'read-dcs 'read-sos 'read-osc 'read-pm 'read-apc)
                 state)
           ,buf)
         ;; Find the end of string.
         (let ((match (string-match (if (eq state 'read-osc)
                                        (rx (or ?\a ?\\))
                                      (rx ?\\))
                                    output index)))
           (if (not match)
               (progn
                 ;; Not found, store the text to process it later when
                 ;; we get the end of string.
                 (setf (eat--t-term-parser-state eat--t-term)
                       `(,state ,(concat buf (substring output
                                                        index))))
                 (setq index (length output)))
             ;; Matched!  Get the string from the output and previous
             ;; runs.
             (let ((str (concat buf (substring output index
                                               match))))
               (setq index (match-end 0))
               ;; Is it really the end of string?
               (if (and (= (aref output match) ?\\)
                        (not (or (zerop (length str))
                                 (= (aref str (1- (length str)))
                                    ?\e))))
                   ;; No.  Push the '\' character to process later.
                   (setf (eat--t-term-parser-state eat--t-term)
                         `(,state ,(concat str "\\")))
                 ;; Yes!  It's the end!  We can parse it.
                 (when (= (aref output match) ?\\)
                   (setq str (substring str 0 (1- (length str)))))
                 (setf (eat--t-term-parser-state eat--t-term) nil)
                 ;; Dispatch control sequence.
                 (pcase state
                   ('read-osc
                    (pcase str
                      ;; OSC 0 ; <t> ST.
                      ;; OSC 2 ; <t> ST.
                      ((rx string-start (or ?0 ?2) ?\;
                           (let title (zero-or-more anything))
                           string-end)
                       (eat--t-set-title title))
                      ;; OSC 7 ; <t> ST.
                      ((rx string-start ?7 ?\;
                           (let url (zero-or-more anything))
                           string-end)
                       (eat--t-set-cwd 'url nil url))
                      ;; OSC 1 0 ; ? ST.
                      ("10;?"
                       (eat--t-report-foreground-color))
                      ;; OSC 1 1 ; ? ST.
                      ("11;?"
                       (eat--t-report-background-color))
                      ;; In XTerm, OSC 51 is reserved for Emacs shell.
                      ;; I have no idea why, but Vterm uses this OSC
                      ;; to set the current directory and remotely
                      ;; execute Emacs Lisp code.  Vterm uses the
                      ;; characters 'A' and 'E' as the first character
                      ;; of second parameter of this OSC.  We use 'e'
                      ;; as the second parameter, followed by one or
                      ;; more parameters.
                      ;; OSC 5 1 ; e ; A ; <t> ; <s> ST
                      ((rx string-start "51;e;A;"
                           (let host (zero-or-more (not ?\;)))
                           ?\; (let path (zero-or-more anything))
                           string-end)
                       (eat--t-set-cwd 'base64 host path))
                      ;; OSC 5 1 ; e ; B ST
                      ("51;e;B"
                       (eat--t-prompt-start))
                      ;; OSC 5 1 ; e ; C ST
                      ("51;e;C"
                       (eat--t-prompt-end))
                      ;; OSC 5 1 ; e ; D ST
                      ("51;e;D"
                       (eat--t-cont-prompt-start))
                      ;; OSC 5 1 ; e ; E ST
                      ("51;e;E"
                       (eat--t-cont-prompt-end))
                      ;; OSC 5 1 ; e ; F ; <t> ST
                      ((rx string-start "51;e;F;"
                           (let cmd (zero-or-more anything))
                           string-end)
                       (eat--t-set-cmd cmd))
                      ;; OSC 5 1 ; e ; G ST
                      ("51;e;G"
                       (eat--t-cmd-start))
                      ;; OSC 5 1 ; e ; H ; <n> ST
                      ((rx string-start "51;e;H;"
                           (let status (one-or-more digit))
                           string-end)
                       (eat--t-cmd-finish (string-to-number status)))
                      ;; OSC 5 2 ; <t> ; <s> ST.
                      ((rx string-start "52;"
                           (let targets
                             (zero-or-more (any ?c ?p ?q ?s
                                                (?0 . ?9))))
                           ?\; (let data (zero-or-more anything))
                           string-end)
                       (eat--t-manipulate-selection
                        targets data))))))))))
        (`(read-charset-standard ,slot ,buf)
         ;; Find the end.
         (let ((match (string-match (rx (any ?0 ?2 ?4 ?5 ?6 ?7 ?9 ?<
                                             ?= ?> ?? ?A ?B ?C ?E ?H
                                             ?K ?Q ?R ?Y ?Z ?f))
                                    output index)))
           (if (not match)
               (progn
                 ;; Not found, store the text to process it later when
                 ;; we find the end.
                 (setf (eat--t-term-parser-state eat--t-term)
                       `(read-charset-standard
                         ,slot ,(concat buf (substring
                                             output index))))
                 (setq index (length output)))
             ;; Got the end!
             (let ((str (concat buf (substring output index
                                               (match-end 0)))))
               (setq index (match-end 0))
               (setf (eat--t-term-parser-state eat--t-term) nil)
               ;; Set the character set.
               (eat--t-set-charset
                slot
                (pcase str
                  ;; ESC ( 0.
                  ;; ESC ) 0.
                  ;; ESC * 0.
                  ;; ESC + 0.
                  ("0" 'dec-line-drawing)
                  ;; ESC ( B.
                  ;; ESC ) B.
                  ;; ESC * B.
                  ;; ESC + B.
                  ("B" 'us-ascii)))))))
        (`(read-charset-vt300 ,_slot)
         (let ((_charset (aref output index)))
           (cl-incf index)
           (setf (eat--t-term-parser-state eat--t-term) nil)
           (pcase charset
             ;; TODO: Currently ignored.  It is here just to recognize
             ;; the control sequence.
             )))))))

(defun eat--t-resize (width height)
  "Resize terminal to WIDTH x HEIGHT."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (old-width (eat--t-disp-width disp))
         (old-height (eat--t-disp-height disp)))
    ;; Don't do anything if size hasn't changed, or the new size is
    ;; too small.
    (when (and (not (and (eq old-width width)
                         (eq old-height height)))
               (>= width 1)
               (>= height 1))
      ;; Update state.
      (setf (eat--t-disp-width disp) width)
      (setf (eat--t-disp-height disp) height)
      (setf (eat--t-term-scroll-begin eat--t-term) 1)
      (setf (eat--t-term-scroll-end eat--t-term)
            (eat--t-disp-height disp))
      (set-marker (eat--t-cur-position cursor) (point))
      (if (eat--t-term-main-display eat--t-term)
          ;; For alternative display, just delete the part of the
          ;; display that went out of the edges.  So if the terminal
          ;; was enlarged, we don't have anything to do.
          (when (or (< width old-width)
                    (< height old-height))
            ;; Go to the beginning of display.
            (goto-char (eat--t-disp-begin disp))
            (dotimes (l height)
              (eat--t-col-motion width)
              (delete-region (point) (car (eat--t-eol)))
              (if (< (1+ l) height)
                  (forward-char)
                (delete-region (point) (point-max))
                (let ((y (eat--t-cur-y cursor))
                      (x (eat--t-cur-x cursor)))
                  (eat--t-goto 1 1)
                  (eat--t-goto y x)))))
        ;; REVIEW: This works, but it is very simple.  Most
        ;; terminals have more sophisticated mechanisms to do this.
        ;; It would be nice thing have them here.
        ;; Go to the beginning of display.
        (goto-char (eat--t-disp-begin disp))
        ;; Try to move to the end of previous line, maybe that's a
        ;; part of a too long line.
        (unless (bobp)
          (backward-char))
        ;; Join all long lines.
        (while (not (eobp))
          (eat--t-join-long-line))
        ;; Go to display beginning again and break long lines.
        (goto-char (eat--t-disp-begin disp))
        (while (not (eobp))
          (eat--t-break-long-line (eat--t-disp-width disp)))
        ;; Calculate the beginning position of display.
        (goto-char (point-max))
        ;; TODO: This part needs explanation.
        (let ((disp-begin (car (eat--t-bol (- (1- height))))))
          (when (< (eat--t-disp-begin disp) disp-begin)
            (goto-char (max (- (eat--t-disp-begin disp) 1)
                            (point-min)))
            (set-marker (eat--t-disp-begin disp) disp-begin)
            (while (< (point) (1- (eat--t-disp-begin disp)))
              (eat--t-join-long-line
               (1- (eat--t-disp-begin disp))))))
        ;; Update the cursor if needed.
        (when (< (eat--t-cur-position cursor)
                 (eat--t-disp-begin disp))
          (set-marker (eat--t-cur-position cursor)
                      (eat--t-disp-begin disp)))
        ;; Update the coordinates of cursor.
        (goto-char (eat--t-cur-position cursor))
        (setf (eat--t-cur-x cursor) (1+ (eat--t-current-col)))
        (goto-char (eat--t-disp-begin disp))
        (setf (eat--t-cur-y cursor)
              (let ((y 0))
                (while (< (point) (eat--t-cur-position cursor))
                  (condition-case nil
                      (search-forward
                       "\n" (eat--t-cur-position cursor))
                    (search-failed
                     (goto-char (eat--t-cur-position cursor))))
                  (cl-incf y))
                (when (or (= (point) (point-min))
                          (= (char-before) ?\n))
                  (cl-incf y))
                (max y 1)))))))

;;;###autoload
(defun eat-term-make (buffer position)
  "Make a Eat terminal at POSITION in BUFFER."
  (eat--t-make-term
   :buffer buffer
   :begin (copy-marker position t)
   :end (copy-marker position)
   :display (eat--t-make-disp
             :begin (copy-marker position)
             :old-begin (copy-marker position)
             :cursor (eat--t-make-cur
                      :position (copy-marker position)))))

(defmacro eat--t-with-env (terminal &rest body)
  "Setup the environment for TERMINAL and eval BODY in it."
  (declare (indent 1))
  `(let ((eat--t-term ,terminal))
     (with-current-buffer (eat--t-term-buffer eat--t-term)
       (save-excursion
         (save-restriction
           (narrow-to-region (eat--t-term-begin eat--t-term)
                             (eat--t-term-end eat--t-term))
           (goto-char (eat--t-cur-position
                       (eat--t-disp-cursor
                        (eat--t-term-display eat--t-term))))
           (unwind-protect
               (progn ,@body)
             (set-marker (eat--t-cur-position
                          (eat--t-disp-cursor
                           (eat--t-term-display eat--t-term)))
                         (point))
             (set-marker (eat--t-term-begin eat--t-term) (point-min))
             (set-marker (eat--t-term-end eat--t-term)
                         (point-max))))))))

(defun eat-term-delete (terminal)
  "Delete TERMINAL and do any cleanup to do."
  (let ((inhibit-quit t)
        (eat--t-term terminal))
    (with-current-buffer (eat--t-term-buffer eat--t-term)
      (save-excursion
        (save-restriction
          (narrow-to-region (eat--t-term-begin eat--t-term)
                            (eat--t-term-end eat--t-term))
          (eat--t-set-cursor-state :default)
          ;; Go to the beginning of display.
          (goto-char (eat--t-disp-begin
                      (eat--t-term-display eat--t-term)))
          ;; Join all long lines.
          (unless (bobp)
            (backward-char))
          (while (not (eobp))
            (eat--t-join-long-line)))))))

(defun eat-term-reset (terminal)
  "Reset TERMINAL."
  (let ((inhibit-quit t))
    (eat--t-with-env terminal
      (eat--t-reset))))

(defun eat-term-parameter (terminal parameter)
  "Return the value of parameter PARAMETER of TERMINAL."
  (cdr (assq parameter (eat--t-term-params terminal))))

(defun eat-term-set-parameter (terminal parameter value)
  "Set the value of parameter PARAMETER of TERMINAL to VALUE."
  (let ((pair (assq parameter (eat--t-term-params terminal))))
    (if pair
        (setcdr pair value)
      (push (cons parameter value) (eat--t-term-params terminal)))))

(gv-define-setter eat-term-parameter (value terminal parameter)
  `(eat-term-set-parameter ,terminal ,parameter ,value))

(defun eat-term-input-function (terminal)
  "Return the function used to send input from TERMINAL.

The function is called with two arguments, TERMINAL and the string to
send.  The function should not change point and buffer restriction.

To set it, use (`setf' (`eat-term-input-function' TERMINAL) FUNCTION),
where FUNCTION is the input function."
  (eat--t-term-input-fn terminal))

(gv-define-setter eat-term-input-function (function terminal)
  `(setf (eat--t-term-input-fn ,terminal) ,function))

(defun eat-term-cursor-type (terminal)
  "Return the cursor state of TERMINAL.

The return value can be one of the following:

  `:default'            Default cursor.
  `:invisible'          Invisible cursor.
  `:very-visible'       Very visible cursor."
  (eat--t-term-cur-state terminal))

(defun eat-term-set-cursor-function (terminal)
  "Return the function used to set the cursor of TERMINAL.

The function is called with two arguments, TERMINAL and a symbol STATE
describing the new state of cursor.  The function should not change
point and buffer restriction.  STATE can be one of the following:

  `:default'            Default cursor.
  `:invisible'          Invisible cursor.
  `:very-visible'       Very visible cursor.  Can also be implemented
                        as blinking cursor.

More possible values might be added in future.  So in case the
function doesn't know about a particular cursor state, it should reset
the cursor to the default like the `:default' state.

To set it, use (`setf' (`eat-term-set-cursor-function' TERMINAL)
FUNCTION), where FUNCTION is the function to set cursor."
  (eat--t-term-set-cursor-fn terminal))

(gv-define-setter eat-term-set-cursor-function (function terminal)
  `(setf (eat--t-term-set-cursor-fn ,terminal) ,function))

(defun eat-term-grab-mouse-function (terminal)
  "Return the function used to grab the mouse.

The function is called with two arguments, TERMINAL and a symbol MODE
describing the new mouse mode MODE.  The function should not change
point and buffer restriction.  MODE can be one of the following:

  nil                 Disable mouse.
  `:click'              Pass `mouse-1', `mouse-2', and `mouse-3'
                        clicks.
  `:modifier-click'     Pass all mouse click events on both press and
                        release, including `control', `meta' and
                        `shift' modifiers.
  `:drag'               All of `:modifier-click', plus dragging
                        (moving mouse while pressed) information.
  `:all'                Pass all mouse events, including movement.

More possible values might be added in future.  So in case the
function doesn't know about a particular mouse mode, it should behave
as if MODE was nil and disable mouse.

To set it, use (`setf' (`eat-term-set-mouse-mode-function' TERMINAL)
FUNCTION), where FUNCTION is the function to set mouse mode."
  (eat--t-term-grab-mouse-fn terminal))

(gv-define-setter eat-term-grab-mouse-function (function terminal)
  `(setf (eat--t-term-grab-mouse-fn ,terminal) ,function))

(defun eat-term-grab-focus-events-function (terminal)
  "Return the function used to grab focus in and out events.

The function is called with two arguments, TERMINAL and a boolean
describing the new grabbing mode.  When the boolean is nil, don't send
focus event, otherwise send focus events.  The function should not
change point and buffer restriction.

To set it, use (`setf' (`eat-term-grab-focus-events-function'
TERMINAL) FUNCTION), where FUNCTION is the function to grab focus
events."
  (eat--t-term-set-focus-ev-mode-fn terminal))

(gv-define-setter eat-term-grab-focus-events-function
    (function terminal)
  `(setf (eat--t-term-set-focus-ev-mode-fn ,terminal) ,function))

(defun eat-term-manipulate-selection-function (terminal)
  "Return the function used to manipulate selection (or `kill-ring').

The function is called with three arguments, TERMINAL, a symbol
SELECTION describing the selection paramater and DATA, a string, or a
boolean.  The function should not change point and buffer restriction.
SELECTION can be one of `:clipboard', `:primary', `:secondary',
`:select'.  When DATA is a string, it should set the selection to that
string, when DATA is nil, it should unset the selection, and when DATA
is t, it should return the selection, or nil if none.

To set it, use (`setf' (`eat-term-manipulate-selection-function'
TERMINAL) FUNCTION), where FUNCTION is the function to manipulate
selection."
  (eat--t-term-manipulate-selection-fn terminal))

(gv-define-setter eat-term-manipulate-selection-function
    (function terminal)
  `(setf (eat--t-term-manipulate-selection-fn ,terminal) ,function))

(defun eat-term-ring-bell-function (terminal)
  "Return the function used to ring the bell.

The function is called with a single argument TERMINAL.  The function
should not change point and buffer restriction.

To set it, use (`setf' (`eat-term-ring-bell-function' TERMINAL)
FUNCTION), where FUNCTION is the function to ring the bell."
  (eat--t-term-manipulate-selection-fn terminal))

(gv-define-setter eat-term-ring-bell-function (function terminal)
  `(setf (eat--t-term-bell-fn ,terminal) ,function))

(defun eat-term-title (terminal)
  "Return the current title of TERMINAL."
  (eat--t-term-title terminal))

(defun eat-term-set-title-function (terminal)
  "Return the function used to set the title of TERMINAL.

The function is called with two arguments, TERMINAL and the new title
of TERMINAL.  The function should not change point and buffer
restriction.

Note that the client is responsible for the arguments to the function,
verify them before using.

To set it, use (`setf' (`eat-term-set-title-function' TERMINAL)
FUNCTION), where FUNCTION is the function to set title."
  (eat--t-term-set-title-fn terminal))

(gv-define-setter eat-term-set-title-function (function terminal)
  `(setf (eat--t-term-set-title-fn ,terminal) ,function))

(defun eat-term-cwd (terminal)
  "Return the current working directory of TERMINAL with the hostname.

The return value is of form (HOST . PATH), where HOST is the hostname
and PATH is the working directory."
  (eat--t-term-cwd terminal))

(defun eat-term-set-cwd-function (terminal)
  "Return the function used to set the working directory of TERMINAL.

The function is called with three arguments, TERMINAL, the host where
the directory is, and the new (current) working directory of TERMINAL.
The function should not change point and buffer restriction.

Note that the client is responsible for the arguments to the function,
verify them before using.

To set it, use (`setf' (`eat-term-set-cwd-function' TERMINAL)
FUNCTION), where FUNCTION is the function to set the current working
directory."
  (eat--t-term-set-cwd-fn terminal))

(gv-define-setter eat-term-set-cwd-function (function terminal)
  `(setf (eat--t-term-set-cwd-fn ,terminal) ,function))

(defun eat-term-prompt-start-function (terminal)
  "Return the function called just before shell prompt.

The function is called with with a single argument, TERMINAL.  The
function should not change point and buffer restriction.

To set it, use (`setf' (`eat-term-prompt-start-function' TERMINAL)
FUNCTION), where FUNCTION is the function to call."
  (eat--t-term-prompt-start-fn terminal))

(gv-define-setter eat-term-prompt-start-function (function terminal)
  `(setf (eat--t-term-prompt-start-fn ,terminal) ,function))

(defun eat-term-prompt-end-function (terminal)
  "Return the function called just after shell prompt.

The function is called with with a single argument, TERMINAL.  The
function should not change point and buffer restriction.

To set it, use (`setf' (`eat-term-prompt-end-function' TERMINAL)
FUNCTION), where FUNCTION is the function to call."
  (eat--t-term-prompt-end-fn terminal))

(gv-define-setter eat-term-prompt-end-function (function terminal)
  `(setf (eat--t-term-prompt-end-fn ,terminal) ,function))

(defun eat-term-continuation-prompt-start-function (terminal)
  "Return the function called just before shell continuation prompt.

The function is called with with a single argument, TERMINAL.  The
function should not change point and buffer restriction.

To set it, use (`setf' (`eat-term-continuation-prompt-start-function'
TERMINAL) FUNCTION), where FUNCTION is the function to call."
  (eat--t-term-cont-prompt-start-fn terminal))

(gv-define-setter eat-term-continuation-prompt-start-function
    (function terminal)
  `(setf (eat--t-term-cont-prompt-start-fn ,terminal) ,function))

(defun eat-term-continuation-prompt-end-function (terminal)
  "Return the function called just after shell continuation prompt.

The function is called with with a single argument, TERMINAL.  The
function should not change point and buffer restriction.

To set it, use (`setf' (`eat-term-continuation-prompt-end-function'
TERMINAL) FUNCTION), where FUNCTION is the function to call."
  (eat--t-term-cont-prompt-end-fn terminal))

(gv-define-setter eat-term-continuation-prompt-end-function
    (function terminal)
  `(setf (eat--t-term-cont-prompt-end-fn ,terminal) ,function))

(defun eat-term-set-cmd-function (terminal)
  "Return the function used to set the command being run in TERMINAL.

The function is called with two arguments, TERMINAL, the host where
the directory is, and the new (current) working directory of TERMINAL.
The function should not change point and buffer restriction.

Note that the client is responsible for the arguments to the function,
verify them before using.

To set it, use (`setf' (`eat-term-set-cwd-function' TERMINAL)
FUNCTION), where FUNCTION is the function to set the command."
  (eat--t-term-set-cmd-fn terminal))

(gv-define-setter eat-term-set-cmd-function (function terminal)
  `(setf (eat--t-term-set-cmd-fn ,terminal) ,function))

(defun eat-term-cmd-start-function (terminal)
  "Return the function called just before a command is run in shell.

The function is called with with a single argument, TERMINAL.  The
function should not change point and buffer restriction.

To set it, use (`setf' (`eat-term-cmd-start-function' TERMINAL)
FUNCTION), where FUNCTION is the function to call."
  (eat--t-term-cmd-start-fn terminal))

(gv-define-setter eat-term-cmd-start-function (function terminal)
  `(setf (eat--t-term-cmd-start-fn ,terminal) ,function))

(defun eat-term-cmd-finish-function (terminal)
  "Return the function called after a command has finished in shell.

The function is called with with a two arguments, TERMINAL and the
exit status of the command.  The function should not change point and
buffer restriction.

To set it, use (`setf' (`eat-term-cmd-finish-function' TERMINAL)
FUNCTION), where FUNCTION is the function to call."
  (eat--t-term-cmd-finish-fn terminal))

(gv-define-setter eat-term-cmd-finish-function (function terminal)
  `(setf (eat--t-term-cmd-finish-fn ,terminal) ,function))

(defun eat-term-size (terminal)
  "Return the size of TERMINAL as (WIDTH . HEIGHT)."
  (let ((disp (eat--t-term-display terminal)))
    (cons (eat--t-disp-width disp) (eat--t-disp-height disp))))

(defun eat-term-beginning (terminal)
  "Return the beginning position of TERMINAL.

Don't use markers to store the position, call this function whenever
you need the position."
  (eat--t-term-begin terminal))

(defun eat-term-end (terminal)
  "Return the end position of TERMINAL.

This is also the end position of TERMINAL's display.

Don't use markers to store the position, call this function whenever
you need the position."
  (eat--t-term-end terminal))

(defun eat-term-display-beginning (terminal)
  "Return the beginning position of TERMINAL's display."
  (eat--t-disp-begin (eat--t-term-display terminal)))

(defun eat-term-display-cursor (terminal)
  "Return the cursor's current position on TERMINAL's display."
  (let* ((disp (eat--t-term-display terminal))
         (cursor (eat--t-disp-cursor disp)))
    ;; The cursor might be after the edge of the display.  But we
    ;; don't want the UI to show that, so show cursor at the edge.
    (if (> (eat--t-cur-x cursor) (eat--t-disp-width disp))
        (1- (eat--t-cur-position cursor))
      (eat--t-cur-position cursor))))

(defun eat-term-process-output (terminal output)
  "Process OUTPUT from client and show it on TERMINAL's display."
  (let ((inhibit-quit t))
    (eat--t-with-env terminal
      (eat--t-handle-output output))))

(defun eat-term-redisplay (terminal)
  "Prepare TERMINAL for displaying."
  (let ((inhibit-quit t))
    (eat--t-with-env terminal
      (let ((disp (eat--t-term-display eat--t-term)))
        (when (< (eat--t-disp-old-begin disp)
                 (eat--t-disp-begin disp))
          ;; Join long lines.
          (let ((limit (copy-marker (1- (eat--t-disp-begin disp)))))
            (save-excursion
              (goto-char (max (1- (eat--t-disp-old-begin disp))
                              (point-min)))
              (while (< (point) limit)
                (eat--t-join-long-line limit))))
          ;; Truncate scrollback.
          (when eat-term-scrollback-size
            (delete-region
             (point-min)
             (max (point-min) (- (point) eat-term-scrollback-size))))
          (set-marker (eat--t-disp-old-begin disp)
                      (eat--t-disp-begin disp)))))))

(defun eat-term-resize (terminal width height)
  "Resize TERMINAL to WIDTH x HEIGHT."
  (let ((inhibit-quit t))
    (eat--t-with-env terminal
      (eat--t-resize width height))))

(defun eat-term-in-alternative-display-p (terminal)
  "Return non-nil when TERMINAL is in alternative display mode."
  (eat--t-term-main-display terminal))

(defun eat-term-input-event (terminal n event &optional ref-pos)
  "Send EVENT as input N times to TERMINAL.

EVENT should be a event.  It can be any standard Emacs event, or a
event list of any of the following forms:

  (eat-focus-in)
    Terminal just gained focus.

  (eat-focus-out)
    Terminal just lost focus.

REF-POS is a mouse position list pointing to the start of terminal
display satisfying the predicate `posnp'.  It is used to calculate the
position of mouse events and `eat-mouse-drag' events on terminal when
given.

For mouse events, events should be sent on both mouse button press and
release unless the mouse grabing mode is `:click', otherwise the
client process may get confused."
  (let ((disp (eat--t-term-display terminal)))
    (cl-flet ((send (str)
                (funcall (eat--t-term-input-fn terminal)
                         terminal str)))
      (dotimes (_ (or n 1))
        (pcase event
          ;; Arrow key, `insert', `delete', `deletechar', `home',
          ;; `end', `prior', `next' and their modifier variants.
          ((and (or 'up 'down 'right 'left
                    'C-up 'C-down 'C-right 'C-left
                    'M-up 'M-down 'M-right 'M-left
                    'S-up 'S-down 'S-right 'S-left
                    'C-M-up 'C-M-down 'C-M-right 'C-M-left
                    'C-S-up 'C-S-down 'C-S-right 'C-S-left
                    'M-S-up 'M-S-down 'M-S-right 'M-S-left
                    'C-M-S-up 'C-M-S-down 'C-M-S-right 'C-M-S-left
                    'insert 'C-insert 'M-insert 'S-insert 'C-M-insert
                    'C-S-insert 'M-S-insert 'C-M-S-insert
                    'delete 'C-delete 'M-delete 'S-delete 'C-M-delete
                    'C-S-delete 'M-S-delete 'C-M-S-delete
                    'deletechar 'C-deletechar 'M-deletechar
                    'S-deletechar 'C-M-deletechar 'C-S-deletechar
                    'M-S-deletechar 'C-M-S-deletechar
                    'home 'C-home 'M-home 'S-home 'C-M-home 'C-S-home
                    'M-S-home 'C-M-S-home
                    'end 'C-end 'M-end 'S-end 'C-M-end 'C-S-end
                    'M-S-end 'C-M-S-end
                    'prior 'C-prior 'M-prior 'S-prior 'C-M-prior
                    'C-S-prior 'M-S-prior 'C-M-S-prior
                    'next 'C-next 'M-next 'S-next 'C-M-next 'C-S-next
                    'M-S-next 'C-M-S-next)
                ev)
           (send
            (format
             "\e%s%c"
             (if (not (or (memq 'control (event-modifiers ev))
                          (memq 'meta (event-modifiers ev))
                          (memq 'shift (event-modifiers ev))))
                 (pcase (event-basic-type ev)
                   ('insert "[2")
                   ((or 'delete 'deletechar) "[3")
                   ('prior "[5")
                   ('next "[6")
                   (_ (if (eat--t-term-keypad-mode terminal)
                          "O"
                        "[")))
               (format
                "[%c;%c"
                (pcase (event-basic-type ev)
                  ('insert ?2)
                  ((or 'delete 'deletechar) ?3)
                  ('prior ?5)
                  ('next ?6)
                  (_ ?1))
                (pcase-exhaustive (event-modifiers ev)
                  ((and (pred (memq 'control))
                        (pred (memq 'meta))
                        (pred (memq 'shift)))
                   ?8)
                  ((and (pred (memq 'control))
                        (pred (memq 'meta)))
                   ?7)
                  ((and (pred (memq 'control))
                        (pred (memq 'shift)))
                   ?6)
                  ((and (pred (memq 'meta))
                        (pred (memq 'shift)))
                   ?4)
                  ((pred (memq 'control))
                   ?5)
                  ((pred (memq 'meta))
                   ?3)
                  ((pred (memq 'shift))
                   ?2))))
             (pcase (event-basic-type ev)
               ('up ?A)
               ('down ?B)
               ('right ?C)
               ('left ?D)
               ('home ?H)
               ('end ?F)
               (_ ?~)))))
          ('backspace
           (send "\C-?"))
          ('C-backspace
           (send "\C-h"))
          ;; Function keys.
          ((and (pred symbolp)
                fn-key
                (let (rx string-start "f"
                         (let fn-num (one-or-more (any (?0 . ?9))))
                         string-end)
                  (symbol-name fn-key))
                (let (and (pred (<= 1))
                          (pred (>= 63))
                          key)
                  (string-to-number fn-num)))
           (send
            (aref
             ["\eOP" "\eOQ" "\eOR" "\eOS" "\e[15~" "\e[17~" "\e[18~"
              "\e[19~" "\e[20~" "\e[21~" "\e[23~" "\e[24~" "\e[1;2P"
              "\e[1;2Q" "\e[1;2R" "\e[1;2S" "\e[15;2~" "\e[17;2~"
              "\e[18;2~" "\e[19;2~" "\e[20;2~" "\e[21;2~" "\e[23;2~"
              "\e[24;2~" "\e[1;5P" "\e[1;5Q" "\e[1;5R" "\e[1;5S"
              "\e[15;5~" "\e[17;5~" "\e[18;5~" "\e[19;5~" "\e[20;5~"
              "\e[21;5~" "\e[23;5~" "\e[24;5~" "\e[1;6P" "\e[1;6Q"
              "\e[1;6R" "\e[1;6S" "\e[15;6~" "\e[17;6~" "\e[18;6~"
              "\e[19;6~" "\e[20;6~" "\e[21;6~" "\e[23;6~" "\e[24;6~"
              "\e[1;3P" "\e[1;3Q" "\e[1;3R" "\e[1;3S" "\e[15;3~"
              "\e[17;3~" "\e[18;3~" "\e[19;3~" "\e[20;3~" "\e[21;3~"
              "\e[23;3~" "\e[24;3~" "\e[1;4P" "\e[1;4Q" "\e[1;4R"]
             (1- key))))
          ((and (or (pred numberp)
                    (pred symbolp))
                char)
           ;; Adapted from Term source.
           (when (symbolp char)
             ;; Convert `return' to C-m, etc.
             (let ((tmp (get char 'event-symbol-elements)))
               (when tmp
                 (setq char (car tmp)))
               (and (symbolp char)
                    (setq tmp (get char 'ascii-character))
                    (setq char tmp))))
           (when (numberp char)
             (let ((base (event-basic-type char))
                   (mods (event-modifiers char)))
               ;; Try to avoid event-convert-list if possible.
               (if (and (characterp char)
                        (not (memq 'meta mods))
                        (not (and (memq 'control mods)
                                  (memq 'shift mods))))
                   (send (format "%c" char))
                 (when (memq 'control mods)
                   (setq mods (delq 'shift mods)))
                 (let ((ch (pcase (event-convert-list
                                   (append (remq 'meta mods)
                                           (list base)))
                             (?\C-\s ?\C-@)
                             (?\C-/ ?\C-?)
                             (?\C-- ?\C-_)
                             (c c))))
                   (when (characterp ch)
                     (send (cond
                            ((and (memq 'meta mods)
                                  (memq ch '(?\[ ?O)))
                             "\e")
                            (t
                             (format
                              (if (memq 'meta mods) "\e%c" "%c")
                              ch))))))))))
          ;; Mouse handling.
          ((and (guard (eat--t-term-mouse-mode terminal))
                mouse
                (pred eventp)
                (or (and (let mouse-type (event-basic-type mouse))
                         (let (rx string-start "mouse-"
                                  (let key-num (one-or-more
                                                (any (?0 . ?9))))
                                  string-end)
                           (symbol-name mouse-type))
                         (let (and (pred (<= 1))
                                   (pred (>= 11))
                                   mouse-num)
                           (string-to-number key-num)))
                    (and (let 'wheel-up (event-basic-type mouse))
                         (let mouse-num 4))
                    (and (let 'wheel-down (event-basic-type mouse))
                         (let mouse-num 5))
                    (and (let 'wheel-right (event-basic-type mouse))
                         (let mouse-num 6))
                    (and (let 'wheel-left (event-basic-type mouse))
                         (let mouse-num 7))))
           (let* ((modifiers (event-modifiers mouse))
                  (pos (if (memq 'drag modifiers)
                           (event-end mouse)
                         (event-start mouse)))
                  (x-y (if (< emacs-major-version 29)
                           (posn-col-row pos)
                         (with-suppressed-warnings
                             ((callargs posn-col-row))
                           (posn-col-row pos 'use-window))))
                  (x (1+ (car x-y)))
                  (y (1+ (cdr x-y)))
                  (button
                   (let ((b (aref
                             [0 1 2 64 65 66 67 128 129 130 131]
                             (1- mouse-num))))
                     (when (memq 'shift modifiers)
                       (cl-incf b 4))
                     (when (memq 'meta modifiers)
                       (cl-incf b 8))
                     (when (memq 'control modifiers)
                       (cl-incf b 16))
                     b)))
             (when ref-pos
               (let ((ref-x-y
                      (if (< emacs-major-version 29)
                          (posn-col-row ref-pos)
                        (with-suppressed-warnings
                            ((callargs posn-col-row))
                          (posn-col-row ref-pos 'use-window)))))
                 (cl-decf x (car ref-x-y))
                 (cl-decf y (cdr ref-x-y))))
             (when (and (<= 1 x (eat--t-disp-width disp))
                        (<= 1 y (eat--t-disp-height disp))
                        (or (eat--t-term-mouse-encoding terminal)
                            (and (<= x 95)
                                 (<= y 95)
                                 (<= button 95))))
               (if (eq (eat--t-term-mouse-mode terminal) 'x10)
                   (when (and (< button 3)
                              (or (memq 'click modifiers)
                                  (memq 'drag modifiers)))
                     (send
                      (if (eq (eat--t-term-mouse-encoding terminal)
                              'sgr)
                          (format "\e[<%i;%i;%iM" button x y)
                        (format "\e[M%c%c%c" (+ button 32) (+ x 32)
                                (+ y 32)))))
                 (cond
                  ;; `down-mouse-1' and friends.
                  ((memq 'down modifiers)
                   ;; For `mouse-1', `mouse-2' and `mouse-3', keep
                   ;; track the button's state, we'll need it when
                   ;; button event mouse mode is enabled.
                   (when (< (logand button 3) 3)
                     (setf (eat--t-term-mouse-pressed terminal)
                           ;; In XTerm and Kitty, mouse-1 is
                           ;; prioritized over mouse-2, and mouse-2
                           ;; over mouse-3.  However St doesn't keep
                           ;; track of multiple buttons.
                           (sort
                            (cons button (eat--t-term-mouse-pressed
                                          terminal))
                            #'<)))
                   (send
                    (if (eq (eat--t-term-mouse-encoding terminal)
                            'sgr)
                        (format "\e[<%i;%i;%iM" button x y)
                      (format "\e[M%c%c%c" (+ button 32) (+ x 32)
                              (+ y 32)))))
                  ;; `mouse-1', `mouse-2', `mouse-3', and their
                  ;; `drag'ged variants.
                  ((and (or (memq 'click modifiers)
                            (memq 'drag modifiers))
                        (<= mouse-num 3))
                   ;; For `mouse-1', `mouse-2' and `mouse-3', keep
                   ;; track the button's state, we'll need it when
                   ;; button event mouse mode is enabled.
                   (setf (eat--t-term-mouse-pressed terminal)
                         (cl-delete-if
                          (lambda (b)
                            (= (logand b 3) (logand button 3)))
                          (eat--t-term-mouse-pressed terminal)))
                   (send
                    (if (eq (eat--t-term-mouse-encoding terminal)
                            'sgr)
                        (format "\e[<%i;%i;%im" button x y)
                      (format "\e[M%c%c%c" (+ (logior button 3) 32)
                              (+ x 32) (+ y 32)))))
                  ;; Mouse wheel, `mouse-4' and friends.
                  (t
                   (send
                    (if (eq (eat--t-term-mouse-encoding terminal)
                            'sgr)
                        (format "\e[<%i;%i;%iM" button x y)
                      (format "\e[M%c%c%c" (+ button 32) (+ x 32)
                              (+ y 32))))))))))
          ;; Mouse movement tracking.
          ((and (guard (memq (eat--t-term-mouse-mode terminal)
                             '(button-event any-event)))
                (pred mouse-movement-p)
                movement)
           (let* ((pos (event-start movement))
                  (x-y (if (< emacs-major-version 29)
                           (posn-col-row pos)
                         (with-suppressed-warnings
                             ((callargs posn-col-row))
                           (posn-col-row pos 'use-window))))
                  (x (1+ (car x-y)))
                  (y (1+ (cdr x-y)))
                  (button
                   (if (car (eat--t-term-mouse-pressed terminal))
                       (+ (car (eat--t-term-mouse-pressed terminal))
                          32)
                     35)))
             (when ref-pos
               (let ((ref-x-y
                      (if (< emacs-major-version 29)
                          (posn-col-row ref-pos)
                        (with-suppressed-warnings
                            ((callargs posn-col-row))
                          (posn-col-row ref-pos 'use-window)))))
                 (cl-decf x (car ref-x-y))
                 (cl-decf y (cdr ref-x-y))))
             (when (and (or (eq (eat--t-term-mouse-mode terminal)
                                'any-event)
                            (/= button 35))
                        (<= 1 x (eat--t-disp-width disp))
                        (<= 1 y (eat--t-disp-height disp))
                        (or (eat--t-term-mouse-encoding terminal)
                            (and (<= x 95)
                                 (<= y 95)
                                 (<= button 95))))
               (send
                (if (eq (eat--t-term-mouse-encoding terminal)
                        'sgr)
                    (format "\e[<%i;%i;%iM" button x y)
                  (format "\e[M%c%c%c" (+ button 32) (+ x 32)
                          (+ y 32)))))))
          ;; Focus events.
          ('(eat-focus-in)
           (when (eat--t-term-focus-event-mode terminal)
             (send "\e[I")))
          ('(eat-focus-out)
           (when (eat--t-term-focus-event-mode terminal)
             (send "\e[O"))))))))

(defun eat-send-string-as-yank (terminal args)
  "Send ARGS to TERMINAL, honoring bracketed yank mode.

Each argument in ARGS can be either string or character."
  (funcall (eat--t-term-input-fn terminal) terminal
           (let ((str (mapconcat (lambda (s)
                                   (if (stringp s) s (string s)))
                                 args "")))
             (if (eat--t-term-bracketed-yank terminal)
                 ;; REVIEW: What if `str' itself contains these escape
                 ;; sequences?  St doesn't care and just wraps the
                 ;; string with these magic escape sequences, while
                 ;; Kitty tries to be smart.
                 (format "\e[200~%s\e[201~" str)
               str))))

(defun eat-term-make-keymap (input-command categories exceptions)
  "Make a keymap binding INPUT-COMMAND to the events of CATEGORIES.

CATEGORIES is a list whose elements should be a one of the following
keywords:

  `:ascii'              All self-insertable characters, plus
                        `backspace', `insert', `delete' and
                        `deletechar' keys, with all possible
                        modifiers.
  `:arrow'              Arrow keys with all possible modifiers.
  `:navigation'         Navigation keys: home, end, prior (or page up)
                        and next (or page down) with all possible
                        modifiers.
  `:function'           Function keys (f1 - f63).
  `:mouse-click'        `mouse-1', `mouse-2' and `mouse-3'.
  `:mouse-modifier'     All mouse events except mouse movement.
  `:mouse-movement'     Mouse movement.

EXCEPTIONS is a list of key sequences to not bind.  Don't use
\"M-...\" key sequences in EXCEPTIONS, use \"ESC ...\" instead."
  (let ((map (make-sparse-keymap)))
    (cl-flet ((bind (key)
                (unless (member key exceptions)
                  (define-key map key input-command))))
      (when (memq :ascii categories)
        ;; Bind ASCII and self-insertable characters except ESC and
        ;; DEL.
        (bind [remap self-insert-command])
        (cl-loop
         for i from ?\C-@ to ?~
         do (unless (= i meta-prefix-char)
              (bind (vector i))))
        ;; Bind `backspace', `delete', `deletechar', and all modified
        ;; variants.
        (dolist (key '( backspace C-backspace
                        insert C-insert M-insert S-insert C-M-insert
                        C-S-insert M-S-insert C-M-S-insert
                        delete C-delete M-delete S-delete C-M-delete
                        C-S-delete M-S-delete C-M-S-delete
                        deletechar C-deletechar M-deletechar
                        S-deletechar C-M-deletechar C-S-deletechar
                        M-S-deletechar C-M-S-deletechar))
          (bind (vector key)))
        ;; Bind these non-encodable keys.  They are translated.
        (dolist (key '(?\C-- ?\C-? ?\C-\s))
          (bind (vector key)))
        ;; Bind M-<ASCII> keys.
        (unless (member (vector meta-prefix-char) exceptions)
          (define-key map (vector meta-prefix-char)
                      (make-sparse-keymap))
          (cl-loop
           for i from ?\C-@ to ?~
           do (unless (memq i '(?O ?\[))
                (bind (vector meta-prefix-char i))))
          (bind (vector meta-prefix-char meta-prefix-char))))
      (when (memq :arrow categories)
        (dolist (key '( up down right left
                        C-up C-down C-right C-left
                        M-up M-down M-right M-left
                        S-up S-down S-right S-left
                        C-M-up C-M-down C-M-right C-M-left
                        C-S-up C-S-down C-S-right C-S-left
                        M-S-up M-S-down M-S-right M-S-left
                        C-M-S-up C-M-S-down C-M-S-right C-M-S-left))
          (bind (vector key))))
      (when (memq :navigation categories)
        (dolist (key '( home C-home M-home S-home C-M-home C-S-home
                        M-S-home C-M-S-home
                        end C-end M-end S-end C-M-end C-S-end
                        M-S-end C-M-S-end
                        prior C-prior M-prior S-prior C-M-prior
                        C-S-prior M-S-prior C-M-S-prior
                        next C-next M-next S-next C-M-next C-S-next
                        M-S-next C-M-S-next))
          (bind (vector key))))
      (when (memq :function categories)
        (cl-loop
         for i from 1 to 63
         do (let ((key (intern (format "f%i" i))))
              (bind (vector key)))))
      (when (memq :mouse-click categories)
        (dolist (key '(mouse-1 mouse-2 mouse-3))
          (bind (vector key))))
      (when (memq :mouse-modifier categories)
        (dolist (key
                 '( down-mouse-1 drag-mouse-1 down-mouse-2
                    drag-mouse-2 down-mouse-3 drag-mouse-3
                    C-down-mouse-1 C-drag-mouse-1 C-down-mouse-2
                    C-drag-mouse-2 C-down-mouse-3 C-drag-mouse-3
                    M-down-mouse-1 M-drag-mouse-1 M-down-mouse-2
                    M-drag-mouse-2 M-down-mouse-3 M-drag-mouse-3
                    S-down-mouse-1 S-drag-mouse-1 S-down-mouse-2
                    S-drag-mouse-2 S-down-mouse-3 S-drag-mouse-3
                    C-M-down-mouse-1 C-M-drag-mouse-1
                    C-M-down-mouse-2 C-M-drag-mouse-2
                    C-M-down-mouse-3 C-M-drag-mouse-3
                    C-S-down-mouse-1 C-S-drag-mouse-1
                    C-S-down-mouse-2 C-S-drag-mouse-2
                    C-S-down-mouse-3 C-S-drag-mouse-3
                    M-S-down-mouse-1 M-S-drag-mouse-1
                    M-S-down-mouse-2 M-S-drag-mouse-2
                    M-S-down-mouse-3 M-S-drag-mouse-3
                    C-M-S-down-mouse-1 C-M-S-drag-mouse-1
                    C-M-S-down-mouse-2 C-M-S-drag-mouse-2
                    C-M-S-down-mouse-3 C-M-S-drag-mouse-3 mouse-1
                    mouse-2 mouse-3 mouse-4 mouse-5 mouse-6 mouse-7
                    mouse-8 mouse-9 mouse-10 mouse-11 C-mouse-1
                    C-mouse-2 C-mouse-3 C-mouse-4 C-mouse-5
                    C-mouse-6 C-mouse-7 C-mouse-8 C-mouse-9
                    C-mouse-10 C-mouse-11 M-mouse-1 M-mouse-2
                    M-mouse-3 M-mouse-4 M-mouse-5 M-mouse-6
                    M-mouse-7 M-mouse-8 M-mouse-9 M-mouse-10
                    M-mouse-11 S-mouse-1 S-mouse-2 S-mouse-3
                    S-mouse-4 S-mouse-5 S-mouse-6 S-mouse-7
                    S-mouse-8 S-mouse-9 S-mouse-10 S-mouse-11
                    C-M-mouse-1 C-M-mouse-2 C-M-mouse-3 C-M-mouse-4
                    C-M-mouse-5 C-M-mouse-6 C-M-mouse-7 C-M-mouse-8
                    C-M-mouse-9 C-M-mouse-10 C-M-mouse-11
                    C-S-mouse-1 C-S-mouse-2 C-S-mouse-3 C-S-mouse-4
                    C-S-mouse-5 C-S-mouse-6 C-S-mouse-7 C-S-mouse-8
                    C-S-mouse-9 C-S-mouse-10 C-S-mouse-11
                    M-S-mouse-1 M-S-mouse-2 M-S-mouse-3 M-S-mouse-4
                    M-S-mouse-5 M-S-mouse-6 M-S-mouse-7 M-S-mouse-8
                    M-S-mouse-9 M-S-mouse-10 M-S-mouse-11
                    C-M-S-mouse-1 C-M-S-mouse-2 C-M-S-mouse-3
                    C-M-S-mouse-4 C-M-S-mouse-5 C-M-S-mouse-6
                    C-M-S-mouse-7 C-M-S-mouse-8 C-M-S-mouse-9
                    C-M-S-mouse-10 C-M-S-mouse-11 wheel-up
                    wheel-down wheel-right wheel-left C-wheel-up
                    C-wheel-down C-wheel-right C-wheel-left
                    M-wheel-up M-wheel-down M-wheel-right
                    M-wheel-left S-wheel-up S-wheel-down
                    S-wheel-right S-wheel-left C-M-wheel-up
                    C-M-wheel-down C-M-wheel-right C-M-wheel-left
                    C-S-wheel-up C-S-wheel-down C-S-wheel-right
                    C-S-wheel-left M-S-wheel-up M-S-wheel-down
                    M-S-wheel-right M-S-wheel-left C-M-S-wheel-up
                    C-M-S-wheel-down C-M-S-wheel-right
                    C-M-S-wheel-left))
          (bind (vector key))))
      (when (memq :mouse-movement categories)
        (bind [mouse-movement])))
    map))

(defun eat-term-name ()
  "Return the value of `TERM' environment variable for Eat."
  (if (stringp eat-term-name)
      eat-term-name
    (funcall eat-term-name)))

(defun eat-term-get-suitable-term-name (&optional display)
  "Return the most suitable value for `TERM' for DISPLAY.

If the number of colors supported by display (as returned by
`display-color-cells') is more than 256, return \"eat-truecolor\", if
it is more than 8 but less than or equal to 256, return
\"eat-256color\", if is more than 1 but less than or equal to 8,
return \"eat-color\", otherwise return \"eat-mono\"."
  (let ((colors (display-color-cells display)))
    (cond ((> colors 256) "eat-truecolor")
          ((> colors 8) "eat-256color")
          ((> colors 1) "eat-color")
          (t "eat-mono"))))

(defun eat-term-filter-string (string)
  "Filter Eat's special text properties from STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (not (eobp))
      (eat--t-join-long-line))
    (buffer-string)))


;;;; Blink mode.

(defvar eat--slow-blink-state nil
  "Current state of slowly blinking text, t means inverse video.")

(defvar eat--fast-blink-state nil
  "Current state of rapidly blinking text, t means inverse video.")

(defvar eat--slow-blink-remap nil
  "Face remapping cookie of slowly blinking face.")

(defvar eat--fast-blink-remap nil
  "Face remapping cookie of rapidly blinking face.")

(defvar eat--slow-blink-timer nil
  "Timer for blinking slowly blinking text.")

(defvar eat--fast-blink-timer nil
  "Timer for blinking rapidly blinking text.")

(declare-function face-remap-add-relative "face-remap"
                  (face &rest specs))
(declare-function face-remap-remove-relative "face-remap" (cookie))

(defun eat--flip-slow-blink-state ()
  "Flip the state of slowly blinking text."
  (face-remap-remove-relative eat--slow-blink-remap)
  (setq eat--slow-blink-remap
        (face-remap-add-relative
         'eat-slow-blink
         `(:box nil :inverse-video ,(not eat--slow-blink-state)))
        eat--slow-blink-state (not eat--slow-blink-state)))

(defun eat--flip-fast-blink-state ()
  "Flip the state of rapidly blinking text."
  (face-remap-remove-relative eat--fast-blink-remap)
  (setq eat--fast-blink-remap
        (face-remap-add-relative
         'eat-fast-blink
         `(:box nil :inverse-video ,(not eat--fast-blink-state)))
        eat--fast-blink-state (not eat--fast-blink-state)))

(defun eat--blink-stop-timers ()
  "Start blinking timers."
  (when eat--slow-blink-timer
    (cancel-timer eat--slow-blink-timer)
    (setq eat--slow-blink-timer nil))
  (when eat--fast-blink-timer
    (cancel-timer eat--fast-blink-timer)
    (setq eat--fast-blink-timer nil)))

(defun eat--blink-start-timers ()
  "Start blinking timers."
  (eat--blink-stop-timers)
  (setq eat--slow-blink-timer
        (run-with-timer t (/ (float eat-slow-blink-frequency))
                        #'eat--flip-slow-blink-state))
  (setq eat--fast-blink-timer
        (run-with-timer t (/ (float eat-fast-blink-frequency))
                        #'eat--flip-fast-blink-state)))

(define-minor-mode eat-blink-mode
  "Toggle blinking of text with blink attribute."
  :lighter " Eat-Blink"
  (let ((locals '( eat--slow-blink-state eat--fast-blink-state
                   eat--slow-blink-remap eat--fast-blink-remap
                   eat--slow-blink-timer eat--fast-blink-timer)))
    (cond
     (eat-blink-mode
      (setq eat-blink-mode nil)
      (require 'face-remap)
      (setq eat-blink-mode t)
      (mapc #'make-local-variable locals)
      (setq eat--slow-blink-state nil)
      (setq eat--fast-blink-state nil)
      (setq eat--slow-blink-remap
            (face-remap-add-relative 'eat-term-slow-blink
                                     '(:box nil)))
      (setq eat--fast-blink-remap
            (face-remap-add-relative 'eat-term-fast-blink
                                     '(:box nil)))
      (add-hook 'pre-command-hook #'eat--blink-stop-timers nil t)
      (add-hook 'post-command-hook #'eat--blink-start-timers nil t))
     (t
      (eat--blink-stop-timers)
      (face-remap-remove-relative eat--slow-blink-remap)
      (face-remap-remove-relative eat--fast-blink-remap)
      (remove-hook 'pre-command-hook #'eat--blink-stop-timers t)
      (remove-hook 'post-command-hook #'eat--blink-start-timers t)
      (mapc #'kill-local-variable locals)))))


;;;; Buffer-local Cursor Blinking.

(defvar eat--cursor-blink-type nil
  "Type of blinking cursor.")

(defvar eat--cursor-blink-state nil
  "Current state of slowly blinking text, non-nil means on.")

(defvar eat--cursor-blink-timer nil
  "Timer for blinking slowly blinking text.")

(defvar eat--cursor-blink-mode)

(defun eat--flip-cursor-blink-state ()
  "Flip the state of slowly blinking text."
  (when (and eat--cursor-blink-mode
             (display-graphic-p))
    (setq-local cursor-type (if eat--cursor-blink-state
                                (caddr eat--cursor-blink-type)
                              (car eat--cursor-blink-type)))
    (setq eat--cursor-blink-state (not eat--cursor-blink-state))
    ;; REVIEW: This is expensive, and some causes flickering.  Any
    ;; better way?
    (when-let* ((window (get-buffer-window nil 'visible)))
      (redraw-frame (window-frame window)))))

(defun eat--cursor-blink-stop-timers ()
  "Stop blinking timers."
  (unless eat--cursor-blink-state
    (eat--flip-cursor-blink-state))
  (when eat--cursor-blink-timer
    (cancel-timer eat--cursor-blink-timer)
    (setq eat--cursor-blink-timer nil)))

(defun eat--cursor-blink-start-timers ()
  "Start blinking timers."
  (eat--cursor-blink-stop-timers)
  (setq eat--cursor-blink-timer
        (run-with-timer t (/ (float (cadr eat--cursor-blink-type)))
                        #'eat--flip-cursor-blink-state)))

(define-minor-mode eat--cursor-blink-mode
  "Toggle blinking of cursor."
  :interactive nil
  (let ((locals '(eat--cursor-blink-state eat--cursor-blink-timer)))
    (cond
     (eat--cursor-blink-mode
      (mapc #'make-local-variable locals)
      (setq eat--cursor-blink-state nil)
      (setq eat--cursor-blink-timer nil)
      (add-hook 'pre-command-hook #'eat--cursor-blink-stop-timers nil
                t)
      (add-hook 'post-command-hook #'eat--cursor-blink-start-timers
                nil t)
      (when (current-idle-time)
        (eat--cursor-blink-start-timers)))
     (t
      (eat--cursor-blink-stop-timers)
      (remove-hook 'pre-command-hook #'eat--cursor-blink-stop-timers
                   t)
      (remove-hook 'post-command-hook #'eat--cursor-blink-start-timers
                   t)
      (mapc #'kill-local-variable locals)))))


;;;; User Interface.

(defvar eat--terminal nil
  "The terminal object.")

(defvar eat--synchronize-scroll-function nil
  "Function to synchronize scrolling between terminal and window.")

(defvar eat--shell-command-status 0
  "If the current shell command has finished, its exit status.")

(defvar eat--shell-prompt-begin nil
  "Beginning of last shell prompt.")

(defvar eat--shell-prompt-mark nil
  "Display property used to put a mark before the previous prompt.")

(defvar eat--shell-prompt-mark-overlays nil
  "List of overlay used to put marks before shell prompts.")

(defun eat-reset ()
  "Perform a terminal reset."
  (interactive)
  (when eat--terminal
    (let ((inhibit-read-only t))
      (eat-term-reset eat--terminal)
      (eat-term-redisplay eat--terminal))))

(defun eat--set-cursor (_ state)
  "Set cursor type according to STATE.

STATE can be one of the following:

`:default'            Default cursor.
`:invisible'          Invisible cursor.
`:very-visible'       Very visible cursor.  Can also be implemented as
                      blinking cursor.
Any other value     Default cursor."
  (setq-local eat--cursor-blink-type
              (pcase state
                (:invisible eat-invisible-cursor-type)
                (:very-visible eat-very-visible-cursor-type)
                (_ eat-default-cursor-type))) ; `:default'
  (setq-local cursor-type (car eat--cursor-blink-type))
  (eat--cursor-blink-mode (if (cadr eat--cursor-blink-type) +1 -1)))

(defun eat--manipulate-kill-ring (_ selection data)
  "Manipulate `kill-ring'.

SELECTION can be one of `:clipboard', `:primary', `:secondary',
`:select'.  When DATA is a string, set the selection to that string,
when DATA is nil, unset the selection, and when DATA is t, return the
selection, or nil if none."
  (let ((inhibit-eol-conversion t)
        (select-enable-clipboard (eq selection :clipboard))
        (select-enable-primary (eq selection :primary)))
    (pcase-exhaustive data
      ('t
       (when eat-enable-yank-to-terminal
         (ignore-error error
           (current-kill 0 'do-not-move))))
      ((and (pred stringp) str)
       (when eat-enable-kill-from-terminal
         (kill-new str))))))

(defun eat--bell (_)
  "Ring the bell."
  (ding t))

(defun eat--set-cwd (_ host cwd)
  "Set CWD as the current working directory (`default-directory').

If HOST isn't the host Emacs is running on, don't do anything."
  (when (and eat-enable-directory-tracking
             (string= host (system-name)))
    (ignore-errors
      (cd-absolute cwd))))

(defun eat--pre-prompt (_)
  "Save the beginning position of shell prompt."
  (setq eat--shell-prompt-begin (point-marker)))

(defun eat--post-prompt (_)
  "Put a mark in the marginal area on current line."
  (when eat-enable-shell-prompt-annotation
    (let ((indicator
           (if (zerop eat--shell-command-status)
               (propertize
                eat-shell-prompt-annotation-success-margin-indicator
                'face 'eat-shell-prompt-annotation-success)
             (propertize
              eat-shell-prompt-annotation-failure-margin-indicator
              'face 'eat-shell-prompt-annotation-failure))))
      ;; Update previous prompt's indicator using side-effect.
      (when eat--shell-prompt-mark
        (setf (cadr eat--shell-prompt-mark) indicator)
        (setq eat--shell-prompt-mark nil))
      ;; Show this prompt's indicator.
      (when eat--shell-prompt-begin
        (when (< eat--shell-prompt-begin (point))
          ;; Save it, we'll use side-effect.
          (setq eat--shell-prompt-mark
                `((margin ,eat-shell-prompt-annotation-position)
                  ,indicator))
          ;; Make overlay and put bookkeeping properties.
          (let ((identifier (gensym "eat--prompt-mark-identifier-"))
                (before-str
                 (propertize " " 'display eat--shell-prompt-mark))
                (ov (make-overlay eat--shell-prompt-begin
                                  (1+ eat--shell-prompt-begin))))
            (overlay-put ov 'before-string before-str)
            (overlay-put ov 'eat--shell-prompt-mark-id identifier)
            (add-text-properties
             eat--shell-prompt-begin (1+ eat--shell-prompt-begin)
             (list 'eat--before-string before-str
                   'eat--shell-prompt-mark-id identifier
                   'eat--shell-prompt-mark-overlay ov))
            (push ov eat--shell-prompt-mark-overlays)))
        (setq eat--shell-prompt-begin nil))))
  (when eat--shell-prompt-begin
    (when (< eat--shell-prompt-begin (point))
      ;; Put a text property to allow previous or next prompts.
      (put-text-property (1- (point)) (point)
                         'eat--shell-prompt-end t))))

(defun eat--correct-shell-prompt-mark-overlays (buffer)
  "Correct all overlays used to add mark before shell prompt.

BUFFER is the terminal buffer."
  (when (and (buffer-live-p buffer)
             (buffer-local-value 'eat--terminal buffer)
             eat-enable-shell-prompt-annotation)
    (with-current-buffer buffer
      (while-no-input
        ;; Delete all outdated overlays.
        (dolist (ov eat--shell-prompt-mark-overlays)
          (unless (eq (overlay-get ov 'eat--shell-prompt-mark-id)
                      (get-text-property (overlay-start ov)
                                         'eat--shell-prompt-mark-id))
            (delete-overlay ov)
            (setq eat--shell-prompt-mark-overlays
                  (delq ov eat--shell-prompt-mark-overlays))))
        (save-excursion
          ;; Recreate overlays if needed.
          (goto-char (eat-term-beginning eat--terminal))
          (while (< (point) (eat-term-end eat--terminal))
            (when (get-text-property
                   (point) 'eat--shell-prompt-mark-id)
              (let ((ov (get-text-property
                         (point) 'eat--shell-prompt-mark-overlay)))
                (unless
                    (and ov
                         (overlay-buffer ov)
                         (eq (overlay-get
                              ov 'eat--shell-prompt-mark-id)
                             (get-text-property
                              (point) 'eat--shell-prompt-mark-id)))
                  ;; Recreate.
                  (when ov
                    (delete-overlay ov)
                    (setq eat--shell-prompt-mark-overlays
                          (delq ov eat--shell-prompt-mark-overlays)))
                  (setq ov (make-overlay (point) (1+ (point))))
                  (overlay-put ov 'before-string
                               (get-text-property
                                (point) 'eat--before-string))
                  (overlay-put ov 'eat--shell-prompt-mark-id
                               (get-text-property
                                (point) 'eat--shell-prompt-mark-id))
                  (push ov eat--shell-prompt-mark-overlays))))
            (goto-char (or (next-single-property-change
                            (point) 'eat--shell-prompt-mark-id nil
                            (eat-term-end eat--terminal))
                           (eat-term-end eat--terminal)))))))))

(defun eat--set-cmd (_ cmd)
  "Add CMD to `shell-command-history'."
  (when eat-enable-shell-command-history
    (add-to-history 'shell-command-history cmd)))

(defun eat--pre-cmd (_)
  "Update shell prompt mark to indicate command is running."
  (when (and eat-enable-shell-prompt-annotation
             eat--shell-prompt-mark)
    (setf (cadr eat--shell-prompt-mark)
          (propertize
           eat-shell-prompt-annotation-running-margin-indicator
           'face 'eat-shell-prompt-annotation-running))))

(defun eat--set-cmd-status (_ code)
  "Set CODE as the current shell command's exit status."
  (when eat-enable-shell-prompt-annotation
    ;; We'll update the mark later when the prompt appears.
    (setq eat--shell-command-status code)))

(defun eat-previous-shell-prompt (&optional arg)
  "Go to the previous shell prompt.

When numeric prefix argument, ARG, is given, go to ARGth previous
shell prompt."
  (interactive "p")
  (dotimes (_ (or arg 1))
    (let ((previous (previous-single-property-change
                     (point) 'eat--shell-prompt-end)))
      (goto-char (or previous (point-min)))
      (when (get-text-property (point) 'eat--shell-prompt-end)
        (setq previous (previous-single-property-change
                        (point) 'eat--shell-prompt-end))
        (goto-char (or previous (point-min))))
      (unless previous
        (user-error "No previous prompt")))))

(defun eat-next-shell-prompt (&optional arg)
  "Go to the next shell prompt.

When numeric prefix argument, ARG, is given, go to ARGth next shell
prompt."
  (interactive "p")
  (dotimes (_ (or arg 1))
    (let ((next (next-single-property-change
                 (point) 'eat--shell-prompt-end)))
      (goto-char (or next (point-max)))
      (when (get-text-property (point) 'eat--shell-prompt-end)
        (goto-char (or (next-single-property-change
                        (point) 'eat--shell-prompt-end)
                       (point-max))))
      (unless next
        (user-error "No next prompt")))))


;;;;; Input.

(defvar eat--mouse-grabbing-type nil
  "Current mouse grabbing type/mode.")

(defvar eat--mouse-pressed-buttons nil
  "Mouse buttons currently pressed.")

(defvar eat--mouse-last-position nil
  "Last position of mouse, nil when not dragging.")

(defvar eat--mouse-drag-transient-map-exit nil
  "Function to exit mouse dragging transient map.")

(defun eat-self-input (n &optional e)
  "Send E as input N times.

N defaults to 1 and E defaults to `last-command-event' and should be a
event."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (if (and (> (length (this-command-keys)) 1)
                  (eq (aref (this-command-keys)
                            (- (length (this-command-keys)) 2))
                      meta-prefix-char))
             ;; HACK: Capture meta modifier (ESC prefix) in terminal.
             (cond
              ((eq last-command-event meta-prefix-char)
               last-command-event)
              ((characterp last-command-event)
               (aref
                (kbd (format "M-%c" last-command-event))
                0))
              ((symbolp last-command-event)
               (aref
                (kbd (format "M-<%S>" last-command-event))
                0))
              (t
               last-command-event))
           last-command-event)))
  (when (memq (event-basic-type e)
              '( mouse-1 mouse-2 mouse-3 mouse-4 mouse-5 mouse-6
                 mouse-7 mouse-8 mouse-9 mouse-10 mouse-11))
    (select-window (posn-window (event-start e))))
  (when eat--terminal
    (unless (mouse-movement-p e)
      (funcall eat--synchronize-scroll-function))
    (if (memq (event-basic-type e)
              '( mouse-1 mouse-2 mouse-3 mouse-4 mouse-5 mouse-6
                 mouse-7 mouse-8 mouse-9 mouse-10 mouse-11
                 mouse-movement))
        (let ((disp-begin-posn
               (posn-at-point
                (eat-term-display-beginning eat--terminal)))
              (e (if (or (not eat--mouse-last-position)
                         (eq (posn-window
                              (if (memq 'drag (event-modifiers e))
                                  (event-end e)
                                (event-start e)))
                             (posn-window eat--mouse-last-position)))
                     e
                   (pcase e
                     (`(,type ,_)
                      `(,type ,eat--mouse-last-position))
                     (`(,type ,start ,_)
                      `(,type ,start ,eat--mouse-last-position))
                     (ev ev)))))
          (if (not (mouse-movement-p e))
              (eat-term-input-event eat--terminal n e disp-begin-posn)
            (if (not eat--mouse-pressed-buttons)
                (when (eq eat--mouse-grabbing-type :all)
                  (eat-term-input-event eat--terminal n e
                                        disp-begin-posn))
              (when (memq eat--mouse-grabbing-type '(:all :drag))
                (eat-term-input-event eat--terminal n e
                                      disp-begin-posn))
              (setq eat--mouse-last-position (event-start e))))
          (when (memq (event-basic-type e) '(mouse-1 mouse-2 mouse-3))
            (when (or (memq 'click (event-modifiers e))
                      (memq 'drag (event-modifiers e)))
              (setq eat--mouse-pressed-buttons
                    (delq (event-basic-type e)
                          eat--mouse-pressed-buttons))
              (unless eat--mouse-pressed-buttons
                (setq eat--mouse-last-position nil)
                (when eat--mouse-drag-transient-map-exit
                  (funcall eat--mouse-drag-transient-map-exit)
                  (setq eat--mouse-drag-transient-map-exit nil))))
            (when (memq 'down (event-modifiers e))
              (push (event-basic-type e) eat--mouse-pressed-buttons)
              (setq eat--mouse-last-position (event-start e))
              (unless eat--mouse-drag-transient-map-exit
                (let ((old-track-mouse track-mouse)
                      (buffer (current-buffer)))
                  (setq track-mouse 'dragging)
                  (setq eat--mouse-drag-transient-map-exit
                        (set-transient-map
                         (let ((map (eat-term-make-keymap
                                     #'eat-self-input
                                     '(:mouse-modifier
                                       :mouse-movement)
                                     nil)))
                           ;; Some of the events will of course end up
                           ;; looked up with a mode-line, header-line
                           ;; or vertical-line prefix ...
                           (define-key map [mode-line] map)
                           (define-key map [header-line] map)
                           (define-key map [tab-line] map)
                           (define-key map [vertical-line] map)
                           ;; ... and some maybe even with a right- or
                           ;; bottom-divider prefix.
                           (define-key map [right-divider] map)
                           (define-key map [bottom-divider] map))
                         #'always
                         (lambda ()
                           (with-current-buffer buffer
                             (setq track-mouse
                                   old-track-mouse))))))))))
      (eat-term-input-event eat--terminal n e))))

(defun eat-quoted-input ()
  "Read a character and send it as INPUT."
  (declare (interactive-only "Use `eat-self-input' instead."))
  (interactive)
  ;; HACK: Quick hack to allow inputting `C-g'.  Any better way to do
  ;; this?
  (eat-self-input
   1 (let ((inhibit-quit t)
           ;; Don't trigger `quit' exiting this `let'.
           (quit-flag nil))
       (read-event))))

(defun eat-input-char (character count)
  "Input CHARACTER, COUNT times.

Interactively, ask for the character CHARACTER to input.  The numeric prefix
argument COUNT specifies how many times to insert CHARACTER."
  (declare (interactive-only "Use `eat-self-input' instead."))
  (interactive (list (read-char-by-name
                      "Insert character (Unicode name or hex): ")
                     (prefix-numeric-value current-prefix-arg)))
  (eat-self-input count character))

(defun eat-yank (&optional arg)
  "Same as `yank', but for Eat.

ARG is passed to `yank', which see."
  (interactive "*P")
  (when eat--terminal
    (funcall eat--synchronize-scroll-function)
    (eat-send-string-as-yank
     eat--terminal (let ((yank-hook yank-transform-functions))
                     (with-temp-buffer
                       (setq-local yank-transform-functions yank-hook)
                       (yank arg)
                       (buffer-string))))))

(defun eat-yank-from-kill-ring (string &optional arg)
  "Same as `yank-from-kill-ring', but for Eat.

STRING and ARG are passed to `yank-pop', which see."
  (interactive (list (read-from-kill-ring "Yank from kill-ring: ")
                     current-prefix-arg))
  (when eat--terminal
    (funcall eat--synchronize-scroll-function)
    (eat-send-string-as-yank
     eat--terminal (let ((yank-hook yank-transform-functions))
                     (with-temp-buffer
                       (setq-local yank-transform-functions yank-hook)
                       (yank-from-kill-ring string arg)
                       (buffer-string))))))

;; When changing these keymaps, be sure to update the manual, README
;; and commentary.
(defvar eat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?\M-d] #'eat-char-mode)
    (define-key map [?\C-c ?\C-j] #'eat-semi-char-mode)
    (define-key map [?\C-c ?\C-k] #'eat-kill-process)
    (define-key map [?\C-c ?\C-p] #'eat-previous-shell-prompt)
    (define-key map [?\C-c ?\C-n] #'eat-next-shell-prompt)
    map)
  "Keymap for Eat mode.")

(defvar eat-semi-char-mode-map
  (let ((map (eat-term-make-keymap
              #'eat-self-input
              '(:ascii :arrow :navigation)
              '( [?\C-\\] [?\C-q] [?\C-c] [?\C-x] [?\C-g] [?\C-h]
                 [?\e ?\C-c] [?\C-u] [?\C-q] [?\e ?x] [?\e ?:]
                 [?\e ?!] [?\e ?&] [?\C-y] [?\e ?y]))))
    (define-key map [?\C-q] #'eat-quoted-input)
    (define-key map [?\C-y] #'eat-yank)
    (define-key map [?\M-y] #'eat-yank-from-kill-ring)
    (define-key map [?\C-c ?\C-c] #'eat-self-input)
    (define-key map [?\C-c ?\C-e] #'eat-emacs-mode)
    (define-key map [remap insert-char] #'eat-input-char)
    map)
  "Keymap for Eat semi-char mode.")

(defvar eat-char-mode-map
  (let ((map (eat-term-make-keymap
              #'eat-self-input
              '(:ascii :arrow :navigation :function)
              '([?\e ?\C-m]))))
    (define-key map [?\C-\M-m] #'eat-semi-char-mode)
    map)
  "Keymap for Eat char mode.")

(defvar eat--mouse-click-mode-map
  (eat-term-make-keymap #'eat-self-input '(:mouse-click) nil)
  "Keymap for `eat--mouse-click-mode'.")

(defvar eat--mouse-modifier-click-mode-map
  (eat-term-make-keymap #'eat-self-input '(:mouse-modifier) nil)
  "Keymap for `eat--mouse-modifier-click-mode'.")

(defvar eat--mouse-movement-mode-map
  (eat-term-make-keymap #'eat-self-input '(:mouse-movement) nil)
  "Keymap for `eat--mouse-movement-mode'.")

(define-minor-mode eat--semi-char-mode
  "Minor mode for semi-char mode keymap."
  :interactive nil
  :keymap eat-semi-char-mode-map)

(define-minor-mode eat--char-mode
  "Minor mode for char mode keymap."
  :interactive nil
  :keymap eat-char-mode-map)

(define-minor-mode eat--mouse-click-mode
  "Minor mode for mouse click keymap."
  :interactive nil)

(define-minor-mode eat--mouse-modifier-click-mode
  "Minor mode for mouse click with modifiers keymap."
  :interactive nil)

(define-minor-mode eat--mouse-movement-mode
  "Minor mode for mouse movement keymap."
  :interactive nil)

(defun eat-emacs-mode ()
  "Switch to Emacs keybindings mode."
  (interactive)
  (eat--semi-char-mode -1)
  (eat--char-mode -1)
  (setq buffer-read-only t)
  (eat--grab-mouse nil eat--mouse-grabbing-type)
  (force-mode-line-update))

(defun eat-semi-char-mode ()
  "Switch to semi-char mode."
  (interactive)
  (unless eat--terminal
    (error "Process not running"))
  (setq buffer-read-only nil)
  (eat--char-mode -1)
  (eat--semi-char-mode +1)
  (eat--grab-mouse nil eat--mouse-grabbing-type)
  (force-mode-line-update))

(defun eat-char-mode ()
  "Switch to char mode."
  (interactive)
  (unless eat--terminal
    (error "Process not running"))
  (setq buffer-read-only nil)
  (eat--semi-char-mode -1)
  (eat--char-mode +1)
  (eat--grab-mouse nil eat--mouse-grabbing-type)
  (force-mode-line-update))

(defvar eat--eshell-semi-char-mode)
(defvar eat--eshell-char-mode)

(defun eat--grab-mouse (_ mode)
  "Grab mouse.

MODE should one of:

  nil                 Disable mouse.
  `:click'              Pass `mouse-1', `mouse-2', and `mouse-3'
                        clicks.
  `:modifier-click'     Pass all mouse clicks, including control,
                        meta and shift modifiers.
  `:drag'               All of :modifier-click, plus dragging
                        (moving mouse while pressed) information.
  `:all'                Pass all mouse events, including movement.
  Any other value     Disable mouse."
  (setq eat--mouse-grabbing-type mode)
  (pcase (and eat-enable-mouse
              (or eat--semi-char-mode
                  eat--char-mode
                  eat--eshell-semi-char-mode
                  eat--eshell-char-mode)
              mode)
    (:all
     (setq track-mouse t)
     (eat--mouse-click-mode -1)
     (eat--mouse-modifier-click-mode +1)
     (eat--mouse-movement-mode +1))
    ((or :modifier-click :drag)
     (setq track-mouse nil)
     (eat--mouse-click-mode -1)
     (eat--mouse-movement-mode -1)
     (eat--mouse-modifier-click-mode +1))
    (:click
     (setq track-mouse nil)
     (eat--mouse-modifier-click-mode -1)
     (eat--mouse-movement-mode -1)
     (eat--mouse-click-mode +1))
    (_
     (setq track-mouse nil)
     (eat--mouse-click-mode -1)
     (eat--mouse-modifier-click-mode -1)
     (eat--mouse-movement-mode -1))))


;;;;; Major Mode.

(defun eat--synchronize-scroll ()
  "Synchronize scrolling and point between terminal and window."
  (when-let* ((window (get-buffer-window (current-buffer))))
    (set-window-start
     window (eat-term-display-beginning eat--terminal))
    (set-window-point
     window (eat-term-display-cursor eat--terminal)))
  (goto-char (eat-term-display-cursor eat--terminal)))

(defun eat--setup-glyphless-chars ()
  "Setup the display of glyphless characters."
  (setq-local glyphless-char-display
              (copy-sequence (default-value 'glyphless-char-display)))
  (set-char-table-extra-slot
   glyphless-char-display 0
   (if (display-graphic-p) 'empty-box 'thin-space)))

(defun eat--filter-buffer-substring (begin end &optional delete)
  "Filter buffer substring from BEGIN to END and return that.

When DELETE is given and non-nil, delete the text between BEGIN and
END if it's safe to do so."
  (let ((str (eat-term-filter-string (buffer-substring begin end))))
    (when (and delete
               (or (not eat--terminal)
                   (and (<= (eat-term-end eat--terminal) begin)
                        (<= (eat-term-end eat--terminal) end))
                   (and (<= begin (eat-term-beginning eat--terminal))
                        (<= end (eat-term-beginning eat--terminal)))))
      (delete-region begin end))
    str))

(define-derived-mode eat-mode fundamental-mode "Eat"
  "Major mode for Eat."
  :group 'eat-ui
  (mapc #'make-local-variable
        '(buffer-read-only
          buffer-undo-list
          filter-buffer-substring-function
          mode-line-process
          mode-line-buffer-identification
          glyphless-char-display
          cursor-type
          track-mouse
          eat--terminal
          eat--process
          eat--synchronize-scroll-function
          eat--mouse-grabbing-type
          eat--shell-command-status
          eat--shell-prompt-begin
          eat--shell-prompt-mark
          eat--shell-prompt-mark-overlays
          eat--pending-output-chunks
          eat--output-queue-first-chunk-time
          eat--process-output-queue-timer
          eat--shell-prompt-annotation-correction-timer))
  ;; This is intended; input methods don't work on read-only buffers.
  (setq buffer-read-only nil)
  (setq buffer-undo-list t)
  (setq eat--synchronize-scroll-function #'eat--synchronize-scroll)
  (setq filter-buffer-substring-function
        #'eat--filter-buffer-substring)
  (setq eat--mouse-grabbing-type nil)
  (setq mode-line-process
        '(""
          (:eval
           (when eat--process
             (cond
              (eat--semi-char-mode
               '("["
                 (:propertize
                  "semi-char"
                  help-echo "mouse-1: Switch to char mode, \
mouse-3: Switch to emacs mode"
                  mouse-face mode-line-highlight
                  local-map
                  (keymap
                   (mode-line
                    . (keymap
                       (down-mouse-1 . eat-char-mode)
                       (down-mouse-3 . eat-emacs-mode)))))
                 "]"))
              (eat--char-mode
               '("["
                 (:propertize
                  "char"
                  help-echo "mouse-1: Switch to semi-char mode, \
mouse-3: Switch to emacs mode"
                  mouse-face mode-line-highlight
                  local-map
                  (keymap
                   (mode-line
                    . (keymap
                       (down-mouse-1 . eat-semi-char-mode)
                       (down-mouse-3 . eat-emacs-mode)))))
                 "]"))
              (t
               '("["
                 (:propertize
                  "emacs"
                  help-echo "mouse-1: Switch to semi char mode, \
mouse-3: Switch to char mode"
                  mouse-face mode-line-highlight
                  local-map
                  (keymap
                   (mode-line
                    . (keymap
                       (down-mouse-1 . eat-semi-char-mode)
                       (down-mouse-3 . eat-char-mode)))))
                 "]")))))
          ":%s"))
  (setq mode-line-buffer-identification
        `(12 (""
              ,(nconc
                (propertized-buffer-identification "%b")
                '(" "
                  (:propertize
                   (:eval
                    (when (and eat--terminal
                               (not (string-empty-p (eat-term-title
                                                     eat--terminal))))
                      (format "(%s)" (eat-term-title eat--terminal))))
                   help-echo "Title"))))))
  (eat-emacs-mode)
  ;; Make sure glyphless character don't display a huge box glyph,
  ;; that would break the display.
  (eat--setup-glyphless-chars)
  (when eat-enable-blinking-text
    (eat-blink-mode +1))
  (when eat-enable-shell-prompt-annotation
    (let ((margin-width
           (max
            (string-width
             eat-shell-prompt-annotation-running-margin-indicator)
            (string-width
             eat-shell-prompt-annotation-success-margin-indicator)
            (string-width
             eat-shell-prompt-annotation-failure-margin-indicator))))
      (pcase-exhaustive eat-shell-prompt-annotation-position
        ('left-margin
         (setq left-margin-width margin-width))
        ('right-margin
         (setq right-margin-width margin-width))))
    ;; Make sure the marginal area is resized.
    (dolist (win (get-buffer-window-list))
      (set-window-buffer win (current-buffer)))))


;;;;; Process Handling.

(defvar eat--process nil
  "The running process.")

(defvar eat--pending-output-chunks nil
  "The list of pending output chunks.

The output chunks are pushed, so last output appears first.")

(defvar eat--output-queue-first-chunk-time nil
  "Time when the first chunk in the current output queue was pushed.")

(defvar eat--process-output-queue-timer nil
  "Timer to process output queue.")

(defvar eat--shell-prompt-annotation-correction-timer nil
  "Timer to correct shell prompt annotations.")

(defun eat-kill-process ()
  "Kill Eat process in current buffer."
  (interactive)
  (when eat--process
    (kill-process eat--process)))

(defun eat--send-string (process string)
  "Send to PROCESS the contents of STRING as input.

This is equivalent to `process-send-string', except that long input
strings are broken up into chunks of size `eat-input-chunk-size'.
Processes are given a chance to output between chunks.  This can help
prevent processes from hanging when you send them long inputs on some
OS's."
  (let ((i 0)
        (j eat-input-chunk-size)
        (l (length string)))
    (while (< i l)
      (process-send-string process (substring string i (min j l)))
      (accept-process-output)
      (cl-incf i eat-input-chunk-size)
      (cl-incf j eat-input-chunk-size))))

(defun eat--send-input (_ input)
  "Send INPUT to subprocess."
  (when eat--process
    (eat--send-string eat--process input)))

(defun eat--process-output-queue (buffer)
  "Process the output queue on BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-quit t)            ; Don't disturb!
            (inhibit-read-only t)
            (inhibit-modification-hooks t)
            (synchronize-scroll
             (or (= (eat-term-display-cursor eat--terminal) (point))
                 eat--char-mode)))
        (when eat--process-output-queue-timer
          (cancel-timer eat--process-output-queue-timer))
        (setq eat--output-queue-first-chunk-time nil)
        (let ((queue eat--pending-output-chunks))
          (setq eat--pending-output-chunks nil)
          (dolist (output (nreverse queue))
            (eat-term-process-output eat--terminal output)))
        (eat-term-redisplay eat--terminal)
        ;; Truncate output of previous dead processes.
        (when (and eat-term-scrollback-size
                   (< eat-term-scrollback-size
                      (- (point) (point-min))))
          (delete-region
           (point-min)
           (max (point-min)
                (- (eat-term-display-beginning eat--terminal)
                   eat-term-scrollback-size))))
        (setq eat--shell-prompt-annotation-correction-timer
              (run-with-timer
               eat-shell-prompt-annotation-correction-delay
               nil #'eat--correct-shell-prompt-mark-overlays
               buffer))
        (when synchronize-scroll
          (funcall eat--synchronize-scroll-function))))))

(defun eat--filter (process output)
  "Handle OUTPUT from PROCESS."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (when eat--process-output-queue-timer
        (cancel-timer eat--process-output-queue-timer))
      (when eat--shell-prompt-annotation-correction-timer
        (cancel-timer eat--shell-prompt-annotation-correction-timer))
      (unless eat--output-queue-first-chunk-time
        (setq eat--output-queue-first-chunk-time (current-time)))
      (push output eat--pending-output-chunks)
      (let ((time-left
             (- eat-maximum-latency
                (float-time
                 (time-subtract
                  nil eat--output-queue-first-chunk-time)))))
        (if (<= time-left 0)
            (eat--process-output-queue (current-buffer))
          (setq eat--process-output-queue-timer
                (run-with-timer
                 (min time-left eat-minimum-latency) nil
                 #'eat--process-output-queue (current-buffer))))))))

(defun eat--sentinel (process message)
  "Sentinel for Eat buffers.

PROCESS is the process and MESSAGE is the description of what happened
to it."
  (let ((buffer (process-buffer process)))
    (when (memq (process-status process) '(signal exit))
      (if (buffer-live-p buffer)
          (if eat-kill-buffer-on-exit
              (kill-buffer buffer)
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (when eat--process-output-queue-timer
                  (cancel-timer eat--process-output-queue-timer)
                  (setq eat--process-output-queue-timer nil))
                (eat--process-output-queue buffer)
                (when eat--shell-prompt-annotation-correction-timer
                  (cancel-timer
                   eat--shell-prompt-annotation-correction-timer)
                  (setq eat--shell-prompt-annotation-correction-timer
                        nil))
                (when eat-enable-shell-prompt-annotation
                  (eat--correct-shell-prompt-mark-overlays buffer)
                  (setq eat--shell-command-status 0)
                  (setq eat--shell-prompt-begin nil)
                  (setq eat--shell-prompt-mark nil)
                  (setq eat--shell-prompt-mark-overlays nil))
                (eat-emacs-mode)
                (setq eat--process nil)
                (delete-process process)
                (eat-term-delete eat--terminal)
                (setq eat--terminal nil)
                (eat--set-cursor nil :default)
                (eat--grab-mouse nil nil)
                (goto-char (point-max))
                (insert "\nProcess " (process-name process) " "
                        message)
                (setq buffer-read-only nil))))
        (set-process-buffer process nil)))))

(defun eat--adjust-process-window-size (process windows)
  "Resize process window and terminal.  Return new dimensions.

PROCESS is the process whose window to resize, and WINDOWS is the list
of window displaying PROCESS's buffer."
  (let ((size (funcall window-adjust-process-window-size-function
                       process windows)))
    (when size
      (let ((width (max (car size) 1))
            (height (max (cdr size) 1))
            (inhibit-read-only t)
            (synchronize-scroll
             (or (= (eat-term-display-cursor eat--terminal) (point))
                 eat--char-mode)))
        (eat-term-resize eat--terminal width height)
        (eat-term-redisplay eat--terminal)
        (when synchronize-scroll
          (funcall eat--synchronize-scroll-function))))
    size))

;; Adapted from Term.
(defun eat-exec (buffer name command startfile switches)
  "Start up a process in BUFFER for Eat mode.

Run COMMAND with SWITCHES.  Set NAME as the name of the process.
Blast any old process running in the buffer.  Don't set the buffer
mode.  You can use this to cheaply run a series of processes in the
same Eat buffer.  The hook `eat-exec-hook' is run after each exec."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (when eat--process
        (let ((eat-kill-buffer-on-exit nil))
          (delete-process eat--process)))
      ;; Ensure final newline.
      (goto-char (point-max))
      (unless (or (= (point-min) (point-max))
                  (= (char-before (point-max)) ?\n))
        (insert ?\n))
      (unless (= (point-min) (point-max))
        (insert "\n\n"))
      (setq eat--terminal (eat-term-make buffer (point)))
      (eat-semi-char-mode)
      (when-let* ((window (get-buffer-window nil t)))
        (with-selected-window window
          (eat-term-resize eat--terminal (window-max-chars-per-line)
                           (floor (window-screen-lines)))))
      (setf (eat-term-input-function eat--terminal) #'eat--send-input)
      (setf (eat-term-set-cursor-function eat--terminal)
            #'eat--set-cursor)
      (setf (eat-term-grab-mouse-function eat--terminal)
            #'eat--grab-mouse)
      (setf (eat-term-manipulate-selection-function eat--terminal)
            #'eat--manipulate-kill-ring)
      (setf (eat-term-ring-bell-function eat--terminal) #'eat--bell)
      (setf (eat-term-set-cwd-function eat--terminal) #'eat--set-cwd)
      (setf (eat-term-prompt-start-function eat--terminal)
            #'eat--pre-prompt)
      (setf (eat-term-prompt-end-function eat--terminal)
            #'eat--post-prompt)
      (setf (eat-term-set-cmd-function eat--terminal) #'eat--set-cmd)
      (setf (eat-term-cmd-start-function eat--terminal)
            #'eat--pre-cmd)
      (setf (eat-term-cmd-finish-function eat--terminal)
            #'eat--set-cmd-status)
      ;; Crank up a new process.
      (let* ((size (eat-term-size eat--terminal))
             (process-environment
              (nconc
               (list
                (concat "TERM=" (eat-term-name))
                (concat "TERMINFO=" eat-term-terminfo-directory)
                (concat "INSIDE_EMACS=" eat-term-inside-emacs)
                (concat "EAT_SHELL_INTEGRATION_DIR="
                        eat-term-shell-integration-directory))
               process-environment))
             (process-connection-type t)
             ;; We should suppress conversion of end-of-line format.
             (inhibit-eol-conversion t)
             (process
              (make-process
               :name name
               :buffer buffer
               :command `("/usr/bin/env" "sh" "-c"
                          ,(format "stty -nl echo rows %d columns \
%d sane 2>%s ; if [ $1 = .. ]; then shift; fi; exec \"$@\""
                                   (cdr size) (car size)
                                   null-device)
                          ".."
                          ,command ,@switches)
               :filter #'eat--filter
               :sentinel #'eat--sentinel
               :file-handler t)))
        (process-put process 'adjust-window-size-function
                     #'eat--adjust-process-window-size)
        ;; Jump to the end, and set the process mark.
        (goto-char (point-max))
        (set-marker (process-mark process) (point))
        (setq eat--process process)
        ;; Feed it the startfile.
        (when startfile
          ;;This is guaranteed to wait long enough
          ;;but has bad results if the shell does not prompt at all
          ;;         (while (= size (buffer-size))
          ;;           (sleep-for 1))
          ;;I hope 1 second is enough!
          (sleep-for 1)
          (goto-char (point-max))
          (insert-file-contents startfile)
          (process-send-string
           process (delete-and-extract-region (point) (point-max)))))
      (eat-term-redisplay eat--terminal)
      (run-hooks 'eat-exec-hook)
      buffer)))


;;;;; Entry Points.

(defun eat-make (name program &optional startfile &rest switches)
  "Make a Eat process NAME in a buffer, running PROGRAM.

The name of the buffer is made by surrounding NAME with `*'s.  If
there is already a running process in that buffer, it is not
restarted.  Optional third arg STARTFILE is the name of a file to send
the contents of to the process.  SWITCHES are the arguments to
PROGRAM."
  (let ((buffer (get-buffer-create (concat "*" name "*"))))
    ;; If no process, or nuked process, crank up a new one and put
    ;; buffer in Eat mode.  Otherwise, leave buffer and existing
    ;; process alone.
    (when (not (let ((proc (get-buffer-process buffer)))
                 (and proc (memq (process-status proc)
                                 '(run stop open listen connect)))))
      (with-current-buffer buffer
        (eat-mode))
      (eat-exec buffer name program startfile switches))
    buffer))

;;;###autoload
(defun eat (&optional program arg)
  "Start a new Eat terminal emulator in a buffer.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG (like \\[universal-argument] 42 \\[eshell]),
switch to the session with that number, or create it if it doesn't
already exist.

PROGRAM can be a shell command."
  (interactive (list (read-shell-command "Run program: "
                                         (or explicit-shell-file-name
                                             (getenv "ESHELL")
                                             shell-file-name))
                     current-prefix-arg))
  (let ((program (or program (or explicit-shell-file-name
                                 (getenv "ESHELL")
                                 shell-file-name)))
        (buffer
         (cond
          ((numberp arg)
	   (get-buffer-create (format "%s<%d>" eat-buffer-name arg)))
	  (arg
	   (generate-new-buffer eat-buffer-name))
	  (t
	   (get-buffer-create eat-buffer-name)))))
    (with-current-buffer buffer
      (unless (eq major-mode #'eat-mode)
        (eat-mode))
      (pop-to-buffer-same-window buffer)
      (unless eat--process
        (eat-exec buffer "eat" "/usr/bin/env" nil
                  `("sh" "-c" ,program)))
      buffer)))


;;;; Eshell integration.

;;;;; Input.

;; When changing these keymaps, be sure to update the manual, README
;; and commentary.
(defvar eat-eshell-emacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?\C-j] #'eat-eshell-semi-char-mode)
    (define-key map [remap eshell-toggle-direct-send] ; C-c M-d
                #'eat-eshell-char-mode)
    map)
  "Keymap for Eat Eshell \"emacs\" mode.")

(defvar eat-eshell-semi-char-mode-map
  (let ((map (eat-term-make-keymap
              #'eat-self-input
              '(:ascii :arrow :navigation)
              '( [?\C-\\] [?\C-q] [?\C-c] [?\C-x] [?\C-g] [?\C-h]
                 [?\e ?\C-c] [?\C-u] [?\C-q] [?\e ?x] [?\e ?:]
                 [?\e ?!] [?\e ?&] [?\C-y] [?\e ?y]))))
    (define-key map [?\C-q] #'eat-quoted-input)
    (define-key map [?\C-y] #'eat-yank)
    (define-key map [?\M-y] #'eat-yank-from-kill-ring)
    (define-key map [?\C-c ?\C-e] #'eat-eshell-emacs-mode)
    (define-key map [remap insert-char] #'eat-input-char)
    map)
  "Keymap for Eat Eshell semi-char mode.")

(defvar eat-eshell-char-mode-map
  (let ((map (eat-term-make-keymap
              #'eat-self-input
              '(:ascii :arrow :navigation :function)
              '([?\e ?\C-m]))))
    (define-key map [?\C-\M-m] #'eat-eshell-semi-char-mode)
    map)
  "Keymap for Eat Eshell char mode.")

(define-minor-mode eat--eshell-process-running-mode
  "Minor mode for \"emacs\" mode keymap when process is running."
  :interactive nil
  :keymap eat-eshell-emacs-mode-map)

(define-minor-mode eat--eshell-semi-char-mode
  "Minor mode for semi-char mode keymap."
  :interactive nil
  :keymap eat-eshell-semi-char-mode-map
  ;; HACK: Some keys like `C-c' are overriden by other keymaps
  ;; (possibly by the keymaps of other minor modes), so we also put
  ;; the keymap to `minor-mode-overriding-map-alist' to make Emacs
  ;; prioritize us.
  (setq minor-mode-overriding-map-alist
        (delete (cons #'eat--eshell-semi-char-mode
                      eat-eshell-semi-char-mode-map)
                minor-mode-overriding-map-alist))
  (when eat--eshell-semi-char-mode
    (push (cons #'eat--eshell-semi-char-mode
                eat-eshell-semi-char-mode-map)
          minor-mode-overriding-map-alist)))

(define-minor-mode eat--eshell-char-mode
  "Minor mode for char mode keymap."
  :interactive nil
  :keymap eat-eshell-char-mode-map
  ;; HACK: Some keys like `C-c' are overriden by other keymaps
  ;; (possibly by the keymaps of other minor modes), so we also put
  ;; the keymap to `minor-mode-overriding-map-alist' to make Emacs
  ;; prioritize us.
  (setq minor-mode-overriding-map-alist
        (delete (cons #'eat--eshell-char-mode
                      eat-eshell-char-mode-map)
                minor-mode-overriding-map-alist))
  (when eat--eshell-char-mode
    (push (cons #'eat--eshell-char-mode eat-eshell-char-mode-map)
          minor-mode-overriding-map-alist)))

(defun eat-eshell-emacs-mode ()
  "Switch to Emacs keybindings mode."
  (interactive)
  (eat--eshell-semi-char-mode -1)
  (eat--eshell-char-mode -1)
  (setq buffer-read-only t)
  (eat--grab-mouse nil eat--mouse-grabbing-type)
  (force-mode-line-update))

(defun eat-eshell-semi-char-mode ()
  "Switch to semi-char mode."
  (interactive)
  (when eat--terminal
    (setq buffer-read-only nil)
    (eat--eshell-char-mode -1)
    (eat--eshell-semi-char-mode +1)
    (eat--grab-mouse nil eat--mouse-grabbing-type)
    (force-mode-line-update)))

(defun eat-eshell-char-mode ()
  "Switch to char mode."
  (interactive)
  (when eat--terminal
    (setq buffer-read-only nil)
    (eat--eshell-semi-char-mode -1)
    (eat--eshell-char-mode +1)
    (eat--grab-mouse nil eat--mouse-grabbing-type)
    (force-mode-line-update)))


;;;;; Process Handling.

(defvar eat--eshell-invocation-directory nil
  "The directory from where the current process was started.")

(defvar eshell-last-output-start) ; In `esh-mode'.
(defvar eshell-last-output-end) ; In `esh-mode'.

(defun eat--eshell-term-name (&rest _)
  "Return the value of `TERM' environment variable for Eshell."
  (eat-term-name))

(defun eat--eshell-output-filter ()
  "Handle output from subprocess."
  (let ((inhibit-quit t)            ; Don't disturb!
        (inhibit-read-only t)
        (str (buffer-substring-no-properties
              eshell-last-output-start
              eshell-last-output-end)))
    (delete-region eshell-last-output-start eshell-last-output-end)
    (let ((synchronize-scroll
           (or (= (eat-term-display-cursor eat--terminal) (point))
               eat--eshell-char-mode)))
      (eat-term-process-output eat--terminal str)
      (eat-term-redisplay eat--terminal)
      (when synchronize-scroll
        (funcall eat--synchronize-scroll-function)))
    (let ((end (eat-term-end eat--terminal)))
      (set-marker eshell-last-output-start end)
      (set-marker eshell-last-output-end end)
      (set-marker (process-mark eat--process) end))))

(defun eat--eshell-setup-proc-and-term (proc)
  "Setup process PROC and a new terminal for it."
  (unless (or eat--terminal eat--process)
    (setq eat--process proc)
    (process-put proc 'adjust-window-size-function
                 #'eat--adjust-process-window-size)
    (setq eat--terminal (eat-term-make (current-buffer)
                                       (process-mark proc)))
    (set-marker (process-mark proc) (eat-term-end eat--terminal))
    (setf (eat-term-input-function eat--terminal) #'eat--send-input)
    (setf (eat-term-set-cursor-function eat--terminal)
          #'eat--set-cursor)
    (setf (eat-term-grab-mouse-function eat--terminal)
          #'eat--grab-mouse)
    (setf (eat-term-manipulate-selection-function eat--terminal)
          #'eat--manipulate-kill-ring)
    (setf (eat-term-ring-bell-function eat--terminal) #'eat--bell)
    (setf (eat-term-set-cwd-function eat--terminal) #'eat--set-cwd)
    (setf (eat-term-set-cmd-function eat--terminal) #'eat--set-cmd)
    (when-let* ((window (get-buffer-window nil t)))
      (with-selected-window window
        (eat-term-resize eat--terminal (window-max-chars-per-line)
                         (floor (window-screen-lines)))))
    (eat-term-redisplay eat--terminal)
    (setq-local eshell-output-filter-functions
                '(eat--eshell-output-filter))
    (eat--eshell-process-running-mode +1)
    (eat-eshell-semi-char-mode)))

(defun eat--eshell-cleanup ()
  "Cleanup everything."
  (when eat--terminal
    (let ((inhibit-read-only t))
      (cd-absolute eat--eshell-invocation-directory)
      (goto-char (eat-term-end eat--terminal))
      (unless (or (= (point) (point-min))
                  (= (char-before) ?\n))
        (insert ?\n))
      (set-marker eshell-last-output-start (point))
      (set-marker eshell-last-output-end (point))
      (eat--cursor-blink-mode -1)
      (eat--grab-mouse nil nil)
      (eat-term-delete eat--terminal)
      (set-process-filter eat--process #'eshell-output-filter)
      (setq eat--process nil)
      (setq eat--terminal nil)
      (kill-local-variable 'eshell-output-filter-functions)
      (eat--eshell-semi-char-mode -1)
      (eat--eshell-char-mode -1)
      (eat--eshell-process-running-mode -1)
      (setq buffer-read-only nil))))

(declare-function eshell-output-filter "esh-mode" (process string))

(defun eat--eshell-process-output-queue (process buffer)
  "Process the output queue on BUFFER from PROCESS."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when eat--process-output-queue-timer
        (cancel-timer eat--process-output-queue-timer))
      (setq eat--output-queue-first-chunk-time nil)
      (let ((queue eat--pending-output-chunks))
        (setq eat--pending-output-chunks nil)
        (combine-change-calls
            (eat-term-beginning eat--terminal)
            (eat-term-end eat--terminal)
          ;; TODO: Is `string-join' OK or should we use a loop?
          (eshell-output-filter
           process (string-join (nreverse queue))))))))

(defun eat--eshell-filter (process string)
  "Process output STRING from PROCESS."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (when eat--process-output-queue-timer
        (cancel-timer eat--process-output-queue-timer))
      (unless eat--output-queue-first-chunk-time
        (setq eat--output-queue-first-chunk-time (current-time)))
      (push string eat--pending-output-chunks)
      (let ((time-left
             (- eat-maximum-latency
                (float-time
                 (time-subtract
                  nil eat--output-queue-first-chunk-time)))))
        (if (<= time-left 0)
            (eat--eshell-process-output-queue
             process (current-buffer))
          (setq eat--process-output-queue-timer
                (run-with-timer
                 (min time-left eat-minimum-latency) nil
                 #'eat--eshell-process-output-queue process
                 (current-buffer))))))))

(declare-function eshell-sentinel "esh-proc" (proc string))

(defun eat--eshell-sentinel (process message)
  "Process status message MESSAGE from PROCESS."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (cl-letf* ((process-send-string
                  (symbol-function #'process-send-string))
                 ((symbol-function #'process-send-string)
                  (lambda (proc string)
                    (when (or (not (eq proc process))
                              (process-live-p proc))
                      (funcall process-send-string proc string)))))
        (eat--eshell-process-output-queue process (current-buffer)))
      (when (memq (process-status process) '(signal exit))
        (eat--eshell-cleanup))))
  (eshell-sentinel process message))

;; HACK: This is a dirty hack, it can break easily.
(defun eat--eshell-adjust-make-process-args (fn command args)
  "Setup an environment to adjust `make-process' arguments.

Call FN with COMMAND and ARGS, and whenever `make-process' is called,
modify its argument to change the filter, the sentinel and invoke
`stty' from the new process."
  (cl-letf*
      (;; For Emacs 29 and above.
       (make-process (symbol-function #'make-process))
       ((symbol-function #'make-process)
        (if (< emacs-major-version 29)
            make-process
          (lambda (&rest plist)
            ;; Make sure we don't attack wrong process.
            (if (not (equal (plist-get plist :command)
                            (cons (file-local-name
                                   (expand-file-name command))
                                  args)))
                (apply make-process plist)
              (setf (plist-get plist :command)
                    `("/usr/bin/env" "sh" "-c"
                      ,(format "stty -nl echo rows %d columns %d \
sane 2>%s ; if [ $1 = .. ]; then shift; fi; exec \"$@\""
                               (floor (window-screen-lines))
                               (window-max-chars-per-line)
                               null-device)
                      ".." ,@(plist-get plist :command)))
              (apply make-process plist)))))
       ;; For Emacs 28.
       (start-file-process (symbol-function #'start-file-process))
       ((symbol-function #'start-file-process)
        (if (< emacs-major-version 29)
            (lambda (name buffer &rest command)
              (apply start-file-process name buffer
                     `("/usr/bin/env" "sh" "-c"
                       ,(format "stty -nl echo rows %d columns %d \
sane 2>%s ; if [ $1 = .. ]; then shift; fi; exec \"$@\""
                                (floor (window-screen-lines))
                                (window-max-chars-per-line)
                                null-device)
                       ".." ,@command)))
          ;; Don't override on Emacs 28.
          start-file-process)))
    (let ((hook
           (lambda (proc)
             (set-process-filter proc #'eat--eshell-filter)
             (set-process-sentinel proc #'eat--eshell-sentinel)
             (eat--eshell-setup-proc-and-term proc))))
      (unwind-protect
          (progn
            (add-hook 'eshell-exec-hook hook 99)
            (funcall fn command args))
        (remove-hook 'eshell-exec-hook hook)))))


;;;;; Minor Modes.

(defun eat--eshell-synchronize-scroll ()
  "Synchronize scrolling and point between terminal and window."
  (when-let* ((window (get-buffer-window (current-buffer))))
    (set-window-start
     window
     (if (or (eat-term-in-alternative-display-p eat--terminal)
             eat--eshell-char-mode)
         (eat-term-display-beginning eat--terminal)
       (save-restriction
         (narrow-to-region
          (eat-term-beginning eat--terminal)
          (eat-term-end eat--terminal))
         (let ((start-line (- (floor (window-screen-lines))
                              (line-number-at-pos (point-max)))))
           (goto-char (point-min))
           (widen)
           (if (<= start-line 0)
               (eat-term-display-beginning eat--terminal)
             (vertical-motion (- start-line))
             (point))))))
    (set-window-point
     window (eat-term-display-cursor eat--terminal)))
  (goto-char (eat-term-display-cursor eat--terminal)))

(defun eat--eshell-update-cwd ()
  "Update the current working directory."
  (setq eat--eshell-invocation-directory default-directory))

(define-minor-mode eat--eshell-local-mode
  "Toggle Eat terminal emulation is Eshell."
  :interactive nil
  (let ((locals '(cursor-type
                  glyphless-char-display
                  track-mouse
                  filter-buffer-substring-function
                  eat--terminal
                  eat--process
                  eat--synchronize-scroll-function
                  eat--mouse-grabbing-type
                  eat--pending-output-chunks
                  eat--output-queue-first-chunk-time
                  eat--process-output-queue-timer
                  eat--eshell-invocation-directory)))
    (cond
     (eat--eshell-local-mode
      (mapc #'make-local-variable locals)
      (setq eat--synchronize-scroll-function
            #'eat--eshell-synchronize-scroll)
      (setq filter-buffer-substring-function
            #'eat--filter-buffer-substring)
      ;; Make sure glyphless character don't display a huge box glyph,
      ;; that would break the display.
      (eat--setup-glyphless-chars)
      (eat--eshell-update-cwd)
      (when eat-enable-blinking-text
        (eat-blink-mode +1)))
     (t
      (when eat-enable-blinking-text
        (eat-blink-mode -1))
      (mapc #'kill-local-variable locals)))))

(declare-function eshell-gather-process-output "esh-proc"
                  (command args))
(defvar eshell-variable-aliases-list) ; In `esh-var'.
(defvar eshell-last-async-proc) ; In `esh-cmd'.
(defvar eshell-last-async-procs) ; In `esh-cmd'.

;;;###autoload
(define-minor-mode eat-eshell-mode
  "Toggle Eat terminal emulation is Eshell."
  :global t
  :lighter (eat--eshell-local-mode
            (" Eat-Eshell"
             (:eval
              (when eat--terminal
                (cond
                 (eat--eshell-semi-char-mode
                  `("["
                    (:propertize
                     "semi-char"
                     help-echo
                     ,(concat "mouse-1: Switch to char mode, "
                              "mouse-3: Switch to emacs mode")
                     mouse-face mode-line-highlight
                     local-map
                     (keymap
                      (mode-line
                       . (keymap
                          (down-mouse-1 . eat-eshell-char-mode)
                          (down-mouse-3 . eat-eshell-emacs-mode)))))
                    "]"))
                 (eat--eshell-char-mode
                  '("["
                    (:propertize
                     "char"
                     help-echo
                     ,(concat "mouse-1: Switch to semi-char mode, "
                              "mouse-3: Switch to emacs mode")
                     mouse-face mode-line-highlight
                     local-map
                     (keymap
                      (mode-line
                       . (keymap
                          (down-mouse-1 . eat-eshell-semi-char-mode)
                          (down-mouse-3 . eat-eshell-emacs-mode)))))
                    "]"))
                 (t
                  `("["
                    (:propertize
                     "emacs"
                     help-echo
                     ,(concat "mouse-1: Switch to semi-char mode, "
                              "mouse-3: Switch to char mode")
                     mouse-face mode-line-highlight
                     local-map
                     (keymap
                      (mode-line
                       . (keymap
                          (down-mouse-1 . eat-eshell-semi-char-mode)
                          (down-mouse-3 . eat-eshell-char-mode)))))
                    "]")))))))
  :group 'eat-ehell
  (cond
   (eat-eshell-mode
    (let ((buffers nil))
      (setq eat-eshell-mode nil)
      (require 'esh-mode)
      (require 'esh-proc)
      (require 'esh-var)
      (require 'esh-cmd)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (eq major-mode #'eshell-mode)
            (when (if (< emacs-major-version 29)
                      (bound-and-true-p eshell-last-async-proc)
                    (bound-and-true-p eshell-last-async-procs))
              (user-error
               (concat "Can't toggle Eat Eshell mode while"
                       " any Eshell process is running")))
            (push buffer buffers))))
      (setq eat-eshell-mode t)
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (eat--eshell-local-mode +1))))
    (add-hook 'eshell-mode-hook #'eat--eshell-local-mode)
    (add-hook 'eshell-directory-change-hook #'eat--eshell-update-cwd)
    (setq eshell-variable-aliases-list
          `(("TERM" eat--eshell-term-name t)
            ("TERMINFO" eat-term-terminfo-directory t)
            ("INSIDE_EMACS" eat-term-inside-emacs t)
            ("EAT_SHELL_INTEGRATION_DIR"
             eat-term-shell-integration-directory t)
            ,@eshell-variable-aliases-list))
    (advice-add #'eshell-gather-process-output :around
                #'eat--eshell-adjust-make-process-args))
   (t
    (let ((buffers nil))
      (setq eat-eshell-mode t)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (eq major-mode #'eshell-mode)
                     eat--eshell-local-mode)
            (when (if (< emacs-major-version 29)
                      (bound-and-true-p eshell-last-async-proc)
                    (bound-and-true-p eshell-last-async-procs))
              (user-error
               (concat "Can't toggle Eat Eshell mode while"
                       " any Eshell process is running")))
            (push buffer buffers))))
      (setq eat-eshell-mode nil)
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (eat--eshell-local-mode -1))))
    (remove-hook 'eshell-mode-hook #'eat--eshell-local-mode)
    (remove-hook 'eshell-directory-change-hook
                 #'eat--eshell-update-cwd)
    (setq eshell-variable-aliases-list
          (cl-delete-if
           (lambda (elem)
             (member elem
                     '(("TERM" eat--eshell-term-name t)
                       ("TERMINFO" eat-term-terminfo-directory t)
                       ("INSIDE_EMACS" eat-term-inside-emacs t))))
           eshell-variable-aliases-list))
    (advice-remove #'eshell-gather-process-output
                   #'eat--eshell-adjust-make-process-args))))


;;;; Eshell Visual Command Handling.

(defvar eshell-destroy-buffer-when-process-dies) ; In `em-term'.

;; Adapted from `em-term'.
(defun eat--eshell-visual-sentinel (proc _msg)
  "Clean up the buffer visiting PROC.

If `eshell-destroy-buffer-when-process-dies' is non-nil, destroy
the buffer.

MSG describes PROC's status."
  (when eshell-destroy-buffer-when-process-dies
    (let ((proc-buf (process-buffer proc)))
      (when (and proc-buf (buffer-live-p proc-buf)
                 (not (eq 'run (process-status proc)))
                 (= (process-exit-status proc) 0))
        (if (eq (current-buffer) proc-buf)
            (when-let* ((buf (and (boundp 'eshell-parent-buffer)
                                  (buffer-live-p eshell-parent-buffer)
                                  eshell-parent-buffer)))
              (switch-to-buffer buf)))
        (kill-buffer proc-buf)))))

(defvar eshell-interpreter-alist) ; In `esh-ext'.
(declare-function eshell-find-interpreter "esh-ext"
                  (file args &optional no-examine-p))
(declare-function eshell-stringify-list "esh-util" (args))

(defun eat--eshell-exec-visual (&rest args)
  "Run the specified PROGRAM in a terminal emulation buffer.

ARGS are passed to the program.  At the moment, no piping of input is
allowed."
  (require 'esh-ext)
  (require 'esh-util)
  (let* ((eshell-interpreter-alist nil)
         (interp (eshell-find-interpreter (car args) (cdr args)))
         (program (car interp))
         (args (flatten-tree
                (eshell-stringify-list (append (cdr interp)
                                               (cdr args)))))
         (eat-buf
          (generate-new-buffer
           (concat "*" (file-name-nondirectory program) "*")))
         (eshell-buf (current-buffer)))
    (with-current-buffer eat-buf
      (switch-to-buffer eat-buf)
      (eat-mode)
      (setq-local eshell-parent-buffer eshell-buf)
      (setq-local eat-kill-buffer-on-exit nil)
      (eat-exec eat-buf program program nil args)
      (let ((proc (get-buffer-process eat-buf)))
        (if (and proc (eq 'run (process-status proc)))
            (let ((sentinel (process-sentinel proc)))
              (add-function  :after (var sentinel)
                             #'eat--eshell-visual-sentinel)
              (set-process-sentinel proc sentinel))
          (error "Failed to invoke visual command")))
      (eat-semi-char-mode)))
  nil)

(declare-function eshell-exec-visual "em-term" (&rest args))

;;;###autoload
(define-minor-mode eat-eshell-visual-command-mode
  "Toggle running Eshell visual commands with Eat."
  :group 'eat-eshell
  :global t
  (if eat-eshell-visual-command-mode
      (advice-add #'eshell-exec-visual :override
                  #'eat--eshell-exec-visual)
    (advice-remove #'eshell-exec-visual #'eat--eshell-exec-visual)))


;;;; Project Integration.

(declare-function project-root "project" (project))
(declare-function project-prefixed-buffer-name "project" (mode))

;;;###autoload
(defun eat-project (&optional arg)
  "Start Eat in the current project's root directory.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG (like \\[universal-argument] 42 \\[eshell]),
switch to the session with that number, or create it if it doesn't
already exist."
  (interactive "P")
  (require 'project)
  (let* ((default-directory (project-root (project-current t)))
         (eat-buffer-name (project-prefixed-buffer-name "eat")))
    (eat nil arg)))


;;;; Tracing.

;;;;; Recording Trace Data.

(defconst eat--trace-recorded-variables
  '(eat-term-scrollback-size
    eat-enable-alternative-display)
  "The variable to record in trace output.")

(defvar eat--trace-output-buffer nil
  "Buffer where the trace data is written to.")

(defun eat--trace-log (time operation &rest args)
  "Log TIME, OPERATION and ARGS into trace output.

TIME defaults to the current time.

The current buffer should be the trace output buffer.  Move the point
to the end of (accessible portion of) buffer."
  (goto-char (point-max))
  ;; Hope that `float-time' won't roll over while tracing.  ;-)
  (insert (replace-regexp-in-string
           (rx (any (0 . 31)))
           (lambda (string) (format "\\\\x%02x" (aref string 0)))
           (format "%S" `(,(float-time time) ,operation ,@args)))
          ?\n))

(defun eat--trace-stop ()
  "Stop tracing the terminal in current buffer."
  (when eat--trace-output-buffer
    (with-current-buffer eat--trace-output-buffer
      (eat--trace-log nil 'finish)))
  (remove-hook 'kill-buffer-hook #'eat--trace-stop t)
  (kill-local-variable 'eat--trace-output-buffer))

(defun eat--trace-exec (fn buffer name command startfile switches)
  "Trace `eat-exec'.

BUFFER is the buffer and COMMAND and SWITCHES are the invocation
command.  BUFFER, NAME, COMMAND, STARTFILE and SWITCHES are passed to
FN, `eat-exec', which see."
  (let ((time (current-time)))
    (prog1
        (funcall fn buffer name command startfile switches)
      (let ((buf (generate-new-buffer
                  (format "*eat-trace %s*: %s"
                          (buffer-name buffer)
                          (mapconcat #'shell-quote-argument
                                     (cons command switches) " "))))
            (width nil)
            (height nil)
            (variables nil))
        (with-current-buffer buffer
          (setq-local eat--trace-output-buffer buf)
          (add-hook 'kill-buffer-hook #'eat--trace-stop nil t)
          (let ((size (eat-term-size eat--terminal)))
            (setq width (car size))
            (setq height (cdr size)))
          (dolist (var eat--trace-recorded-variables)
            (push (cons var (symbol-value var)) variables)))
        (with-current-buffer buf
          (lisp-data-mode)
          (insert ";; -*- mode: lisp-data -*-\n")
          (eat--trace-log time 'create 'eat width height
                          variables))))))

(defun eat--trace-process-output-queue (fn buffer)
  "Trace `eat--process-output-queue'.

BUFFER is passed to FN, `eat--process-output-queue', which see."
  (if (or (not (buffer-live-p buffer))
          (not (buffer-local-value 'eat--trace-output-buffer buffer)))
      (funcall fn buffer)
    (cl-letf* ((eat-term-process-output
                (symbol-function #'eat-term-process-output))
               ((symbol-function #'eat-term-process-output)
                (lambda (terminal output)
                  (when (buffer-live-p eat--trace-output-buffer)
                    (with-current-buffer eat--trace-output-buffer
                      (eat--trace-log nil 'output output)))
                  (funcall eat-term-process-output terminal output)))
               (eat-term-redisplay
                (symbol-function #'eat-term-redisplay))
               ((symbol-function #'eat-term-redisplay)
                (lambda (terminal)
                  (when (buffer-live-p eat--trace-output-buffer)
                    (with-current-buffer eat--trace-output-buffer
                      (eat--trace-log nil 'redisplay)))
                  (funcall eat-term-redisplay terminal))))
      (funcall fn buffer))))

(defun eat--trace-adjust-process-window-size (fn process windows)
  "Trace `eat--adjust-process-window-size'.

PROCESS and WINDOWS are passed to FN,
`eat--adjust-process-window-size', which see."
  (cl-letf*
      ((eat-term-resize (symbol-function #'eat-term-resize))
       ((symbol-function #'eat-term-resize)
        (lambda (terminal width height)
          (when (buffer-live-p eat--trace-output-buffer)
            (with-current-buffer eat--trace-output-buffer
              (eat--trace-log nil 'resize width height)))
          (funcall eat-term-resize terminal width height)))
       (eat-term-redisplay (symbol-function #'eat-term-redisplay))
       ((symbol-function #'eat-term-redisplay)
        (lambda (terminal)
          (when (buffer-live-p eat--trace-output-buffer)
            (with-current-buffer eat--trace-output-buffer
              (eat--trace-log nil 'redisplay)))
          (funcall eat-term-redisplay terminal))))
    (funcall fn process windows)))

(defun eat--trace-sentinel (fn &rest args)
  "Trace `eat--sentinel'.

Elements of ARGS are passed to FN, `eat--sentinel', which see."
  (cl-letf* ((eat-term-delete (symbol-function #'eat-term-delete))
             ((symbol-function #'eat-term-delete)
              (lambda (terminal)
                (when (buffer-live-p eat--trace-output-buffer)
                  (eat--trace-stop))
                (funcall eat-term-delete terminal))))
    (apply fn args)))

(defun eat--trace-reset (fn)
  "Trace `eat-reset'.

FN is original definition of `eat-reset'."
  (cl-letf*
      ((eat-term-reset (symbol-function #'eat-term-reset))
       ((symbol-function #'eat-term-reset)
        (lambda (terminal)
          (when (buffer-live-p eat--trace-output-buffer)
            (with-current-buffer eat--trace-output-buffer
              (eat--trace-log nil 'reset)))
          (funcall eat-term-reset terminal)))
       (eat-term-redisplay (symbol-function #'eat-term-redisplay))
       ((symbol-function #'eat-term-redisplay)
        (lambda (terminal)
          (when (buffer-live-p eat--trace-output-buffer)
            (with-current-buffer eat--trace-output-buffer
              (eat--trace-log nil 'redisplay)))
          (funcall eat-term-redisplay terminal))))
    (funcall fn)))

(defun eat--trace-eshell-adjust-make-process-args (fn &rest args)
  "Trace `eat--eshell-adjust-make-process-args'.

ARGS is passed to FN, `eat--eshell-adjust-make-process-args', which
see."
  (cl-letf*
      ((command nil)
       (make-process (symbol-function #'make-process))
       ((symbol-function #'make-process)
        (lambda (&rest plist)
          (prog1
              (apply make-process plist)
            (setq command (nthcdr 5 (plist-get plist :command))))))
       (eat--eshell-setup-proc-and-term
        (symbol-function #'eat--eshell-setup-proc-and-term))
       ((symbol-function #'eat--eshell-setup-proc-and-term)
        (lambda (proc)
          (let ((time (current-time)))
            (prog1
                (funcall eat--eshell-setup-proc-and-term proc)
              (when (eq eat--process proc)
                (let ((buf (generate-new-buffer
                            (format "*eat-trace %s*: %s"
                                    (buffer-name)
                                    (mapconcat
                                     #'shell-quote-argument
                                     command " "))))
                      (width nil)
                      (height nil)
                      (variables nil))
                  (setq-local eat--trace-output-buffer buf)
                  (add-hook 'kill-buffer-hook #'eat--trace-stop nil t)
                  (let ((size (eat-term-size eat--terminal)))
                    (setq width (car size))
                    (setq height (cdr size)))
                  (dolist (var eat--trace-recorded-variables)
                    (push (cons var (symbol-value var)) variables))
                  (with-current-buffer buf
                    (lisp-data-mode)
                    (insert ";; -*- lisp-data -*-\n")
                    (eat--trace-log time 'create 'eshell width height
                                    variables)))))))))
    (apply fn args)))

(defun eat--trace-eshell-output-filter (fn)
  "Trace `eat--eshell-output-filter'.

FN is the original definition of `eat--eshell-output-filter', which
see."
  (if (not (buffer-live-p eat--trace-output-buffer))
      (funcall fn)
    (cl-letf* ((eat-term-process-output
                (symbol-function #'eat-term-process-output))
               ((symbol-function #'eat-term-process-output)
                (lambda (terminal output)
                  (with-current-buffer eat--trace-output-buffer
                    (eat--trace-log nil 'output output))
                  (funcall eat-term-process-output terminal output)))
               (eat-term-redisplay
                (symbol-function #'eat-term-redisplay))
               ((symbol-function #'eat-term-redisplay)
                (lambda (terminal)
                  (with-current-buffer eat--trace-output-buffer
                    (eat--trace-log nil 'redisplay))
                  (funcall eat-term-redisplay terminal))))
      (funcall fn))))

(defun eat--trace-eshell-cleanup (fn)
  "Trace `eat--eshell-cleanup'.

FN is the original definition of `eat--eshell-cleanup', which see."
  (if (not (buffer-live-p eat--trace-output-buffer))
      (funcall fn)
    (cl-letf* ((eat-term-delete (symbol-function #'eat-term-delete))
               ((symbol-function #'eat-term-delete)
                (lambda (terminal)
                  (eat--trace-stop)
                  (funcall eat-term-delete terminal))))
      (funcall fn))))

(define-minor-mode eat-trace-mode
  "Toggle tracing Eat terminal."
  :global t
  :require 'eat
  :lighter " Eat-Trace"
  (if eat-trace-mode
      (progn
        (advice-add #'eat-exec :around #'eat--trace-exec)
        (advice-add #'eat--process-output-queue :around
                    #'eat--trace-process-output-queue)
        (advice-add #'eat--adjust-process-window-size :around
                    #'eat--trace-adjust-process-window-size)
        (advice-add #'eat--sentinel :around #'eat--trace-sentinel)
        (advice-add #'eat-reset :around #'eat--trace-reset)
        (advice-add #'eat--eshell-adjust-make-process-args :around
                    #'eat--trace-eshell-adjust-make-process-args)
        (advice-add #'eat--eshell-output-filter :around
                    #'eat--trace-eshell-output-filter)
        (advice-add #'eat--eshell-cleanup :around
                    #'eat--trace-eshell-cleanup))
    (advice-remove #'eat-exec #'eat--trace-exec)
    (advice-remove #'eat--process-output-queue
                   #'eat--trace-process-output-queue)
    (advice-remove #'eat--adjust-process-window-size
                   #'eat--trace-adjust-process-window-size)
    (advice-remove #'eat--sentinel #'eat--trace-sentinel)
    (advice-remove #'eat-reset #'eat--trace-reset)
    (advice-remove #'eat--eshell-adjust-make-process-args
                   #'eat--trace-eshell-adjust-make-process-args)
    (advice-remove #'eat--eshell-output-filter
                   #'eat--trace-eshell-output-filter)
    (advice-remove #'eat--eshell-cleanup
                   #'eat--trace-eshell-cleanup)
    (dolist (buffer (buffer-list))
      (when (buffer-local-value 'eat--trace-output-buffer buffer)
        (with-current-buffer buffer
          (setq-local eat--trace-output-buffer nil))))))


;;;;; Trace Data Replay.

(defvar eat--trace-replay-buffer nil
  "The buffer replaying the trace data in current buffer.")

(defvar eat--trace-replay-marker nil
  "The point from where to read the next sexp.")

(defvar eat--trace-replay-current-sexp-overlay nil
  "Overlay indicating the current sexp.")

(defvar eat--trace-replay-source-buffer nil
  "The source buffer containing the trace output.")

(defvar eat--trace-replay-recording-start-time 0.0
  "Time when recording was started.")

(defvar eat--trace-replay-frame-count 0
  "The number of the frames in the trace output.")

(defvar eat--trace-replay-progress-frame 0
  "The number of the frames before the current position.")

(defvar eat--trace-replay-progress nil
  "The number of seconds of trace output was shown.")

(defun eat--trace-replay-eval (data)
  "Evalulate DATA as trace output."
  (let ((inhibit-read-only t))
    (setq eat--trace-replay-progress
          (- (car data) eat--trace-replay-recording-start-time))
    (pcase-exhaustive data
      (`(,time create ,_ui ,width ,height ,variables)
       (setq eat--trace-replay-recording-start-time time)
       (setq eat--trace-replay-progress 0)
       (dolist (var eat--trace-recorded-variables)
         (set (make-local-variable var) (alist-get var variables)))
       (setq eat--terminal (eat-term-make (current-buffer) (point)))
       (setf (eat-term-set-cursor-function eat--terminal)
             #'eat--set-cursor)
       (setf (eat-term-ring-bell-function eat--terminal) #'eat--bell)
       (eat-term-resize eat--terminal width height)
       (eat-term-redisplay eat--terminal))
      (`(,_time output ,string)
       (eat-term-process-output eat--terminal string))
      (`(,_time redisplay)
       (eat-term-redisplay eat--terminal))
      (`(,_time resize ,width ,height)
       (eat-term-resize eat--terminal width height))
      (`(,_time reset)
       (eat-term-reset eat--terminal))
      (`(,_time finish)
       (eat-term-delete eat--terminal)))
    (eat--synchronize-scroll)))

(defun eat--trace-replay-eval-next ()
  "Evaluate next sexp in trace output."
  (with-current-buffer eat--trace-replay-source-buffer
    (goto-char eat--trace-replay-marker)
    (ignore-error end-of-file
      (let ((data (read (current-buffer))))
        (set-marker eat--trace-replay-marker (point))
        (backward-list)
        (move-overlay eat--trace-replay-current-sexp-overlay
                      (point) (point))
        (when-let* ((window (get-buffer-window)))
          (set-window-point window (point)))
        (with-current-buffer eat--trace-replay-buffer
          (cl-incf eat--trace-replay-progress-frame)
          (eat--trace-replay-eval data))))))

(defun eat-trace-replay ()
  "Replay terminal according to trace output in current buffer."
  (interactive)
  (unless (buffer-live-p eat--trace-replay-buffer)
    (setq-local eat--trace-replay-buffer
                (generate-new-buffer
                 (format "*eat-trace-replay*: %s" (buffer-name))))
    (setq-local eat--trace-replay-marker (point-min-marker))
    (let ((ov (make-overlay (point-min) (point-min))))
      (overlay-put ov 'before-string
                   #(" " 0 1 (display (left-fringe right-triangle))))
      (setq-local eat--trace-replay-current-sexp-overlay ov))
    (goto-char (point-min))
    (let ((source (current-buffer))
          (frame-count 0))
      (ignore-error end-of-file
        (while (read (current-buffer))
          (cl-incf frame-count)))
      (goto-char (point-min))
      (with-current-buffer eat--trace-replay-buffer
        (eat-trace-replay-mode)
        (setq eat--trace-replay-source-buffer source)
        (setq eat--trace-replay-frame-count frame-count))))
  (display-buffer eat--trace-replay-buffer))

(defun eat-trace-replay-next-frame (&optional n)
  "Show the Nth next frame.

N defaults to 1.  Interactively, N is the prefix argument."
  (interactive "p")
  (dotimes (_ n)
    (eat--trace-replay-eval-next)))

(defun eat-trace--cleanup ()
  "Clean up the source buffer before the terminal being killed."
  (when (buffer-live-p eat--trace-replay-source-buffer)
    (with-current-buffer eat--trace-replay-source-buffer
      (setq eat--trace-replay-buffer nil)
      (setq eat--trace-replay-marker nil)
      (delete-overlay eat--trace-replay-current-sexp-overlay))))

(defvar eat-trace-replay-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'eat-trace-replay-next-frame)
    (define-key map (kbd "<down>") #'eat-trace-replay-next-frame)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for Eat-Trace-Replay mode.")

(define-derived-mode eat-trace-replay-mode special-mode
  "Eat-Trace-Replay"
  "Major mode for replaying terminal according to trace output."
  (mapc #'make-local-variable '(eat--terminal
                                eat--trace-replay-source-buffer
                                eat--trace-replay-recording-start-time
                                eat--trace-replay-progress
                                eat--trace-replay-frame-count
                                eat--trace-replay-progress-frame))
  (setq-local
   mode-line-process
   '("[" (:eval (number-to-string eat--trace-replay-progress-frame))
     "/" (:eval (number-to-string eat--trace-replay-frame-count))
     "]"))
  (add-hook 'kill-buffer-hook #'eat-trace--cleanup nil t))

(provide 'eat)
;;; eat.el ends here
