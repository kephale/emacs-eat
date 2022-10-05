;;; eat.el --- Emulate A Terminal, in a region, a buffer and in Eshell -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-08-15
;; Version: 0.1
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

;; Eat's name self-explainary, it stands "Emulate A Terminal".  Eat is
;; a terminal emulator.  It can run most (if not all) full-screen
;; terminal programs, including Emacs.

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
;;     terminal, except the following keys: `C-\' `C-c' `C-x' `C-g'
;;     `C-h' `C-M-c' `C-u' `M-x' `M-:' `M-!' `M-&'.  The following
;;     special keybinding are available:

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

(require 'cl-lib)
(require 'ansi-color)
(require 'shell)


;;;; User options.

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
  "Size of scrollback area in characters.  Nil means unlimited."
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

(defconst eat--cursor-type-value-type
  '(cons
    (choice
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
     (const :tag "None " nil))
    (choice
     (const :tag "No blinking" nil)
     (number :tag "Blinking frequency")))
  "Custom type specification for Eat's cursor type variables.")

(defcustom eat-default-cursor-type
  (cons (default-value 'cursor-type) nil)
  "Cursor to use in Eat buffer.

When the cursor is visible, the car of the value is used as
`cursor-type', which see.  The cdr of the value, when non-nil, is the
blinking frequency of cursor."
  :type eat--cursor-type-value-type
  :group 'eat-ui
  :group 'eat-ehell)

(defcustom eat-invisible-cursor-type '(nil . nil)
  "Cursor to use in Eat buffer.

When the cursor is invisible, the car of the value is used as
`cursor-type', which see.  The cdr of the value, when non-nil, is the
blinking frequency of cursor."
  :type eat--cursor-type-value-type
  :group 'eat-ui
  :group 'eat-ehell)

(defcustom eat-very-visible-cursor-type
  (cons (default-value 'cursor-type) 2)
  "Cursor to use in Eat buffer.

When the cursor is very visible, the car of the value is used as
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

;; Upgrading Eat causes `eat-term-terminfo-directory' to be outdated,
;; so update it if not modified by user (or something else).
(defvar eat--install-path nil
  "Path to directory where Eat is installed.")

(defvar eat-term-terminfo-directory)
(let ((old-install-path eat--install-path))
  (setq eat--install-path
        (copy-sequence
         (file-name-directory (or load-file-name
                                  buffer-file-name))))

  (defcustom eat-term-terminfo-directory eat--install-path
    "Directory where require terminfo databases can be found.

This value is used by terminal programs to find the terminfo databases
that describe the capabilities of the terminal."
    :type 'directory
    :group 'eat-term)

  (when (eq eat-term-terminfo-directory old-install-path)
    (setq eat-term-terminfo-directory eat--install-path)))

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

(defface eat-term-color-0
  '((t :inherit ansi-color-black))
  "Face used to render black color text."
  :group 'eat-term)

(put 'eat-term-color-black 'face-alias 'eat-term-color-0)

(defface eat-term-color-1
  '((t :inherit ansi-color-red))
  "Face used to render red color text."
  :group 'eat-term
  :version "28.1")

(put 'eat-term-color-red 'face-alias 'eat-term-color-1)

(defface eat-term-color-2
  '((t :inherit ansi-color-green))
  "Face used to render green color text."
  :group 'eat-term
  :version "28.1")

(put 'eat-term-color-green 'face-alias 'eat-term-color-2)

(defface eat-term-color-3
  '((t :inherit ansi-color-yellow))
  "Face used to render yellow color text."
  :group 'eat-term)

(put 'eat-term-color-yellow 'face-alias 'eat-term-color-3)

(defface eat-term-color-4
  '((t :inherit ansi-color-blue))
  "Face used to render blue color text."
  :group 'eat-term)

(put 'eat-term-color-blue 'face-alias 'eat-term-color-4)

(defface eat-term-color-5
  '((t :inherit ansi-color-magenta))
  "Face used to render magenta color text."
  :group 'eat-term)

(put 'eat-term-color-magenta 'face-alias 'eat-term-color-5)

(defface eat-term-color-6
  '((t :inherit ansi-color-cyan))
  "Face used to render cyan color text."
  :group 'eat-term)

(put 'eat-term-color-cyan 'face-alias 'eat-term-color-6)

(defface eat-term-color-7
  '((t :inherit ansi-color-white))
  "Face used to render white color text."
  :group 'eat-term)

(put 'eat-term-color-white 'face-alias 'eat-term-color-7)

(defface eat-term-color-8
  '((t :inherit ansi-color-bright-black))
  "Face used to render bright black color text."
  :group 'eat-term)

(put 'eat-term-color-bright-black 'face-alias 'eat-term-color-8)

(defface eat-term-color-9
  '((t :inherit ansi-color-bright-red))
  "Face used to render bright red color text."
  :group 'eat-term)

(put 'eat-term-color-bright-red 'face-alias 'eat-term-color-9)

(defface eat-term-color-10
  '((t :inherit ansi-color-bright-green))
  "Face used to render bright green color text."
  :group 'eat-term)

(put 'eat-term-color-bright-green 'face-alias 'eat-term-color-10)

(defface eat-term-color-11
  '((t :inherit ansi-color-bright-yellow))
  "Face used to render bright yellow color text."
  :group 'eat-term)

(put 'eat-term-color-bright-yellow 'face-alias 'eat-term-color-11)

(defface eat-term-color-12
  '((t :inherit ansi-color-bright-blue))
  "Face used to render bright blue color text."
  :group 'eat-term)

(put 'eat-term-color-bright-blue 'face-alias 'eat-term-color-12)

(defface eat-term-color-13
  '((t :inherit ansi-color-bright-magenta))
  "Face used to render bright magenta color text."
  :group 'eat-term)

(put 'eat-term-color-bright-magenta 'face-alias 'eat-term-color-13)

(defface eat-term-color-14
  '((t :inherit ansi-color-bright-cyan))
  "Face used to render bright cyan color text."
  :group 'eat-term)

(put 'eat-term-color-bright-cyan 'face-alias 'eat-term-color-14)

(defface eat-term-color-15
  '((t :inherit ansi-color-bright-white))
  "Face used to render bright white color text."
  :group 'eat-term)

(put 'eat-term-color-bright-white 'face-alias 'eat-term-color-15)

(defface eat-term-color-16
  '((t :foreground "#000000" :background "#000000"))
  "Face used to render text with 16th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-17
  '((t :foreground "#00005F" :background "#00005F"))
  "Face used to render text with 17th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-18
  '((t :foreground "#000087" :background "#000087"))
  "Face used to render text with 18th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-19
  '((t :foreground "#0000AF" :background "#0000AF"))
  "Face used to render text with 19th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-20
  '((t :foreground "#0000D7" :background "#0000D7"))
  "Face used to render text with 20th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-21
  '((t :foreground "#0000FF" :background "#0000FF"))
  "Face used to render text with 21th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-22
  '((t :foreground "#005F00" :background "#005F00"))
  "Face used to render text with 22th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-23
  '((t :foreground "#005F5F" :background "#005F5F"))
  "Face used to render text with 23th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-24
  '((t :foreground "#005F87" :background "#005F87"))
  "Face used to render text with 24th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-25
  '((t :foreground "#005FAF" :background "#005FAF"))
  "Face used to render text with 25th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-26
  '((t :foreground "#005FD7" :background "#005FD7"))
  "Face used to render text with 26th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-27
  '((t :foreground "#005FFF" :background "#005FFF"))
  "Face used to render text with 27th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-28
  '((t :foreground "#008700" :background "#008700"))
  "Face used to render text with 28th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-29
  '((t :foreground "#00875F" :background "#00875F"))
  "Face used to render text with 29th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-30
  '((t :foreground "#008787" :background "#008787"))
  "Face used to render text with 30th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-31
  '((t :foreground "#0087AF" :background "#0087AF"))
  "Face used to render text with 31th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-32
  '((t :foreground "#0087D7" :background "#0087D7"))
  "Face used to render text with 32th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-33
  '((t :foreground "#0087FF" :background "#0087FF"))
  "Face used to render text with 33th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-34
  '((t :foreground "#00AF00" :background "#00AF00"))
  "Face used to render text with 34th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-35
  '((t :foreground "#00AF5F" :background "#00AF5F"))
  "Face used to render text with 35th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-36
  '((t :foreground "#00AF87" :background "#00AF87"))
  "Face used to render text with 36th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-37
  '((t :foreground "#00AFAF" :background "#00AFAF"))
  "Face used to render text with 37th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-38
  '((t :foreground "#00AFD7" :background "#00AFD7"))
  "Face used to render text with 38th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-39
  '((t :foreground "#00AFFF" :background "#00AFFF"))
  "Face used to render text with 39th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-40
  '((t :foreground "#00D700" :background "#00D700"))
  "Face used to render text with 40th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-41
  '((t :foreground "#00D75F" :background "#00D75F"))
  "Face used to render text with 41th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-42
  '((t :foreground "#00D787" :background "#00D787"))
  "Face used to render text with 42th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-43
  '((t :foreground "#00D7AF" :background "#00D7AF"))
  "Face used to render text with 43th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-44
  '((t :foreground "#00D7D7" :background "#00D7D7"))
  "Face used to render text with 44th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-45
  '((t :foreground "#00D7FF" :background "#00D7FF"))
  "Face used to render text with 45th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-46
  '((t :foreground "#00FF00" :background "#00FF00"))
  "Face used to render text with 46th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-47
  '((t :foreground "#00FF5F" :background "#00FF5F"))
  "Face used to render text with 47th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-48
  '((t :foreground "#00FF87" :background "#00FF87"))
  "Face used to render text with 48th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-49
  '((t :foreground "#00FFAF" :background "#00FFAF"))
  "Face used to render text with 49th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-50
  '((t :foreground "#00FFD7" :background "#00FFD7"))
  "Face used to render text with 50th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-51
  '((t :foreground "#00FFFF" :background "#00FFFF"))
  "Face used to render text with 51th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-52
  '((t :foreground "#5F0000" :background "#5F0000"))
  "Face used to render text with 52th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-53
  '((t :foreground "#5F005F" :background "#5F005F"))
  "Face used to render text with 53th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-54
  '((t :foreground "#5F0087" :background "#5F0087"))
  "Face used to render text with 54th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-55
  '((t :foreground "#5F00AF" :background "#5F00AF"))
  "Face used to render text with 55th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-56
  '((t :foreground "#5F00D7" :background "#5F00D7"))
  "Face used to render text with 56th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-57
  '((t :foreground "#5F00FF" :background "#5F00FF"))
  "Face used to render text with 57th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-58
  '((t :foreground "#5F5F00" :background "#5F5F00"))
  "Face used to render text with 58th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-59
  '((t :foreground "#5F5F5F" :background "#5F5F5F"))
  "Face used to render text with 59th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-60
  '((t :foreground "#5F5F87" :background "#5F5F87"))
  "Face used to render text with 60th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-61
  '((t :foreground "#5F5FAF" :background "#5F5FAF"))
  "Face used to render text with 61th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-62
  '((t :foreground "#5F5FD7" :background "#5F5FD7"))
  "Face used to render text with 62th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-63
  '((t :foreground "#5F5FFF" :background "#5F5FFF"))
  "Face used to render text with 63th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-64
  '((t :foreground "#5F8700" :background "#5F8700"))
  "Face used to render text with 64th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-65
  '((t :foreground "#5F875F" :background "#5F875F"))
  "Face used to render text with 65th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-66
  '((t :foreground "#5F8787" :background "#5F8787"))
  "Face used to render text with 66th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-67
  '((t :foreground "#5F87AF" :background "#5F87AF"))
  "Face used to render text with 67th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-68
  '((t :foreground "#5F87D7" :background "#5F87D7"))
  "Face used to render text with 68th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-69
  '((t :foreground "#5F87FF" :background "#5F87FF"))
  "Face used to render text with 69th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-70
  '((t :foreground "#5FAF00" :background "#5FAF00"))
  "Face used to render text with 70th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-71
  '((t :foreground "#5FAF5F" :background "#5FAF5F"))
  "Face used to render text with 71th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-72
  '((t :foreground "#5FAF87" :background "#5FAF87"))
  "Face used to render text with 72th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-73
  '((t :foreground "#5FAFAF" :background "#5FAFAF"))
  "Face used to render text with 73th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-74
  '((t :foreground "#5FAFD7" :background "#5FAFD7"))
  "Face used to render text with 74th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-75
  '((t :foreground "#5FAFFF" :background "#5FAFFF"))
  "Face used to render text with 75th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-76
  '((t :foreground "#5FD700" :background "#5FD700"))
  "Face used to render text with 76th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-77
  '((t :foreground "#5FD75F" :background "#5FD75F"))
  "Face used to render text with 77th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-78
  '((t :foreground "#5FD787" :background "#5FD787"))
  "Face used to render text with 78th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-79
  '((t :foreground "#5FD7AF" :background "#5FD7AF"))
  "Face used to render text with 79th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-80
  '((t :foreground "#5FD7D7" :background "#5FD7D7"))
  "Face used to render text with 80th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-81
  '((t :foreground "#5FD7FF" :background "#5FD7FF"))
  "Face used to render text with 81th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-82
  '((t :foreground "#5FFF00" :background "#5FFF00"))
  "Face used to render text with 82th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-83
  '((t :foreground "#5FFF5F" :background "#5FFF5F"))
  "Face used to render text with 83th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-84
  '((t :foreground "#5FFF87" :background "#5FFF87"))
  "Face used to render text with 84th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-85
  '((t :foreground "#5FFFAF" :background "#5FFFAF"))
  "Face used to render text with 85th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-86
  '((t :foreground "#5FFFD7" :background "#5FFFD7"))
  "Face used to render text with 86th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-87
  '((t :foreground "#5FFFFF" :background "#5FFFFF"))
  "Face used to render text with 87th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-88
  '((t :foreground "#870000" :background "#870000"))
  "Face used to render text with 88th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-89
  '((t :foreground "#87005F" :background "#87005F"))
  "Face used to render text with 89th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-90
  '((t :foreground "#870087" :background "#870087"))
  "Face used to render text with 90th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-91
  '((t :foreground "#8700AF" :background "#8700AF"))
  "Face used to render text with 91th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-92
  '((t :foreground "#8700D7" :background "#8700D7"))
  "Face used to render text with 92th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-93
  '((t :foreground "#8700FF" :background "#8700FF"))
  "Face used to render text with 93th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-94
  '((t :foreground "#875F00" :background "#875F00"))
  "Face used to render text with 94th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-95
  '((t :foreground "#875F5F" :background "#875F5F"))
  "Face used to render text with 95th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-96
  '((t :foreground "#875F87" :background "#875F87"))
  "Face used to render text with 96th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-97
  '((t :foreground "#875FAF" :background "#875FAF"))
  "Face used to render text with 97th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-98
  '((t :foreground "#875FD7" :background "#875FD7"))
  "Face used to render text with 98th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-99
  '((t :foreground "#875FFF" :background "#875FFF"))
  "Face used to render text with 99th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-100
  '((t :foreground "#878700" :background "#878700"))
  "Face used to render text with 100th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-101
  '((t :foreground "#87875F" :background "#87875F"))
  "Face used to render text with 101th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-102
  '((t :foreground "#878787" :background "#878787"))
  "Face used to render text with 102th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-103
  '((t :foreground "#8787AF" :background "#8787AF"))
  "Face used to render text with 103th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-104
  '((t :foreground "#8787D7" :background "#8787D7"))
  "Face used to render text with 104th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-105
  '((t :foreground "#8787FF" :background "#8787FF"))
  "Face used to render text with 105th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-106
  '((t :foreground "#87AF00" :background "#87AF00"))
  "Face used to render text with 106th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-107
  '((t :foreground "#87AF5F" :background "#87AF5F"))
  "Face used to render text with 107th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-108
  '((t :foreground "#87AF87" :background "#87AF87"))
  "Face used to render text with 108th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-109
  '((t :foreground "#87AFAF" :background "#87AFAF"))
  "Face used to render text with 109th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-110
  '((t :foreground "#87AFD7" :background "#87AFD7"))
  "Face used to render text with 110th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-111
  '((t :foreground "#87AFFF" :background "#87AFFF"))
  "Face used to render text with 111th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-112
  '((t :foreground "#87D700" :background "#87D700"))
  "Face used to render text with 112th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-113
  '((t :foreground "#87D75F" :background "#87D75F"))
  "Face used to render text with 113th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-114
  '((t :foreground "#87D787" :background "#87D787"))
  "Face used to render text with 114th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-115
  '((t :foreground "#87D7AF" :background "#87D7AF"))
  "Face used to render text with 115th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-116
  '((t :foreground "#87D7D7" :background "#87D7D7"))
  "Face used to render text with 116th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-117
  '((t :foreground "#87D7FF" :background "#87D7FF"))
  "Face used to render text with 117th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-118
  '((t :foreground "#87FF00" :background "#87FF00"))
  "Face used to render text with 118th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-119
  '((t :foreground "#87FF5F" :background "#87FF5F"))
  "Face used to render text with 119th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-120
  '((t :foreground "#87FF87" :background "#87FF87"))
  "Face used to render text with 120th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-121
  '((t :foreground "#87FFAF" :background "#87FFAF"))
  "Face used to render text with 121th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-122
  '((t :foreground "#87FFD7" :background "#87FFD7"))
  "Face used to render text with 122th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-123
  '((t :foreground "#87FFFF" :background "#87FFFF"))
  "Face used to render text with 123th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-124
  '((t :foreground "#AF0000" :background "#AF0000"))
  "Face used to render text with 124th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-125
  '((t :foreground "#AF005F" :background "#AF005F"))
  "Face used to render text with 125th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-126
  '((t :foreground "#AF0087" :background "#AF0087"))
  "Face used to render text with 126th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-127
  '((t :foreground "#AF00AF" :background "#AF00AF"))
  "Face used to render text with 127th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-128
  '((t :foreground "#AF00D7" :background "#AF00D7"))
  "Face used to render text with 128th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-129
  '((t :foreground "#AF00FF" :background "#AF00FF"))
  "Face used to render text with 129th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-130
  '((t :foreground "#AF5F00" :background "#AF5F00"))
  "Face used to render text with 130th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-131
  '((t :foreground "#AF5F5F" :background "#AF5F5F"))
  "Face used to render text with 131th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-132
  '((t :foreground "#AF5F87" :background "#AF5F87"))
  "Face used to render text with 132th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-133
  '((t :foreground "#AF5FAF" :background "#AF5FAF"))
  "Face used to render text with 133th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-134
  '((t :foreground "#AF5FD7" :background "#AF5FD7"))
  "Face used to render text with 134th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-135
  '((t :foreground "#AF5FFF" :background "#AF5FFF"))
  "Face used to render text with 135th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-136
  '((t :foreground "#AF8700" :background "#AF8700"))
  "Face used to render text with 136th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-137
  '((t :foreground "#AF875F" :background "#AF875F"))
  "Face used to render text with 137th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-138
  '((t :foreground "#AF8787" :background "#AF8787"))
  "Face used to render text with 138th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-139
  '((t :foreground "#AF87AF" :background "#AF87AF"))
  "Face used to render text with 139th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-140
  '((t :foreground "#AF87D7" :background "#AF87D7"))
  "Face used to render text with 140th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-141
  '((t :foreground "#AF87FF" :background "#AF87FF"))
  "Face used to render text with 141th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-142
  '((t :foreground "#AFAF00" :background "#AFAF00"))
  "Face used to render text with 142th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-143
  '((t :foreground "#AFAF5F" :background "#AFAF5F"))
  "Face used to render text with 143th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-144
  '((t :foreground "#AFAF87" :background "#AFAF87"))
  "Face used to render text with 144th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-145
  '((t :foreground "#AFAFAF" :background "#AFAFAF"))
  "Face used to render text with 145th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-146
  '((t :foreground "#AFAFD7" :background "#AFAFD7"))
  "Face used to render text with 146th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-147
  '((t :foreground "#AFAFFF" :background "#AFAFFF"))
  "Face used to render text with 147th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-148
  '((t :foreground "#AFD700" :background "#AFD700"))
  "Face used to render text with 148th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-149
  '((t :foreground "#AFD75F" :background "#AFD75F"))
  "Face used to render text with 149th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-150
  '((t :foreground "#AFD787" :background "#AFD787"))
  "Face used to render text with 150th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-151
  '((t :foreground "#AFD7AF" :background "#AFD7AF"))
  "Face used to render text with 151th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-152
  '((t :foreground "#AFD7D7" :background "#AFD7D7"))
  "Face used to render text with 152th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-153
  '((t :foreground "#AFD7FF" :background "#AFD7FF"))
  "Face used to render text with 153th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-154
  '((t :foreground "#AFFF00" :background "#AFFF00"))
  "Face used to render text with 154th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-155
  '((t :foreground "#AFFF5F" :background "#AFFF5F"))
  "Face used to render text with 155th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-156
  '((t :foreground "#AFFF87" :background "#AFFF87"))
  "Face used to render text with 156th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-157
  '((t :foreground "#AFFFAF" :background "#AFFFAF"))
  "Face used to render text with 157th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-158
  '((t :foreground "#AFFFD7" :background "#AFFFD7"))
  "Face used to render text with 158th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-159
  '((t :foreground "#AFFFFF" :background "#AFFFFF"))
  "Face used to render text with 159th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-160
  '((t :foreground "#D70000" :background "#D70000"))
  "Face used to render text with 160th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-161
  '((t :foreground "#D7005F" :background "#D7005F"))
  "Face used to render text with 161th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-162
  '((t :foreground "#D70087" :background "#D70087"))
  "Face used to render text with 162th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-163
  '((t :foreground "#D700AF" :background "#D700AF"))
  "Face used to render text with 163th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-164
  '((t :foreground "#D700D7" :background "#D700D7"))
  "Face used to render text with 164th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-165
  '((t :foreground "#D700FF" :background "#D700FF"))
  "Face used to render text with 165th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-166
  '((t :foreground "#D75F00" :background "#D75F00"))
  "Face used to render text with 166th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-167
  '((t :foreground "#D75F5F" :background "#D75F5F"))
  "Face used to render text with 167th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-168
  '((t :foreground "#D75F87" :background "#D75F87"))
  "Face used to render text with 168th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-169
  '((t :foreground "#D75FAF" :background "#D75FAF"))
  "Face used to render text with 169th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-170
  '((t :foreground "#D75FD7" :background "#D75FD7"))
  "Face used to render text with 170th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-171
  '((t :foreground "#D75FFF" :background "#D75FFF"))
  "Face used to render text with 171th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-172
  '((t :foreground "#D78700" :background "#D78700"))
  "Face used to render text with 172th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-173
  '((t :foreground "#D7875F" :background "#D7875F"))
  "Face used to render text with 173th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-174
  '((t :foreground "#D78787" :background "#D78787"))
  "Face used to render text with 174th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-175
  '((t :foreground "#D787AF" :background "#D787AF"))
  "Face used to render text with 175th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-176
  '((t :foreground "#D787D7" :background "#D787D7"))
  "Face used to render text with 176th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-177
  '((t :foreground "#D787FF" :background "#D787FF"))
  "Face used to render text with 177th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-178
  '((t :foreground "#D7AF00" :background "#D7AF00"))
  "Face used to render text with 178th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-179
  '((t :foreground "#D7AF5F" :background "#D7AF5F"))
  "Face used to render text with 179th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-180
  '((t :foreground "#D7AF87" :background "#D7AF87"))
  "Face used to render text with 180th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-181
  '((t :foreground "#D7AFAF" :background "#D7AFAF"))
  "Face used to render text with 181th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-182
  '((t :foreground "#D7AFD7" :background "#D7AFD7"))
  "Face used to render text with 182th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-183
  '((t :foreground "#D7AFFF" :background "#D7AFFF"))
  "Face used to render text with 183th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-184
  '((t :foreground "#D7D700" :background "#D7D700"))
  "Face used to render text with 184th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-185
  '((t :foreground "#D7D75F" :background "#D7D75F"))
  "Face used to render text with 185th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-186
  '((t :foreground "#D7D787" :background "#D7D787"))
  "Face used to render text with 186th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-187
  '((t :foreground "#D7D7AF" :background "#D7D7AF"))
  "Face used to render text with 187th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-188
  '((t :foreground "#D7D7D7" :background "#D7D7D7"))
  "Face used to render text with 188th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-189
  '((t :foreground "#D7D7FF" :background "#D7D7FF"))
  "Face used to render text with 189th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-190
  '((t :foreground "#D7FF00" :background "#D7FF00"))
  "Face used to render text with 190th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-191
  '((t :foreground "#D7FF5F" :background "#D7FF5F"))
  "Face used to render text with 191th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-192
  '((t :foreground "#D7FF87" :background "#D7FF87"))
  "Face used to render text with 192th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-193
  '((t :foreground "#D7FFAF" :background "#D7FFAF"))
  "Face used to render text with 193th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-194
  '((t :foreground "#D7FFD7" :background "#D7FFD7"))
  "Face used to render text with 194th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-195
  '((t :foreground "#D7FFFF" :background "#D7FFFF"))
  "Face used to render text with 195th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-196
  '((t :foreground "#FF0000" :background "#FF0000"))
  "Face used to render text with 196th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-197
  '((t :foreground "#FF005F" :background "#FF005F"))
  "Face used to render text with 197th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-198
  '((t :foreground "#FF0087" :background "#FF0087"))
  "Face used to render text with 198th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-199
  '((t :foreground "#FF00AF" :background "#FF00AF"))
  "Face used to render text with 199th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-200
  '((t :foreground "#FF00D7" :background "#FF00D7"))
  "Face used to render text with 200th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-201
  '((t :foreground "#FF00FF" :background "#FF00FF"))
  "Face used to render text with 201th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-202
  '((t :foreground "#FF5F00" :background "#FF5F00"))
  "Face used to render text with 202th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-203
  '((t :foreground "#FF5F5F" :background "#FF5F5F"))
  "Face used to render text with 203th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-204
  '((t :foreground "#FF5F87" :background "#FF5F87"))
  "Face used to render text with 204th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-205
  '((t :foreground "#FF5FAF" :background "#FF5FAF"))
  "Face used to render text with 205th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-206
  '((t :foreground "#FF5FD7" :background "#FF5FD7"))
  "Face used to render text with 206th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-207
  '((t :foreground "#FF5FFF" :background "#FF5FFF"))
  "Face used to render text with 207th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-208
  '((t :foreground "#FF8700" :background "#FF8700"))
  "Face used to render text with 208th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-209
  '((t :foreground "#FF875F" :background "#FF875F"))
  "Face used to render text with 209th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-210
  '((t :foreground "#FF8787" :background "#FF8787"))
  "Face used to render text with 210th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-211
  '((t :foreground "#FF87AF" :background "#FF87AF"))
  "Face used to render text with 211th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-212
  '((t :foreground "#FF87D7" :background "#FF87D7"))
  "Face used to render text with 212th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-213
  '((t :foreground "#FF87FF" :background "#FF87FF"))
  "Face used to render text with 213th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-214
  '((t :foreground "#FFAF00" :background "#FFAF00"))
  "Face used to render text with 214th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-215
  '((t :foreground "#FFAF5F" :background "#FFAF5F"))
  "Face used to render text with 215th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-216
  '((t :foreground "#FFAF87" :background "#FFAF87"))
  "Face used to render text with 216th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-217
  '((t :foreground "#FFAFAF" :background "#FFAFAF"))
  "Face used to render text with 217th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-218
  '((t :foreground "#FFAFD7" :background "#FFAFD7"))
  "Face used to render text with 218th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-219
  '((t :foreground "#FFAFFF" :background "#FFAFFF"))
  "Face used to render text with 219th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-220
  '((t :foreground "#FFD700" :background "#FFD700"))
  "Face used to render text with 220th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-221
  '((t :foreground "#FFD75F" :background "#FFD75F"))
  "Face used to render text with 221th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-222
  '((t :foreground "#FFD787" :background "#FFD787"))
  "Face used to render text with 222th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-223
  '((t :foreground "#FFD7AF" :background "#FFD7AF"))
  "Face used to render text with 223th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-224
  '((t :foreground "#FFD7D7" :background "#FFD7D7"))
  "Face used to render text with 224th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-225
  '((t :foreground "#FFD7FF" :background "#FFD7FF"))
  "Face used to render text with 225th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-226
  '((t :foreground "#FFFF00" :background "#FFFF00"))
  "Face used to render text with 226th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-227
  '((t :foreground "#FFFF5F" :background "#FFFF5F"))
  "Face used to render text with 227th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-228
  '((t :foreground "#FFFF87" :background "#FFFF87"))
  "Face used to render text with 228th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-229
  '((t :foreground "#FFFFAF" :background "#FFFFAF"))
  "Face used to render text with 229th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-230
  '((t :foreground "#FFFFD7" :background "#FFFFD7"))
  "Face used to render text with 230th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-231
  '((t :foreground "#FFFFFF" :background "#FFFFFF"))
  "Face used to render text with 231th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-232
  '((t :foreground "#080808" :background "#080808"))
  "Face used to render text with 232th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-233
  '((t :foreground "#121212" :background "#121212"))
  "Face used to render text with 233th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-234
  '((t :foreground "#1C1C1C" :background "#1C1C1C"))
  "Face used to render text with 234th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-235
  '((t :foreground "#262626" :background "#262626"))
  "Face used to render text with 235th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-236
  '((t :foreground "#303030" :background "#303030"))
  "Face used to render text with 236th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-237
  '((t :foreground "#3A3A3A" :background "#3A3A3A"))
  "Face used to render text with 237th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-238
  '((t :foreground "#444444" :background "#444444"))
  "Face used to render text with 238th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-239
  '((t :foreground "#4E4E4E" :background "#4E4E4E"))
  "Face used to render text with 239th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-240
  '((t :foreground "#585858" :background "#585858"))
  "Face used to render text with 240th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-241
  '((t :foreground "#626262" :background "#626262"))
  "Face used to render text with 241th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-242
  '((t :foreground "#6C6C6C" :background "#6C6C6C"))
  "Face used to render text with 242th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-243
  '((t :foreground "#767676" :background "#767676"))
  "Face used to render text with 243th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-244
  '((t :foreground "#808080" :background "#808080"))
  "Face used to render text with 244th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-245
  '((t :foreground "#8A8A8A" :background "#8A8A8A"))
  "Face used to render text with 245th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-246
  '((t :foreground "#949494" :background "#949494"))
  "Face used to render text with 246th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-247
  '((t :foreground "#9E9E9E" :background "#9E9E9E"))
  "Face used to render text with 247th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-248
  '((t :foreground "#A8A8A8" :background "#A8A8A8"))
  "Face used to render text with 248th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-249
  '((t :foreground "#B2B2B2" :background "#B2B2B2"))
  "Face used to render text with 249th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-250
  '((t :foreground "#BCBCBC" :background "#BCBCBC"))
  "Face used to render text with 250th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-251
  '((t :foreground "#C6C6C6" :background "#C6C6C6"))
  "Face used to render text with 251th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-252
  '((t :foreground "#D0D0D0" :background "#D0D0D0"))
  "Face used to render text with 252th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-253
  '((t :foreground "#DADADA" :background "#DADADA"))
  "Face used to render text with 253th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-254
  '((t :foreground "#E4E4E4" :background "#E4E4E4"))
  "Face used to render text with 254th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-color-255
  '((t :foreground "#EEEEEE" :background "#EEEEEE"))
  "Face used to render text with 255th color of 256 color palette."
  :group 'eat-term)

(defface eat-term-font-0 '((t))
  "Default font.")

(put 'eat-term-font-default 'face-alias 'eat-term-font-0)

(defface eat-term-font-1 '((t))
  "Alternative font 1.")

(defface eat-term-font-2 '((t))
  "Alternative font 2.")

(defface eat-term-font-3 '((t))
  "Alternative font 3.")

(defface eat-term-font-4 '((t))
  "Alternative font 4.")

(defface eat-term-font-5 '((t))
  "Alternative font 5.")

(defface eat-term-font-6 '((t))
  "Alternative font 6.")

(defface eat-term-font-7 '((t))
  "Alternative font 7.")

(defface eat-term-font-8 '((t))
  "Alternative font 8.")

(defface eat-term-font-9 '((t))
  "Alternative font 9.")


;;;; Utility functions.

(defun eat--t-goto-bol (&optional n)
  "Go to the beginning of current line.

With optional argument N, go to the beginning of Nth next line if N is
positive, otherwise go to the beginning of -Nth previous line.  If the
specified position is before `point-min' or after `point-max', go to
that point.

Return the number of lines moved.

Treat LINE FEED (?\\n) as the line delimiter."
  (let ((n (or n 0)))
    (cond
     ((> n 0)
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
        moved)))))

(defun eat--t-goto-eol (&optional n)
  "Go to the end of current line.

With optional argument N, go to the end of Nth next line if N is
positive, otherwise go to the end of -Nth previous line.  If the
specified position is before `point-min' or after `point-max', go to
that point.

Return the number of lines moved.

Treat LINE FEED (?\\n) as the line delimiter."
  (let ((n (or n 0)))
    (cond
     ((>= n 0)
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
        moved)))))

(defun eat--t-bol (&optional n)
  "Return the beginning of current line.

With optional argument N, return a cons cell whose car is the
beginning of Nth next line and cdr is N, if N is positive, otherwise
return a cons cell whose car is the beginning of -Nth previous line
and cdr is N.  If the specified position is before `point-min' or
after `point-max', return a cons cell whose car is that point and cdr
is number of lines that point is away from current line.

Treat LINE FEED (?\\n) as the line delimiter."
  (save-excursion
    (let* ((n (or n 0))
           (moved (eat--t-goto-bol n)))
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
  (save-excursion
    (let* ((n (or n 0))
           (moved (eat--t-goto-eol n)))
      (cons (point) moved))))

(defun eat--t-col-motion (n)
  "Move to Nth next column.

Go to Nth next column if N is positive, otherwise go to -Nth previous
column.  If the specified position is before `point-min' or after
`point-max', go to that point.

Return the number of columns moved.

Assume all characters occupy a single column."
  (let ((point (point)))
    (cond
     ((> n 0)
      (let ((eol (car (eat--t-eol)))
            (pos (+ (point) n)))
        (goto-char (min pos eol))))
     ((< n 0)
      (let ((bol (car (eat--t-bol)))
            (pos (+ (point) n)))
        (goto-char (max pos bol)))))
    (- (point) point)))

(defun eat--t-current-col ()
  "Return the current column.

Assume all characters occupy a single column."
  (- (point) (car (eat--t-bol))))

(defun eat--t-goto-col (n)
  "Go to column N.

Return the current column after moving point.

Assume all characters occupy a single column."
  (eat--t-goto-bol)
  (eat--t-col-motion n))

(defun eat--t-repeated-insert (c n &optional face)
  "Insert C, N times.

C is a character.  FACE is the face to use, or nil."
  (let ((str (make-string n c)))
    (insert (if face (propertize str 'face face) str))))

(defun eat--t-join-long-line (&optional limit)
  "Join long line once, but don't try to go beyond LIMIT.

For example: \"*foo\\nbar\\nbaz\" is converted to \"foo*bar\\nbaz\",
where `*' indicates point."
  (if (get-char-property (point) 'eat--t-wrap-line)
      (when (< (point) (or limit (point-max)))
        (delete-char 1))
    (let ((next (next-single-char-property-change
                 (point) 'eat--t-wrap-line nil limit)))
      (goto-char next)
      (when (< (point) (or limit (point-max)))
        (delete-char 1)))))

(defun eat--t-break-long-line (threshold)
  "Break a line longer than THRESHOLD once.

For example: when THRESHOLD is 3, \"*foobarbaz\" is converted to
\"foo\\n*barbaz\", where `*' indicates point."
  (let ((loop t))
    (while (and loop (< (point) (point-max)))
      (eat--t-goto-col threshold)
      (if (eq (char-after) ?\n)
          (forward-char)
        (unless (= (point) (point-max))
          (insert-before-markers (propertize "\n" 'eat--t-wrap-line t)))
        (setq loop nil)))))


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
  (saved-cursor (eat--t-make-cur) :documentation "Saved cursor.")
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
  (bell-fn #'ignore :documentation "Function to ring the bell.")
  (input-fn #'ignore :documentation "Function to send input.")
  (set-cursor-fn #'ignore :documentation "Function to set cursor.")
  (manipulate-selection-fn
   #'ignore
   :documentation "Function to manipulate selection.")
  (set-title-fn #'ignore :documentation "Function to set title.")
  (grab-mouse-fn #'ignore :documentation "Function to grab mouse.")
  (set-focus-ev-mode-fn
   #'ignore
   :documentation "Function to set focus event mode.")
  (parser-state nil :documentation "State of parser.")
  (scroll-begin 1 :documentation "First line of scroll region.")
  (scroll-end 24 :documentation "Last line of scroll region.")
  (display nil :documentation "The display.")
  (main-display nil :documentation "Main display.

Nil when not in alternative display mode.")
  (face (eat--t-make-face) :documentation "Display attributes.")
  (auto-margin t :documentation "State of auto margin mode.")
  (ins-mode nil :documentation "State of insert mode.")
  (charset
   '(g0 (g0 . us-ascii)
        (g1 . dec-line-drawing)
        (g2 . dec-line-drawing)
        (g3 . dec-line-drawing))
   :documentation "Current character set.")
  (cur-state :default :documentation "Current state of cursor.")
  (cur-blinking-p nil :documentation "Is the cursor blinking?")
  (saved-face
   (eat--t-make-face)
   :documentation "Saved SGR attributes.")
  (bracketed-yank nil :documentation "State of bracketed yank mode.")
  (keypad-mode nil :documentation "State of keypad mode.")
  (mouse-mode t :documentation "Current mouse mode.")
  (mouse-pressed nil :documentation "Pressed mouse buttons.")
  (mouse-encoding nil :documentation "Current mouse event encoding.")
  (focus-event-mode nil :documentation "Whether to send focus event.")
  (cut-buffers (make-vector 10 nil) :documentation "Cut buffers."))

(defvar eat--t-term nil
  "The current terminal.

Don't `set' it, bind it to a value with `let'.")

(defun eat--t-reset ()
  "Reset terminal."
  (let* ((disp (eat--t-term-display eat--t-term)))
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
    (delete-region (point-min) (point-max))
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
         (cursor (eat--t-disp-cursor disp))
         (n (min (- (eat--t-disp-width disp) (eat--t-cur-x cursor))
                 (max (or n 1) 1))))
    (unless (zerop n)
      (eat--t-repeated-insert ?  (- n (eat--t-col-motion n)))
      (cl-incf (eat--t-cur-x cursor) n))))

(defun eat--t-cur-left (&optional n)
  "Move cursor N columns left.

N default to 1.  If N is out of range, place cursor at the edge of
display."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (n (min (1- (eat--t-cur-x cursor)) (max (or n 1) 1))))
    (unless (zerop n)
      (eat--t-col-motion (- n))
      (cl-decf (eat--t-cur-x cursor) n))))

(defun eat--t-cur-horizontal-abs (&optional n)
  "Move cursor to Nth column on current line.

N default to 1.  If N is out of range, place cursor at the edge of
display."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (n (min (max (or n 1) 1) (eat--t-disp-width disp))))
    (cond ((< (eat--t-cur-x cursor) n)
           (eat--t-cur-right (- n (eat--t-cur-x cursor))))
          ((< n (eat--t-cur-x cursor))
           (eat--t-cur-left (- (eat--t-cur-x cursor) n))))))

(defun eat--t-beg-of-next-line (n)
  "Move to beginning of Nth next line."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (n (min (- (eat--t-disp-height disp) (eat--t-cur-y cursor))
                 (max (or n 1) 1))))
    (eat--t-repeated-insert ?\n (- n (eat--t-goto-bol n)))
    (cl-incf (eat--t-cur-y cursor) n)
    (setf (eat--t-cur-x cursor) 1)))

(defun eat--t-beg-of-prev-line (n)
  "Move to beginning of Nth previous line."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (n (min (1- (eat--t-cur-y cursor)) (max (or n 1) 1))))
    (eat--t-goto-bol (- n))
    (cl-decf (eat--t-cur-y cursor) n)
    (setf (eat--t-cur-x cursor) 1)))

(defun eat--t-cur-down (&optional n)
  "Move cursor N lines down.

N default to 1.  If N is out of range, place cursor at the edge of
display."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (x (eat--t-cur-x cursor)))
    (eat--t-beg-of-next-line n)
    (eat--t-cur-horizontal-abs x)))

(defun eat--t-cur-up (&optional n)
  "Move cursor N lines up.

N default to 1.  If N is out of range, place cursor at the edge of
display."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (x (eat--t-cur-x cursor)))

    (eat--t-beg-of-prev-line n)
    (eat--t-cur-horizontal-abs x)))

(defun eat--t-cur-vertical-abs (&optional n)
  "Move cursor to Nth line on display.

N default to 1.  If N is out of range, place cursor at the edge of
display."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (n (min (max (or n 1) 1) (eat--t-disp-height disp))))
    (cond ((< (eat--t-cur-y cursor) n)
           (eat--t-cur-down (- n (eat--t-cur-y cursor))))
          ((< n (eat--t-cur-y cursor))
           (eat--t-cur-up (- (eat--t-cur-y cursor) n))))))

(defun eat--t-scroll-up (&optional n preserve-point)
  "Scroll up N lines, preserving cursor position.

N default to 1.  By default, don't change current line and current
column, but if PRESERVE-POINT is given and non-nil, don't move point
relative to the text and change current line accordingly."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (scroll-begin (eat--t-term-scroll-begin eat--t-term))
         (scroll-end (eat--t-term-scroll-end eat--t-term))
         (n (min (max (or n 1) 0) (1+ (- scroll-end scroll-begin)))))
    (unless (zerop n)
      (save-excursion
        (goto-char (eat--t-disp-begin disp))
        (eat--t-goto-bol (1- scroll-begin))
        (if (or (eat--t-term-main-display eat--t-term)
                (> scroll-begin 1))
            (delete-region (point) (car (eat--t-bol n)))
          (eat--t-goto-bol n)
          ;; Make sure we're at the beginning of a line, because we
          ;; might be at `point-max'.
          (unless (or (= (point) (point-min))
                      (= (char-before) ?\n))
            (insert ?\n))
          (set-marker (eat--t-disp-begin disp) (point)))
        (eat--t-goto-bol (- (1+ (- scroll-end scroll-begin)) n))
        (eat--t-repeated-insert ?\n n))
      (when (and preserve-point
                 (<= scroll-begin (eat--t-cur-y cursor) scroll-end))
        (setf (eat--t-cur-y cursor) (min (- (eat--t-cur-y cursor) n)
                                         scroll-end)))
      (let ((y (eat--t-cur-y cursor))
            (x (eat--t-cur-x cursor)))
        (eat--t-goto 1 1)
        (eat--t-goto y x)))))

(defun eat--t-scroll-down (&optional n)
  "Scroll down N lines, preserving cursor position.

N default to 1."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (scroll-begin (eat--t-term-scroll-begin eat--t-term))
         (scroll-end (eat--t-term-scroll-end eat--t-term))
         (n (min (max (or n 1) 0) (1+ (- scroll-end scroll-begin)))))
    (unless (zerop n)
      (save-excursion
        (goto-char (eat--t-disp-begin disp))
        (eat--t-goto-bol (1- scroll-begin))
        (eat--t-repeated-insert ?\n n)
        (eat--t-goto-eol (- (1+ (- scroll-end scroll-begin)) (1+ n)))
        (delete-region (point) (car (eat--t-eol n))))
      (let ((y (eat--t-cur-y cursor))
            (x (eat--t-cur-x cursor)))
        (eat--t-goto 1 1)
        (eat--t-goto y x)))))

(defun eat--t-goto (&optional y x)
  "Go to Xth column of Yth line of display.

Y and X default to 1.  Y and X are one-based.  If Y and/or X are out
of range, place cursor at the edge of display."
  (if (and (or (not y) (eql y 1))
           (or (not x) (eql x 1)))
      (let* ((disp (eat--t-term-display eat--t-term))
             (cursor (eat--t-disp-cursor disp)))
        (goto-char (eat--t-disp-begin disp))
        (setf (eat--t-cur-y cursor) 1)
        (setf (eat--t-cur-x cursor) 1)))
  (eat--t-cur-horizontal-abs 1)
  (eat--t-cur-vertical-abs y)
  (eat--t-cur-horizontal-abs x))

(defun eat--t-enable-auto-margin ()
  "Enable automatic margin."
  (setf (eat--t-term-auto-margin eat--t-term) t))

(defun eat--t-disable-auto-margin ()
  "Disable automatic margin."
  (setf (eat--t-term-auto-margin eat--t-term) nil))

(defun eat--t-set-charset (slot charset)
  "SLOT's character set to CHARSET."
  (setf (alist-get slot (cdr (eat--t-term-charset eat--t-term)))
        charset))

(defun eat--t-change-charset (charset)
  "Change character set to CHARSET.

CHARSET should be one of `g0', `g1', `g2' and `g3'."
  (setf (car (eat--t-term-charset eat--t-term)) charset))

(defun eat--t-write (str)
  "Write STR on display."
  (let* ((str
          (pcase (alist-get (car (eat--t-term-charset eat--t-term))
                            (cdr (eat--t-term-charset eat--t-term)))
            ('us-ascii
             str)
            ('dec-line-drawing
             (let ((s (copy-sequence str)))
               (dotimes (i (length s))
                 (let ((replacement (alist-get (aref s i)
                                               '((?+ . ?→)
                                                 (?, . ?←)
                                                 (?- . ?↑)
                                                 (?. . ?↓)
                                                 (?0 . ?█)
                                                 (?` . ?�)
                                                 (?a . ?▒)
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
                                                 (?~ . ?•)))))
                   (when replacement
                     (aset s i replacement))))
               s))
            (_
             str)))
         (str (propertize str 'face
                          (eat--t-face-face
                           (eat--t-term-face eat--t-term)))))
    ;; REVIEW: This probably needs to be updated.
    (let* ((disp (eat--t-term-display eat--t-term))
           (cursor (eat--t-disp-cursor disp))
           (scroll-end (eat--t-term-scroll-end eat--t-term)))
      (while (not (string-empty-p str))
        (let ((ins-count (min (- (eat--t-disp-width disp)
                                 (1- (eat--t-cur-x cursor)))
                              (length str))))
          (insert (substring str 0 ins-count))
          (setq str (substring str ins-count))
          (cl-incf (eat--t-cur-x cursor) ins-count)
          (if (eat--t-term-ins-mode eat--t-term)
              (delete-region
               (save-excursion
                 (eat--t-col-motion (- (eat--t-disp-width disp)
                                       (1- (eat--t-cur-x cursor))))
                 (point))
               (car (eat--t-eol)))
            (delete-region (point) (min (+ ins-count (point))
                                        (car (eat--t-eol)))))
          (when (> (eat--t-cur-x cursor) (eat--t-disp-width disp))
            (if (not (eat--t-term-auto-margin eat--t-term))
                (eat--t-cur-left 1)
              (unless (string-empty-p str)
                (when (= (eat--t-cur-y cursor) scroll-end)
                  ;; We need to save the point because otherwise
                  ;; `eat--t-scroll-up' would move it.
                  (save-excursion
                    (eat--t-scroll-up 1 'preserve-point)))
                (if (= (eat--t-cur-y cursor) scroll-end)
                    (eat--t-carriage-return)
                  (if (= (point) (point-max))
                      (insert (propertize "\n" 'eat--t-wrap-line t))
                    (put-text-property (point) (1+ (point))
                                       'eat--t-wrap-line t)
                    (forward-char))
                  (setf (eat--t-cur-x cursor) 1)
                  (cl-incf (eat--t-cur-y cursor)))))))))))

(defun eat--t-horizontal-tab (&optional n)
  "Go to the Nth next tabulation stop.

N default to 1."
  (let* ((n (max (or n 1) 1))
         (disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    (eat--t-cur-right (+ (- 8 (mod (1- (eat--t-cur-x cursor)) 8))
                         (* (1- n) 8)))))

(defun eat--t-horizontal-backtab (&optional n)
  "Go to the Nth previous tabulation stop.

N default to 1."
  (let* ((n (max (or n 1) 1))
         (disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    (eat--t-cur-left (+ (1+ (mod (- (eat--t-cur-x cursor) 2) 8))
                        (* (1- n) 8)))))

(defun eat--t-index (&optional n)
  "Go to the Nth next line preserving column, scrolling if necessary.

N default to 1."
  (let* ((n (max (or n 1) 0))
         (disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (scroll-begin (eat--t-term-scroll-begin eat--t-term))
         (scroll-end (eat--t-term-scroll-end eat--t-term))
         (in-scroll-region (<= (eat--t-cur-y cursor) scroll-end))
         (available-lines (- (if in-scroll-region
                                 scroll-end
                               (eat--t-disp-height disp))
                             (eat--t-cur-y cursor)))
         (scroll (min (max (- n available-lines) 0)
                      (if in-scroll-region
                          (1+ (- scroll-end scroll-begin))
                        (eat--t-disp-height disp)))))
    (unless (zerop n)
      (let ((down (- n (max (- n available-lines) 0))))
        (unless (zerop down)
          (eat--t-cur-down down)))
      (when (and (not (zerop scroll)) in-scroll-region)
        (eat--t-scroll-up scroll)))))

(defun eat--t-carriage-return ()
  "Go to column one."
  (eat--t-cur-horizontal-abs 1))

(defun eat--t-line-feed (&optional n)
  "Insert line feed (newline) N times, and go to column one.

N default to 1."
  (if (= (point) (point-max))
      (let* ((n (max (or n 1) 0))
             (disp (eat--t-term-display eat--t-term))
             (cursor (eat--t-disp-cursor disp))
             (scroll-begin (eat--t-term-scroll-begin eat--t-term))
             (scroll-end (eat--t-term-scroll-end eat--t-term))
             (in-scroll-region (<= (eat--t-cur-y cursor) scroll-end))
             (available-lines (- (if in-scroll-region
                                     scroll-end
                                   (eat--t-disp-height disp))
                                 (eat--t-cur-y cursor)))
             (scroll (min (max (- n available-lines) 0)
                          (if in-scroll-region
                              (1+ (- scroll-end scroll-begin))
                            (eat--t-disp-height disp)))))
        (unless (zerop n)
          (let ((m (- n (max (- n available-lines) 0))))
            (if (zerop m)
                (eat--t-carriage-return)
              (eat--t-repeated-insert ?\n m)
              (setf (eat--t-cur-x cursor) 1)
              (cl-incf (eat--t-cur-y cursor) m)))
          (when (and (not (zerop scroll)) in-scroll-region)
            (eat--t-scroll-up scroll))))
    (eat--t-carriage-return)
    (eat--t-index n)))

(defun eat--t-reverse-index (&optional n)
  "Go to Nth previous line preserving column, scrolling if needed.

N default to 1."
  (let* ((n (max (or n 1) 0))
         (disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (scroll-begin (eat--t-term-scroll-begin eat--t-term))
         (scroll-end (eat--t-term-scroll-end eat--t-term))
         (in-scroll-region (<= scroll-begin (eat--t-cur-y cursor)))
         (available-lines (- (eat--t-cur-y cursor)
                             (if in-scroll-region scroll-begin 1)))
         (scroll (min (max (- n available-lines) 0)
                      (1+ (- scroll-end scroll-begin)))))
    (unless (zerop n)
      (let ((up (- n (max (- n available-lines) 0))))
        (unless (zerop up)
          (eat--t-cur-up up)))
      (when (and (not (zerop scroll)) in-scroll-region)
        (eat--t-scroll-down scroll)))))

(defun eat--t-bell ()
  "Ring the bell."
  (funcall (eat--t-term-bell-fn eat--t-term) eat--t-term))

(defun eat--t-form-feed ()
  "Insert a vertical tab."
  (eat--t-index 1))

(defun eat--t-save-cur ()
  "Save current cursor position."
  (let ((disp (eat--t-term-display eat--t-term))
        (saved-face (eat--t-copy-face
                     (eat--t-term-face eat--t-term))))
    (setf (eat--t-disp-saved-cursor disp)
          (eat--t-copy-cur (eat--t-disp-cursor disp)))
    (setf (eat--t-term-saved-face eat--t-term) saved-face)
    (setf (eat--t-face-face saved-face)
          (copy-tree (eat--t-face-face saved-face)))
    (setf (eat--t-face-underline-color saved-face)
          (copy-tree (eat--t-face-underline-color saved-face)))))

(defun eat--t-restore-cur ()
  "Restore previously save cursor position."
  (let ((saved (eat--t-disp-saved-cursor
                (eat--t-term-display eat--t-term))))
    (eat--t-goto (eat--t-cur-y saved) (eat--t-cur-x saved))
    (setf (eat--t-term-face eat--t-term)
          (eat--t-term-saved-face eat--t-term))))

(defun eat--t-erase-in-line (&optional n)
  "Erase part of current line, but don't move cursor.

N defaults to 0.  When N is 0, erase cursor to end of line.  When N is
1, erase beginning of line to cursor.  When N is 2, erase whole line."
  (let* ((disp (eat--t-term-display eat--t-term))
         (face (eat--t-term-face eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    (when (or (not n) (> n 2) (memq n '(0 2)))
      ;; Erase point to end of line.
      (delete-region
       (point)
       (or (save-excursion
             (when-let ((pos (search-forward "\n" nil t)))
               (1- pos)))
           (point-max)))
      (when (eat--t-face-bg face)
        (save-excursion
          (eat--t-repeated-insert ?  (1+ (- (eat--t-disp-width disp)
                                            (eat--t-cur-x cursor)))
                                  (and (eat--t-face-bg face)
                                       (eat--t-face-face face))))))
    (when (memq n '(1 2))
      ;; Erase beginning of line to point.
      (let ((x (eat--t-cur-x cursor)))
        (if (= (point) (point-max))
            (progn
              (delete-region (car (eat--t-bol)) (point))
              (eat--t-repeated-insert ?  (1- x))))
        (delete-region (car (eat--t-bol)) (if (= (point) (point-max))
                                              (point)
                                            (1+ (point))))
        (eat--t-repeated-insert ?  x (and (eat--t-face-bg face)
                                          (eat--t-face-face face)))
        (backward-char)))))

(defun eat--t-erase-in-disp (&optional n)
  "Erase part of display.

N defaults to 0.  When N is 0, erase cursor to end of display.  When N
is 1, erase beginning of display to cursor.  In both on the previous
cases, don't move cursor.  When N is 2, erase display and reset cursor
to (1, 1).  When N is 3, also erase the scrollback."
  (let* ((disp (eat--t-term-display eat--t-term))
         (face (eat--t-term-face eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    (pcase n
      ((or 0 'nil (pred (< 3)))
       (delete-region (point) (point-max))
       (when (eat--t-face-bg face)
         (let ((pos (point)))
           (eat--t-repeated-insert ?  (1+ (- (eat--t-disp-width disp)
                                             (eat--t-cur-x cursor)))
                                   (eat--t-face-face face))
           (dotimes (_ (- (eat--t-disp-height disp)
                          (eat--t-cur-y cursor)))
             (insert ?\n)
             (eat--t-repeated-insert ?  (eat--t-disp-width disp)
                                     (eat--t-face-face face)))
           (goto-char pos))))
      (1
       (let* ((y (eat--t-cur-y cursor))
              (x (eat--t-cur-x cursor))
              (incl-point (/= (point) (point-max))))
         (delete-region (eat--t-disp-begin disp)
                        (if incl-point (1+ (point)) (point)))
         (if (not (eat--t-face-bg face))
             (eat--t-repeated-insert ?\n (1- y))
           (dotimes (_ (1- y))
             (eat--t-repeated-insert ?  (eat--t-disp-width disp)
                                     (eat--t-face-face face))
             (insert ?\n)))
         (eat--t-repeated-insert ?  (if incl-point x (1- x))
                                 (and (eat--t-face-bg face)
                                      (eat--t-face-face face)))
         (when incl-point
           (backward-char))))
      (2
       (eat--t-goto 1 1)
       (delete-region (point) (point-max))
       (when (eat--t-face-bg face)
         (let ((pos (point)))
           (dotimes (i (eat--t-disp-height disp))
             (unless (zerop i)
               (insert ?\n))
             (eat--t-repeated-insert ?  (eat--t-disp-width disp)
                                     (eat--t-face-face face)))
           (goto-char pos))))
      (3
       (eat--t-goto 1 1)
       (delete-region (point-min) (point-max))
       (when (eat--t-face-bg face)
         (let ((pos (point)))
           (dotimes (i (eat--t-disp-height disp))
             (unless (zerop i)
               (insert ?\n))
             (eat--t-repeated-insert ?  (eat--t-disp-width disp)
                                     (eat--t-face-face face)))
           (goto-char pos)))))))

(defun eat--t-device-status-report ()
  "Send the current Y and X coordinate to client."
  (let ((cursor (eat--t-disp-cursor
                 (eat--t-term-display eat--t-term))))
    (funcall (eat--t-term-input-fn eat--t-term) eat--t-term
             (format "\e[%i;%iR" (eat--t-cur-y cursor)
                     (eat--t-cur-x cursor)))))

(defun eat--t-set-cursor-state (state)
  "Set cursor state to STATE.

STATE one of the `:default', `:invisible', `:very-visible'."
  (unless (eq (eat--t-term-cur-state eat--t-term) state)
    (setf (eat--t-term-cur-state eat--t-term) state)
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
  "Make the cursor blink."
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
  (when eat-enable-alternative-display
    (unless (eat--t-term-main-display eat--t-term)
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
        (delete-region (point-min) (point-max))
        (eat--t-goto 1 1)))))

(defun eat--t-disable-alt-disp (&optional dont-move-cursor)
  "Disable alternative display.

If DONT-MOVE-CURSOR is non-nil, don't move cursor from current
position."
  (when (eat--t-term-main-display eat--t-term)
    (let* ((main-disp (eat--t-term-main-display eat--t-term))
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
      (delete-region (point-min) (point-max))
      (insert (cdr main-disp))
      (setf (eat--t-disp-begin (car main-disp))
            (copy-marker (+ (point-min)
                            (eat--t-disp-begin (car main-disp)))))
      (setf (eat--t-disp-old-begin (car main-disp))
            (copy-marker (+ (point-min)
                            (eat--t-disp-old-begin (car main-disp)))))
      (setf (eat--t-cur-position (eat--t-disp-cursor (car main-disp)))
            (copy-marker (+ (point-min)
                            (eat--t-cur-position
                             (eat--t-disp-cursor (car main-disp))))))
      (setf (eat--t-term-display eat--t-term) (car main-disp))
      (setf (eat--t-term-main-display eat--t-term) nil)
      (eat--t-resize width height)
      (goto-char (eat--t-cur-position
                  (eat--t-disp-cursor
                   (eat--t-term-display eat--t-term))))
      (when dont-move-cursor
        (eat--t-goto old-y old-x)))))

(defun eat--t-insert-char (n)
  "Insert N empty (space) characters, preserving cursor."
  (let* ((disp (eat--t-term-display eat--t-term))
         (face (eat--t-term-face eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (n (min (- (eat--t-disp-width disp)
                    (1- (eat--t-cur-x cursor)))
                 (max (or n 1) 0))))
    (unless (zerop n)
      (save-excursion
        (eat--t-repeated-insert ?  n (and (eat--t-face-bg face)
                                          (eat--t-face-face face)))
        (eat--t-col-motion (- (eat--t-disp-width disp)
                              (+ (1- (eat--t-cur-x cursor)) n)))
        (delete-region (point) (car (eat--t-eol)))))))

(defun eat--t-delete-char (n)
  "Delete N characters, preserving cursor."
  (let* ((disp (eat--t-term-display eat--t-term))
         (face (eat--t-term-face eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (n (min (- (eat--t-disp-width disp)
                    (1- (eat--t-cur-x cursor)))
                 (max (or n 1) 0))))
    (unless (zerop n)
      (save-excursion
        (let ((m (point)))
          (eat--t-col-motion n)
          (delete-region m (point))
          (when (eat--t-face-bg face)
            (save-excursion
              (eat--t-goto-eol)
              (let ((empty (1+ (- (eat--t-disp-width disp)
                                  (eat--t-cur-x cursor)
                                  (- (point) m)))))
                (when (> empty n)
                  (eat--t-repeated-insert ?  (- empty n)))
                (eat--t-repeated-insert
                 ?  (min empty n) (eat--t-face-face face))))))))))

(defun eat--t-erase-char (n)
  "Make next N character cells empty, preserving cursor."
  (let* ((disp (eat--t-term-display eat--t-term))
         (face (eat--t-term-face eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (n (min (- (eat--t-disp-width disp)
                    (1- (eat--t-cur-x cursor)))
                 (max (or n 1) 0))))
    (unless (zerop n)
      (save-excursion
        (let ((m (point)))
          (eat--t-col-motion n)
          (delete-region m (point))
          (eat--t-repeated-insert
           ?  n (and (eat--t-face-bg face)
                     (eat--t-face-face face)))
          (eat--t-col-motion
           (- (eat--t-disp-width disp)
              (+ (1- (eat--t-cur-x cursor)) n))))))))

(defun eat--t-insert-line (n)
  "Insert N empty lines, preserving cursor."
  (let* ((disp (eat--t-term-display eat--t-term))
         (face (eat--t-term-face eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (scroll-begin (eat--t-term-scroll-begin eat--t-term))
         (scroll-end (eat--t-term-scroll-end eat--t-term))
         (n (min (- (1+ (- scroll-end scroll-begin))
                    (1- (eat--t-cur-y cursor)))
                 (max (or n 1) 0))))
    (when (and (<= scroll-begin (eat--t-cur-y cursor) scroll-end)
               (not (zerop n)))
      (goto-char
       (prog1
           (progn
             (eat--t-goto-bol)
             (eat--t-repeated-insert ?  (1- (eat--t-cur-x cursor))
                                     (and (eat--t-face-bg face)
                                          (eat--t-face-face face)))
             (point))
         (if (not (eat--t-face-bg face))
             (eat--t-repeated-insert ?\n n)
           (dotimes (i n)
             (eat--t-repeated-insert
              ?  (if (not (zerop i))
                     (eat--t-disp-width disp)
                   (1+ (- (eat--t-disp-width disp)
                          (eat--t-cur-x cursor))))
              (eat--t-face-face face))
             (insert ?\n)))
         (eat--t-goto-eol (- (1+ (- scroll-end scroll-begin))
                             (+ (- (eat--t-cur-y cursor)
                                   (1- scroll-begin))
                                n)))
         (delete-region (point) (car (eat--t-eol n))))))))

(defun eat--t-delete-line (n)
  "Delete N lines, preserving cursor."
  (let* ((disp (eat--t-term-display eat--t-term))
         (face (eat--t-term-face eat--t-term))
         (cursor (eat--t-disp-cursor disp))
         (x (eat--t-cur-x cursor))
         (scroll-begin (eat--t-term-scroll-begin eat--t-term))
         (scroll-end (eat--t-term-scroll-end eat--t-term))
         (n (min (- (1+ (- scroll-end scroll-begin))
                    (1- (eat--t-cur-y cursor)))
                 (max (or n 1) 0))))
    (when (and (<= scroll-begin (eat--t-cur-y cursor) scroll-end)
               (not (zerop n)))
      (eat--t-goto-bol)
      (save-excursion
        (let ((m (point)))
          (eat--t-goto-bol n)
          (delete-region m (point))))
      (let ((pos (point))
            (move (- (1+ (- scroll-end scroll-begin))
                     (- (+ (eat--t-cur-y cursor) n)
                        (1- scroll-begin)))))
        (eat--t-repeated-insert ?\n (- move (eat--t-goto-eol move)))
        (if (not (eat--t-face-bg face))
            (eat--t-repeated-insert ?\n n)
          (dotimes (_ n)
            (insert ?\n)
            (eat--t-repeated-insert ?  (eat--t-disp-width disp)
                                    (eat--t-face-face face))))
        (goto-char pos))
      (eat--t-repeated-insert
       ?  (- (1- x) (eat--t-col-motion (1- x)))))))

(defun eat--t-repeat-last-char (&optional n)
  "Repeat last character N times."
  (let ((disp (eat--t-term-display eat--t-term)))
    (let ((char
           (when (< (eat--t-disp-begin disp) (point))
             (if (get-text-property (1- (point)) 'eat--t-wrap-line)
                 (when (< (eat--t-disp-begin disp) (1- (point)))
                   (char-before (1- (point))))
               (char-before)))))
      (when (and char (/= char ?\n))
        (eat--t-write (make-string n char))))))

(defun eat--t-change-scroll-region (&optional top bottom)
  "Change the scroll region from lines TOP to BOTTOM (inclusive).

TOP defaults to 1 and BOTTOM defaults to the height of the display."
  (let* ((disp (eat--t-term-display eat--t-term))
         (top (or top 1))
         (bottom (or bottom (eat--t-disp-height disp))))
    (when (and (<= 1 top)
               (< top bottom)
               (<= bottom (eat--t-disp-height disp)))
      (setf (eat--t-term-scroll-begin eat--t-term) top)
      (setf (eat--t-term-scroll-end eat--t-term) bottom)
      (eat--t-goto 1 1))))

(defun eat--t-insert-mode ()
  "Enable insert mode and disable replace mode."
  (setf (eat--t-term-ins-mode eat--t-term) t))

(defun eat--t-replace-mode ()
  "Enable replace mode and disable insert mode."
  (setf (eat--t-term-ins-mode eat--t-term) nil))

(defun eat--t-set-sgr-params (params)
  "Set SGR parameters PARAMS."
  (let ((params (or params '((0))))
        (face (eat--t-term-face eat--t-term)))
    (while params
      (pcase (pop params)
        ('(0)
         (setf (eat--t-face-fg face) nil)
         (setf (eat--t-face-bg face) nil)
         (setf (eat--t-face-intensity face) nil)
         (setf (eat--t-face-italic face) nil)
         (setf (eat--t-face-underline face) nil)
         (setf (eat--t-face-underline-color face) nil)
         (setf (eat--t-face-crossed face) nil)
         (setf (eat--t-face-conceal face) nil)
         (setf (eat--t-face-inverse face) nil)
         (setf (eat--t-face-blink face) nil)
         (setf (eat--t-face-font face) 'eat-term-font-0))
        ('(1)
         (setf (eat--t-face-intensity face) 'eat-term-bold))
        ('(2)
         (setf (eat--t-face-intensity face) 'eat-term-faint))
        ('(3)
         (setf (eat--t-face-italic face) 'eat-term-italic))
        ('(4)
         (setf (eat--t-face-underline face) t))
        ('(4 0)
         (setf (eat--t-face-underline face) nil))
        ('(4 1)
         (setf (eat--t-face-underline face) 'line))
        ('(4 2)
         (setf (eat--t-face-underline face) 'line))
        ('(4 3)
         (setf (eat--t-face-underline face) 'wave))
        ('(4 4)
         (setf (eat--t-face-underline face) 'wave))
        ('(4 5)
         (setf (eat--t-face-underline face) 'wave))
        ('(5)
         (setf (eat--t-face-blink face) 'eat-term-slow-blink))
        ('(6)
         (setf (eat--t-face-blink face) 'eat-term-fast-blink))
        ('(7)
         (setf (eat--t-face-inverse face) t))
        ('(8)
         (setf (eat--t-face-conceal face) t))
        ('(9)
         (setf (eat--t-face-crossed face) t))
        (`(,(and (pred (lambda (font) (<= 10 font 19)))
                 font))
         (setf (eat--t-face-font face)
               (intern (format "eat-term-font-%i" (- font 10)))))
        ('(21)
         (setf (eat--t-face-underline face) 'line))
        ('(22)
         (setf (eat--t-face-intensity face) nil))
        ('(23)
         (setf (eat--t-face-italic face) nil))
        ('(24)
         (setf (eat--t-face-underline face) nil))
        ('(25)
         (setf (eat--t-face-blink face) nil))
        ('(27)
         (setf (eat--t-face-inverse face) nil))
        ('(28)
         (setf (eat--t-face-conceal face) nil))
        ('(29)
         (setf (eat--t-face-crossed face) nil))
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
         (setf (eat--t-face-fg face) nil))
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
         (setf (eat--t-face-bg face) nil))
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
         (setf (eat--t-face-underline-color face) nil))
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
    (setf (eat--t-face-face face)
          `(,@(when-let ((fg (or (if (eat--t-face-conceal face)
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
            ,@(when-let ((bg (or (eat--t-face-bg face)
                                 (and (eat--t-face-inverse face)
                                      (face-background 'default)))))
                (list (if (eat--t-face-inverse face)
                          :foreground
                        :background)
                      bg))
            ,@(when-let ((underline (eat--t-face-underline face)))
                (list
                 :underline
                 (list :color (eat--t-face-underline-color face)
                       :style underline)))
            ,@(when-let ((crossed (eat--t-face-crossed face)))
                ;; TODO: How about colors?
                `(:strike-through t))
            :inherit
            (,@(when-let ((intensity (eat--t-face-intensity face)))
                 (list intensity))
             ,@(when-let ((italic (eat--t-face-italic face)))
                 (list italic))
             ,@(when-let ((blink (eat--t-face-blink face)))
                 (list blink))
             ,(eat--t-face-font face))))))

(defun eat--t-enable-keypad ()
  "Enable keypad."
  (setf (eat--t-term-keypad-mode eat--t-term) t))

(defun eat--t-disable-keypad ()
  "Disable keypad."
  (setf (eat--t-term-keypad-mode eat--t-term) nil))

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
  (unless mode
    (eat--t-disable-sgr-mouse-encoding))
  (when (or (not mode)
            (eq mode 'x10))
    (setf (eat--t-term-mouse-pressed eat--t-term) nil))
  (funcall (eat--t-term-grab-mouse-fn eat--t-term) eat--t-term
           (pcase mode
             ('x10 :click)
             ('normal :modifier-click)
             ('button-event :drag)
             ('any-event :all))))

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
  (setf (eat--t-term-focus-event-mode eat--t-term) t)
  (funcall (eat--t-term-set-focus-ev-mode-fn eat--t-term) eat--t-term
           t))

(defun eat--t-disable-focus-event ()
  "Disable sending focus events."
  (setf (eat--t-term-focus-event-mode eat--t-term) nil)
  (funcall (eat--t-term-set-focus-ev-mode-fn eat--t-term) eat--t-term
           nil))

(defun eat--t-set-title (title)
  "Set the title of terminal to TITLE."
  (setf (eat--t-term-title eat--t-term) title)
  (funcall (eat--t-term-set-title-fn eat--t-term) eat--t-term title))

(defun eat--t-send-device-attrs (params format)
  "Return device attributes.

PARAMS is the parameter list and FORMAT is the format of parameters in
output."
  (let ((params (or params '((0)))))
    (pcase format
      ('nil
       (when (= (caar params) 0)
         (funcall (eat--t-term-input-fn eat--t-term) eat--t-term
                  "\e[?1;2c")))
      (?>
       (when (= (caar params) 0)
         (funcall (eat--t-term-input-fn eat--t-term) eat--t-term
                  "\e[>0;242;0c"))))))

(defun eat--t-report-foreground-color ()
  "Report the current default foreground color to the client."
  (funcall
   (eat--t-term-input-fn eat--t-term) eat--t-term
   (format "\e]10;%s\e\\"
           (mapconcat (lambda (n) (format "%04x" n))
                      (color-values (face-foreground 'default))
                      "/"))))

(defun eat--t-report-background-color ()
  "Report the current default background color to the client."
  (funcall
   (eat--t-term-input-fn eat--t-term) eat--t-term
   (format "\e]11;%s\e\\"
           (mapconcat (lambda (n) (format "%04x" n))
                      (color-values (face-background 'default))
                      "/"))))

(defun eat--t-manipulate-selection (targets data)
  "Set and send current selection.

TARGETS is a string containing zero or more characters from the set
`c', `p', `q', `s', `0', `1', `2', `3', `4', `5', `6', `7', `8', `9'.
DATA is the selection data encoded in base64."
  (when (string-empty-p targets)
    (setq targets "s0"))
  (if (string= data "?")
      (funcall
       (eat--t-term-input-fn eat--t-term) eat--t-term
       (let ((str nil)
             (source nil)
             (n 0))
         (while (and (not str) (< n (length targets)))
           (setq
            str
            (pcase (aref targets n)
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
              ((and (pred (<= ?0))
                    (pred (>= ?9))
                    i)
               (aref (eat--t-term-cut-buffers eat--t-term)
                     (- i ?0)))))
           (when str
             (setq source (string (aref targets n))))
           (cl-incf n))
         (unless str
           (setq str "")
           (setq source ""))
         (format "\e]52;%s;%s\e\\" source
                 (base64-encode-string str))))
    (let ((str (ignore-error error
                 (decode-coding-string (base64-decode-string data)
                                       locale-coding-system))))
      (seq-doseq (target targets)
        (pcase target
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
          ((and (pred (<= ?0))
                (pred (>= ?9))
                i)
           (aset (eat--t-term-cut-buffers eat--t-term) (- i ?0)
                 str)))))))

(defun eat--t-set-modes (params format)
  "Set modes according to PARAMS in format FORMAT."
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
      (pcase (eat--t-term-parser-state eat--t-term)
        ('nil
         (let ((match (string-match (rx (or ?\0 ?\a ?\b ?\t ?\n ?\v
                                            ?\f ?\r ?\C-n ?\C-o ?\e
                                            #x7f))
                                    output index)))
           (if (not match)
               (progn
                 (eat--t-write (substring output index))
                 (setq index (length output)))
             (if (/= match index)
                 (progn
                   (eat--t-write (substring output index match))
                   (setq index match))
               (cl-incf index)
               (pcase (aref output (1- index))
                 (?\a
                  (eat--t-bell))
                 (?\b
                  (eat--t-cur-left 1))
                 (?\t
                  (eat--t-horizontal-tab 1))
                 (?\n
                  (eat--t-line-feed 1))
                 (?\v
                  (eat--t-index 1))
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
                  (setf (eat--t-term-parser-state eat--t-term)
                        '(read-esc))))))))
        ('(read-esc)
         (let ((type (aref output index)))
           (cl-incf index)
           (setf (eat--t-term-parser-state eat--t-term) nil)
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
              (eat--t-index 1))
             ;; ESC E.
             (?E
              (eat--t-line-feed 1))
             ;; ESC M.
             (?M
              (eat--t-reverse-index 1))
             ;; ESC P, or DCS.
             (?P
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-dcs "")))
             ;; ESC X, or SOS.
             (?X
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-sos "")))
             ;; ESC [, or CSI.
             (?\[
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-csi "")))
             ;; ESC ], or OSC.
             (?\]
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-osc "")))
             ;; ESC ^, or PM.
             (?^
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-pm "")))
             ;; ESC _, or APC.
             (?_
              (setf (eat--t-term-parser-state eat--t-term)
                    '(read-apc "")))
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
                            (rx (zero-or-more (any (?  . ?/)))
                                string-end)
                            str)
                       (setq str (substring
                                  str 0 (match-beginning 0)))
                       (setq intermediate-bytes
                             (match-string 0 str))))
                   (when (and (not (string-empty-p str))
                              (= (aref str 0) ??))
                     (setq format ??)
                     (setq str (substring str 1)))
                   (when (and (not (string-empty-p str))
                              (= (aref str 0) ?>))
                     (setq format ?>)
                     (setq str (substring str 1)))
                   (when (and (not (string-empty-p str))
                              (= (aref str 0) ?=))
                     (setq format ?=)
                     (setq str (substring str 1)))
                   (setq index (match-end 0))
                   (list
                    (concat intermediate-bytes
                            (match-string 0 output))
                    format
                    (cond
                     ((string-empty-p str) nil)
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
               ;; CSI ... h
               ;; CSI ? ... h
               (`("h" ,format ,(and (pred listp) params))
                (eat--t-set-modes params format))
               ;; CSI ... l
               ;; CSI ? ... l
               (`("l" ,format ,(and (pred listp) params))
                (eat--t-reset-modes params format))
               ;; CSI ... m
               (`("m" nil ,(and (pred listp) params))
                (eat--t-set-sgr-params params))
               ;; CSI 6 n
               ('("n" nil ((6)))
                (eat--t-device-status-report))
               ;; CSI <n> ; <n> r
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
         (let ((match (string-match (if (eq state 'read-osc)
                                        (rx (or ?\a ?\\))
                                      (rx ?\\))
                                    output index)))
           (if (not match)
               (progn
                 (setf (eat--t-term-parser-state eat--t-term)
                       `(,state ,(concat buf (substring output
                                                        index))))
                 (setq index (length output)))
             (let ((str (concat buf (substring output index
                                               match))))
               (setq index (match-end 0))
               (if (and (= (aref output match) ?\\)
                        (not (or (zerop (length str))
                                 (= (aref str (1- (length str)))
                                    ?\e))))
                   (setf (eat--t-term-parser-state eat--t-term)
                         `(,state ,(concat str "\\")))
                 (when (= (aref output match) ?\\)
                   (setq str (substring str 0 (1- (length str)))))
                 (setf (eat--t-term-parser-state eat--t-term) nil)
                 (pcase state
                   ('read-osc
                    (pcase str
                      ;; OSC 0 ; <t> ST.
                      ;; OSC 2 ; <t> ST.
                      ((rx string-start (or ?0 ?2) ?\;
                           (let title (zero-or-more anything))
                           string-end)
                       (eat--t-set-title title))
                      ;; OSC 10 ; ? ST.
                      ("10;?"
                       (eat--t-report-foreground-color))
                      ;; OSC 11 ; ? ST.
                      ("11;?"
                       (eat--t-report-background-color))
                      ;; OSC 52 ; <t> ; <s> ST.
                      ((rx string-start "52;"
                           (let targets
                             (zero-or-more (any ?c ?p ?q ?s
                                                (?0 . ?9))))
                           ";"
                           (let data (zero-or-more anything))
                           string-end)
                       (eat--t-manipulate-selection
                        targets data))))))))))
        (`(read-charset-standard ,slot ,buf)
         (let ((match (string-match (rx (any ?0 ?2 ?4 ?5 ?6 ?7 ?9 ?<
                                             ?= ?> ?? ?A ?B ?C ?E ?H
                                             ?K ?Q ?R ?Y ?Z ?f))
                                    output index)))
           (if (not match)
               (progn
                 (setf (eat--t-term-parser-state eat--t-term)
                       `(read-charset-standard
                         ,slot ,(concat buf (substring
                                             output index))))
                 (setq index (length output)))
             (let ((str (concat buf (substring output index
                                               (match-end 0)))))
               (setq index (match-end 0))
               (setf (eat--t-term-parser-state eat--t-term) nil)
               (pcase str
                 ;; ESC ( 0
                 ;; ESC ) 0
                 ;; ESC * 0
                 ;; ESC + 0
                 ("0"
                  (eat--t-set-charset slot 'dec-line-drawing))
                 ;; ESC ( B
                 ;; ESC ) B
                 ;; ESC * B
                 ;; ESC + B
                 ("B"
                  (eat--t-set-charset slot 'us-ascii)))))))
        (`(read-charset-vt300 ,_slot)
         (let ((_charset (aref output index)))
           (cl-incf index)
           (setf (eat--t-term-parser-state eat--t-term) nil)
           (pcase charset
             ;; IGNORED.  TODO.
             )))))))

(defun eat--t-resize (width height)
  "Resize terminal to WIDTH x HEIGHT."
  (let* ((disp (eat--t-term-display eat--t-term))
         (cursor (eat--t-disp-cursor disp)))
    (when (and (>= width 1)
               (>= height 1)
               (or (not (eq (eat--t-disp-width disp) width))
                   (not (eq (eat--t-disp-height disp) height))))
      (save-excursion
        (setf (eat--t-disp-width disp) width)
        (setf (eat--t-disp-height disp) height)
        (setf (eat--t-term-scroll-begin eat--t-term) 1)
        (setf (eat--t-term-scroll-end eat--t-term)
              (eat--t-disp-height disp))
        (goto-char (eat--t-disp-begin disp))
        (if (eat--t-term-main-display eat--t-term)
            (dotimes (l height)
              (eat--t-col-motion width)
              (delete-region (point) (car (eat--t-eol)))
              (if (< (1+ l) height)
                  (eat--t-goto-bol 1)
                (delete-region (point) (point-max))
                (let ((y (eat--t-cur-y cursor))
                      (x (eat--t-cur-x cursor)))
                  (eat--t-goto 1 1)
                  (eat--t-goto y x)))))
        (unless (bobp)
          (backward-char))
        (while (not (eobp))
          (eat--t-join-long-line))
        (goto-char (eat--t-disp-begin disp))
        (while (not (eobp))
          (eat--t-break-long-line (eat--t-disp-width disp)))
        (goto-char (eat--t-cur-position cursor))
        (let* ((disp-begin (car (eat--t-bol (- (1- height))))))
          (when (< (eat--t-disp-begin disp) disp-begin)
            (goto-char (max (- (eat--t-disp-begin disp) 1)
                            (point-min)))
            (set-marker (eat--t-disp-begin disp) disp-begin)
            (while (< (point) (1- (eat--t-disp-begin disp)))
              (eat--t-join-long-line
               (1- (eat--t-disp-begin disp))))))
        (goto-char (eat--t-cur-position cursor))
        (setf (eat--t-cur-x cursor) (1+ (eat--t-current-col)))
        (goto-char (eat--t-disp-begin disp))
        (setf (eat--t-cur-y cursor)
              (let ((y 0))
                (while (< (point) (eat--t-cur-position cursor))
                  (condition-case nil
                      (search-forward "\n")
                    (search-failed
                     (goto-char (point-max))))
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
          (goto-char (eat--t-disp-begin
                      (eat--t-term-display eat--t-term)))
          (when (< (point-min) (point))
            (backward-char))
          (while (not (eobp))
            (eat--t-join-long-line)))))))

(defun eat-term-reset (terminal)
  "Reset TERMINAL."
  (let ((inhibit-quit t)
        (eat--t-term terminal))
    (with-current-buffer (eat--t-term-buffer eat--t-term)
      (save-excursion
        (save-restriction
          (narrow-to-region (eat--t-term-begin eat--t-term)
                            (eat--t-term-end eat--t-term))
          (eat--t-reset)
          (set-marker (eat--t-cur-position
                       (eat--t-disp-cursor
                        (eat--t-term-display eat--t-term)))
                      (point))
          (set-marker (eat--t-term-begin eat--t-term) (point-min))
          (set-marker (eat--t-term-end eat--t-term) (point-max)))))))

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

(defun eat-term-title (terminal)
  "Return the current title of TERMINAL."
  (eat--t-term-title terminal))

(defun eat-term-set-title-function (terminal)
  "Return the function used to set the title of TERMINAL.

The function is called with two arguments, TERMINAL and the new title
of TERMINAL.  The function should not change point and buffer
restriction.

To set it, use (`setf' (`eat-term-set-title-function' TERMINAL)
FUNCTION), where FUNCTION is the function to set title."
  (eat--t-term-set-title-fn terminal))

(gv-define-setter eat-term-set-title-function (function terminal)
  `(setf (eat--t-term-set-title-fn ,terminal) ,function))

(defun eat-term-grab-mouse-function (terminal)
  "Return the function used to grab the mouse.

The function is called with two arguments, TERMINAL and a symbol MODE
describing the new mouse mode MODE.  The function should not change
point and buffer restriction.  MODE can be one of the following:

  nil                 Disable mouse.
  `:click'              Pass `mouse-1', `mouse-2', and `mouse-3'
                        clicks.
  `:modifier-click'     Pass all mouse clicks, including `control',
                        `meta' and `shift' modifiers.
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

The function is called with a single argument TERMINAL.

To set it, use (`setf' (`eat-term-ring-bell-function' TERMINAL)
FUNCTION), where FUNCTION is the function to ring the bell."
  (eat--t-term-manipulate-selection-fn terminal))

(gv-define-setter eat-term-ring-bell-function (function terminal)
  `(setf (eat--t-term-bell-fn ,terminal) ,function))

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
    (if (> (eat--t-cur-x cursor) (eat--t-disp-width disp))
        (1- (eat--t-cur-position cursor))
      (eat--t-cur-position cursor))))

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

(defun eat-term-process-output (terminal output)
  "Process OUTPUT from client and show it on TERMINAL's display."
  (let ((inhibit-quit t))
    (eat--t-with-env terminal
      (eat--t-handle-output output))))

(defun eat-term-redisplay (terminal)
  "Prepare TERMINAL for displaying."
  (let ((inhibit-quit t))
    (eat--t-with-env terminal
      (let* ((disp (eat--t-term-display eat--t-term)))
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
                (funcall (eat--t-term-input-fn terminal) terminal str)))
      (dotimes (_ (or n 1))
        (pcase (or event last-command-event)
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
                (pcase (event-modifiers ev)
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
               (if tmp (setq char (car tmp)))
               (and (symbolp char)
                    (setq tmp (get char 'ascii-character))
                    (setq char tmp))))
           (when (numberp char)
             (let ((base (event-basic-type char))
                   (mods (event-modifiers char)))
               (when (memq 'control mods)
                 (setq mods (delq 'shift mods)))
               (let ((ch (event-convert-list
                          (append (remq 'meta mods) (list base)))))
                 (send (cond
                        ((and (memq 'meta mods) (memq ch '(?\[ ?O)))
                         "\e")
                        (t
                         (format (if (memq 'meta mods) "\e%c" "%c")
                                 (pcase ch
                                   (?\C-\  ?\C-@)
                                   (?\C-/ ?\C-?)
                                   (?\C-- ?\C-_)
                                   (c c))))))))))
          ((and mouse
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
           (when (eat--t-term-mouse-mode terminal)
             (let* ((modifiers (event-modifiers mouse))
                    (pos (if (memq 'drag modifiers)
                             (event-end mouse)
                           (event-start mouse)))
                    (x (1+ (car (posn-col-row pos))))
                    (y (1+ (cdr (posn-col-row pos))))
                    (button
                     (let ((b (aref
                               [0 1 2 64 65 66 67 128 129 130 131]
                               (1- mouse-num))))
                       (when (memq 'shift modifiers)
                         (cl-incf b 4)
                         (setq b (+ b 4)))
                       (when (memq 'meta modifiers)
                         (cl-incf b 8))
                       (when (memq 'control modifiers)
                         (cl-incf b 16))
                       b)))
               (when ref-pos
                 (cl-decf x (car (posn-col-row ref-pos)))
                 (cl-decf y (cdr (posn-col-row ref-pos))))
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
                    ((memq 'down modifiers)
                     (when (< (logand button) 3)
                       (setf (eat--t-term-mouse-pressed terminal)
                             (nconc (eat--t-term-mouse-pressed terminal)
                                    (list button))))
                     (send
                      (if (eq (eat--t-term-mouse-encoding terminal)
                              'sgr)
                          (format "\e[<%i;%i;%iM" button x y)
                        (format "\e[M%c%c%c" (+ button 32) (+ x 32)
                                (+ y 32)))))
                    ((and (or (memq 'click modifiers)
                              (memq 'drag modifiers))
                          (< mouse-num 3))
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
                    (t
                     (send
                      (if (eq (eat--t-term-mouse-encoding terminal)
                              'sgr)
                          (format "\e[<%i;%i;%im" button x y)
                        (format "\e[M%c%c%c" (+ button 32) (+ x 32)
                                (+ y 32)))))))))))
          ((and (pred mouse-movement-p)
                movement)
           (when (memq (eat--t-term-mouse-mode terminal)
                       '(button-event any-event))
             (let* ((pos (event-start movement))
                    (x (1+ (car (posn-col-row pos))))
                    (y (1+ (cdr (posn-col-row pos))))
                    (button
                     (if (car (eat--t-term-mouse-pressed terminal))
                         (+ (car (eat--t-term-mouse-pressed terminal))
                            32)
                       35)))
               (when ref-pos
                 (cl-decf x (car (posn-col-row ref-pos)))
                 (cl-decf y (cdr (posn-col-row ref-pos))))
               (when (and (or (eq (eat--t-term-mouse-mode terminal)
                                  'any-event)
                              button)
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
                            (+ y 32))))))))
          ('(eat-focus-in)
           (when (eat--t-term-focus-event-mode eat--t-term)
             (send "\e[I")))
          ('(eat-focus-out)
           (when (eat--t-term-focus-event-mode eat--t-term)
             (send "\e[O"))))))))

(defun eat-send-string-as-yank (terminal args)
  "Send ARGS to TERMINAL, honoring bracketed yank mode.

Each argument in ARGS can be either string or character."
  (funcall (eat--t-term-input-fn terminal) terminal
           (let ((str (mapconcat (lambda (s)
                                   (if (stringp s) s (string s)))
                                 args "")))
             (if (eat--t-term-bracketed-yank terminal)
                 ;; NOTE: What if `str' itself contains these escape
                 ;; sequences?
                 (format "\e[200~%s\e[201~" str)
               str))))

(defun eat-term-make-keymap (input-command categories exceptions)
  "Make a keymap binding INPUT-COMMAND to the events of CATEGORIES.

CATEGORIES is a list whose elements should be a one of the following
keywords:

  `:ascii'              All ASCII characters, plus `backspace',
                        `insert', `delete' and `deletechar' keys, with
                        all possible modifiers.
  `:arrow'              Arrow keys with all possible modifiers.
  `:navigation'         Navigation keys: home, end, prior (or page up)
                        and next (or page down) with all possible
                        modifiers.
  `:function'           Function keys (f1 - f63).
  `:mouse-click'        `mouse-1', `mouse-2' and `mouse-3'.
  `:mouse-modifier'     All mouse events except mouse movement.
  `:mouse-movement'     Mouse movement.

EXCEPTIONS is a list of event, which won't be bound."
  (let ((map (make-sparse-keymap))
        (esc-map (make-sparse-keymap)))
    (when (and (memq :self-insert categories)
               (not (member [remap self-insert-command] exceptions)))
      (define-key map [remap self-insert-command] input-command))
    (when (memq :ascii categories)
      (cl-loop
       for i from ?\  to ?~
       do (unless (memq i exceptions)
            (define-key map `[,i] input-command)))
      (dolist (key '( backspace C-backspace
                      insert C-insert M-insert S-insert C-M-insert
                      C-S-insert M-S-insert C-M-S-insert
                      delete C-delete M-delete S-delete C-M-delete
                      C-S-delete M-S-delete C-M-S-delete
                      deletechar C-deletechar M-deletechar
                      S-deletechar C-M-deletechar C-S-deletechar
                      M-S-deletechar C-M-S-deletechar))
        (unless (memq key exceptions)
          (define-key map `[,key] input-command)))
      (cl-loop
       for i from ?\C-@ to ?\C-_
       do (unless (or (= i ?\e)
                      (memq i exceptions))
            (define-key map `[,i] input-command)))
      (dolist (key '(?\C-- ?\C-? ?\C-\ ))
        (unless (memq key exceptions)
          (define-key map `[,key] input-command)))
      (cl-loop
       for i from ?\  to ?~
       do (unless (or (memq i '(?O ?\[))
                      (memq i exceptions))
            (define-key esc-map `[,i] input-command)))
      (cl-loop
       for i from ?\C-@ to ?\C-_
       do (unless (memq i exceptions)
            (define-key esc-map `[,i] input-command))))
    (when (memq :arrow categories)
      (dolist (key '( up down right left
                      C-up C-down C-right C-left
                      M-up M-down M-right M-left
                      S-up S-down S-right S-left
                      C-M-up C-M-down C-M-right C-M-left
                      C-S-up C-S-down C-S-right C-S-left
                      M-S-up M-S-down M-S-right M-S-left
                      C-M-S-up C-M-S-down C-M-S-right C-M-S-left))
        (unless (memq key exceptions)
          (define-key map `[,key] input-command))))
    (when (memq :navigation categories)
      (dolist (key '( home C-home M-home S-home C-M-home C-S-home
                      M-S-home C-M-S-home
                      end C-end M-end S-end C-M-end C-S-end
                      M-S-end C-M-S-end
                      prior C-prior M-prior S-prior C-M-prior
                      C-S-prior M-S-prior C-M-S-prior
                      next C-next M-next S-next C-M-next C-S-next
                      M-S-next C-M-S-next))
        (unless (memq key exceptions)
          (define-key map `[,key] input-command))))
    (when (memq :function categories)
      (cl-loop
       for i from 1 to 63
       do (let ((key (intern (format "f%i" i))))
            (unless (member key exceptions)
              (define-key map `[,key] input-command)))))
    (when (and (or (memq :meta-ascii categories)
                   (memq :control-meta-ascii categories))
               (not (memq meta-prefix-char exceptions)))
      (define-key map `[,meta-prefix-char] esc-map))
    (when (memq :mouse-click categories)
      (dolist (key '(mouse-1 mouse-2 mouse-3))
        (unless (memq key exceptions)
          (define-key map `[,key] input-command))))
    (when (memq :mouse-modifier categories)
      (dolist (key '( down-mouse-1 drag-mouse-1 down-mouse-2
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
        (unless (memq key exceptions)
          (define-key map `[,key] input-command))))
    (when (and (memq :mouse-movement categories)
               (not (memq 'mouse-movement exceptions)))
      (define-key map [mouse-movement] input-command))
    (define-key map '[?\e] esc-map)
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

(defun eat--flip-slow-blink-state ()
  "Flip the state of slowly blinking text."
  (declare-function face-remap-add-relative "face-remap"
                    (face &rest specs))
  (declare-function face-remap-remove-relative "face-remap" (cookie))
  (face-remap-remove-relative eat--slow-blink-remap)
  (setq eat--slow-blink-remap
        (face-remap-add-relative
         'eat-slow-blink
         `(:box nil :inverse-video ,(not eat--slow-blink-state))))
  (setq eat--slow-blink-state (not eat--slow-blink-state)))

(defun eat--flip-fast-blink-state ()
  "Flip the state of rapidly blinking text."
  (declare-function face-remap-add-relative "face-remap"
                    (face &rest specs))
  (declare-function face-remap-remove-relative "face-remap" (cookie))
  (face-remap-remove-relative eat--fast-blink-remap)
  (setq eat--fast-blink-remap
        (face-remap-add-relative
         'eat-fast-blink
         `(:box nil :inverse-video ,(not eat--fast-blink-state))))
  (setq eat--fast-blink-state (not eat--fast-blink-state)))

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
  (declare-function face-remap-add-relative "face-remap"
                    (face &rest specs))
  (cond
   (eat-blink-mode
    (setq eat-blink-mode nil)
    (require 'face-remap)
    (setq eat-blink-mode t)
    (make-local-variable 'eat--slow-blink-state)
    (make-local-variable 'eat--fast-blink-state)
    (make-local-variable 'eat--slow-blink-remap)
    (make-local-variable 'eat--fast-blink-remap)
    (make-local-variable 'eat--slow-blink-timer)
    (make-local-variable 'eat--fast-blink-timer)
    (setq eat--slow-blink-state nil)
    (setq eat--fast-blink-state nil)
    (setq eat--slow-blink-remap
          (face-remap-add-relative 'eat-term-slow-blink '(:box nil)))
    (setq eat--fast-blink-remap
          (face-remap-add-relative 'eat-term-fast-blink '(:box nil)))
    (add-hook 'pre-command-hook #'eat--blink-stop-timers nil t)
    (add-hook 'post-command-hook #'eat--blink-start-timers nil t))
   (t
    (eat--blink-stop-timers)
    (face-remap-remove-relative eat--slow-blink-remap)
    (face-remap-remove-relative eat--fast-blink-remap)
    (remove-hook 'pre-command-hook #'eat--blink-stop-timers t)
    (remove-hook 'post-command-hook #'eat--blink-start-timers t)
    (kill-local-variable 'eat--slow-blink-state)
    (kill-local-variable 'eat--fast-blink-state)
    (kill-local-variable 'eat--slow-blink-remap)
    (kill-local-variable 'eat--fast-blink-remap)
    (kill-local-variable 'eat--slow-blink-timer)
    (kill-local-variable 'eat--fast-blink-timer))))


;;;; Buffer-local Cursor Blinking.

(defvar eat--cursor-blink-state nil
  "Current state of slowly blinking text, non-nil means invisible.")

(defvar eat--cursor-blink-timer nil
  "Timer for blinking slowly blinking text.")

(defvar eat--cursor-blink-frequency nil
  "Frequency of cursor blinking.")

(defvar eat--cursor-blink-mode)

(defun eat--flip-cursor-blink-state ()
  "Flip the state of slowly blinking text."
  (when (and eat--cursor-blink-mode
             (display-graphic-p))
    (if eat--cursor-blink-state
        (progn
          (setq-local cursor-type eat--cursor-blink-state)
          (setq eat--cursor-blink-state nil))
      (setq eat--cursor-blink-state cursor-type)
      (setq-local cursor-type nil))
    (when-let ((window (get-buffer-window nil 'visible)))
      (redraw-frame (window-frame window)))))

(defun eat--cursor-blink-stop-timers ()
  "Start blinking timers."
  (when eat--cursor-blink-state
    (eat--flip-cursor-blink-state))
  (when eat--cursor-blink-timer
    (cancel-timer eat--cursor-blink-timer)
    (setq eat--cursor-blink-timer nil)))

(defun eat--cursor-blink-start-timers ()
  "Start blinking timers."
  (eat--cursor-blink-stop-timers)
  (setq eat--cursor-blink-timer
        (run-with-timer t (/ (float eat--cursor-blink-frequency))
                        #'eat--flip-cursor-blink-state)))

(define-minor-mode eat--cursor-blink-mode
  "Toggle blinking of cursor."
  :interactive nil
  (cond
   (eat--cursor-blink-mode
    (make-local-variable 'eat--cursor-blink-state)
    (make-local-variable 'eat--cursor-blink-timer)
    (setq eat--cursor-blink-state nil)
    (setq eat--cursor-blink-timer nil)
    (add-hook 'pre-command-hook #'eat--cursor-blink-stop-timers nil t)
    (add-hook 'post-command-hook #'eat--cursor-blink-start-timers
              nil t)
    (when (current-idle-time)
      (eat--cursor-blink-start-timers)))
   (t
    (eat--cursor-blink-stop-timers)
    (remove-hook 'pre-command-hook #'eat--cursor-blink-stop-timers t)
    (remove-hook 'post-command-hook #'eat--cursor-blink-start-timers
                 t)
    (kill-local-variable 'eat--cursor-blink-state)
    (kill-local-variable 'eat--cursor-blink-timer))))


;;;; User Interface.

(defvar eat--terminal nil
  "The terminal object.")

(defvar eat--synchronize-scroll-function nil
  "Function to synchronize scrolling between terminal and window.")

(defun eat-reset ()
  "Perform a terminal reset."
  (interactive)
  (when eat--terminal
    (let ((inhibit-read-only t))
      (eat-term-reset eat--terminal))))

(defun eat--set-cursor (_ state)
  "Set cursor type according to STATE.

  STATE can be one of the following:

  `:default'            Default cursor.
  `:invisible'          Invisible cursor.
  `:very-visible'       Very visible cursor.  Can also be implemented
                        as blinking cursor.
  Any other value     Default cursor."
  (pcase state
    (:invisible
     (setq-local cursor-type (car eat-invisible-cursor-type))
     (setq-local eat--cursor-blink-frequency
                 (cdr eat-invisible-cursor-type))
     (eat--cursor-blink-mode
      (if (cdr eat-invisible-cursor-type) +1 -1)))
    (:very-visible
     (setq-local cursor-type (car eat-very-visible-cursor-type))
     (setq-local eat--cursor-blink-frequency
                 (cdr eat-very-visible-cursor-type))
     (eat--cursor-blink-mode
      (if (cdr eat-very-visible-cursor-type) +1 -1)))
    (_ ; `:default'
     (setq-local cursor-type (car eat-default-cursor-type))
     (setq-local eat--cursor-blink-frequency
                 (cdr eat-default-cursor-type))
     (eat--cursor-blink-mode
      (if (cdr eat-default-cursor-type) +1 -1)))))


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
                (funcall eat--mouse-drag-transient-map-exit)
                (setq eat--mouse-drag-transient-map-exit nil)))
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
  "Read a char and send it as INPUT."
  (declare (interactive-only "Use `eat-self-input' instead."))
  (interactive)
  (eat-self-input 1 (read-event)))

(defun eat-yank (&optional arg)
  "Same as `yank', but for Eat.

ARG is passed to `yank', which see."
  (interactive "*P")
  (when eat--terminal
    (funcall eat--synchronize-scroll-function)
    (cl-letf* ((inhibit-read-only t)
               (insert-for-yank (symbol-function #'insert-for-yank))
               ((symbol-function #'insert-for-yank)
                (lambda (&rest args)
                  (cl-letf (((symbol-function #'insert)
                             (lambda (&rest args)
                               (eat-send-string-as-yank
                                eat--terminal
                                (mapconcat (lambda (arg)
                                             (if (stringp arg)
                                                 arg
                                               (string arg)))
                                           args)))))
                    (apply insert-for-yank args)))))
      (yank arg))))

(defun eat-yank-pop (&optional arg)
  "Same as `yank-pop', but for Eat.

ARG is passed to `yank-pop', which see."
  (interactive "p")
  (when eat--terminal
    (funcall eat--synchronize-scroll-function)
    (cl-letf* ((inhibit-read-only t)
               (insert-for-yank (symbol-function #'insert-for-yank))
               ((symbol-function #'insert-for-yank)
                (lambda (&rest args)
                  (cl-letf (((symbol-function #'insert)
                             (lambda (&rest args)
                               (eat-send-string-as-yank
                                eat--terminal
                                (mapconcat (lambda (arg)
                                             (if (stringp arg)
                                                 arg
                                               (string arg)))
                                           args)))))
                    (apply insert-for-yank args)))))
      (yank-pop arg))))

(defvar eat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?\C-j] #'eat-char-mode)
    (define-key map [?\C-c ?\C-s] #'eat-semi-char-mode)
    (define-key map [?\C-c ?\C-k] #'eat-kill-process)
    map)
  "Keymap for Eat mode.")

(defvar eat-semi-char-mode-map
  (let ((map (eat-term-make-keymap
              #'eat-self-input
              '(:ascii :arrow :navigation)
              '( ?\C-\\ ?\C-q ?\C-c ?\C-x ?\C-g ?\C-h ?\C-\M-c ?\C-u
                 ?\M-x ?\M-: ?\M-! ?\M-& ?\C-y ?\M-y))))
    (define-key map [?\C-q] #'eat-quoted-input)
    (define-key map [?\C-y] #'eat-yank)
    (define-key map [?\M-y] #'eat-yank-pop)
    (define-key map [?\C-c ?\C-c] #'eat-self-input)
    (define-key map [?\C-c ?\C-e] #'eat-emacs-mode)
    map)
  "Keymap for Eat semi-char mode.")

(defvar eat-char-mode-map
  (let ((map (eat-term-make-keymap
              #'eat-self-input
              '(:ascii :arrow :navigation :function)
              nil)))
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
  (if (not eat--terminal)
      (error "Process not running")
    (setq buffer-read-only nil)
    (eat--char-mode -1)
    (eat--semi-char-mode +1)
    (eat--grab-mouse nil eat--mouse-grabbing-type)
    (force-mode-line-update)))

(defun eat-char-mode ()
  "Switch to char mode."
  (interactive)
  (if (not eat--terminal)
      (error "Process not running")
    (setq buffer-read-only nil)
    (eat--semi-char-mode -1)
    (eat--char-mode +1)
    (eat--grab-mouse nil eat--mouse-grabbing-type)
    (force-mode-line-update)))

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
  Any other value    Disable mouse."
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

(defun eat--manipulate-kill-ring (_ selection data)
  "Manipulate `kill-ring'.

SELECTION can be one of `:clipboard', `:primary', `:secondary',
`:select'.  When DATA is a string, set the selection to that string,
when DATA is nil, unset the selection, and when DATA is t, return the
selection, or nil if none."
  (let ((inhibit-eol-conversion t)
        (select-enable-clipboard (eq selection :clipboard))
        (select-enable-primary (eq selection :primary)))
    (pcase data
      ('t
       (when eat-enable-yank-to-terminal
         (ignore-error error
           (current-kill 0 'do-not-move))))
      ((and (pred stringp)
            str)
       (when eat-enable-kill-from-terminal
         (kill-new str))))))

(defun eat--bell (_)
  "Ring the bell."
  (beep t))


;;;;; Major Mode.

(defun eat--synchronize-scroll ()
  "Synchronize scrolling and point between terminal and window."
  (when-let ((window (get-buffer-window (current-buffer))))
    (set-window-start
     window (eat-term-display-beginning eat--terminal)))
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

(define-derived-mode eat-mode nil "Eat"
  "Major mode for Eat."
  :group 'eat-ui
  :lighter
  (make-local-variable 'buffer-read-only)
  (make-local-variable 'buffer-undo-list)
  (make-local-variable 'filter-buffer-substring-function)
  (make-local-variable 'mode-line-process)
  (make-local-variable 'mode-line-buffer-identification)
  (make-local-variable 'glyphless-char-display)
  (make-local-variable 'cursor-type)
  (make-local-variable 'track-mouse)
  (make-local-variable 'eat--terminal)
  (make-local-variable 'eat--process)
  (make-local-variable 'eat--synchronize-scroll-function)
  (make-local-variable 'eat--mouse-grabbing-type)
  (make-local-variable 'eat--pending-output-chunks)
  (make-local-variable 'eat--output-queue-first-chunk-time)
  (make-local-variable 'eat--process-output-queue-timer)
  ;; This is intended; input methods don't work on read-only buffers.
  (setq buffer-read-only nil)
  (setq buffer-undo-list t)
  (setq eat--synchronize-scroll-function #'eat--synchronize-scroll)
  (setq eat--mouse-grabbing-type nil)
  (setq filter-buffer-substring-function
        #'eat--filter-buffer-substring)
  (setq mode-line-process
        '(""
          (:eval
           (when eat--process
             (cond
              (eat--semi-char-mode
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
                       (down-mouse-1 . eat-char-mode)
                       (down-mouse-3 . eat-emacs-mode)))))
                 "]"))
              (eat--char-mode
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
                       (down-mouse-1 . eat-semi-char-mode)
                       (down-mouse-3 . eat-emacs-mode)))))
                 "]"))
              (t
               `("["
                 (:propertize
                  "emacs"
                  help-echo
                  ,(concat "mouse-1: Switch to semi char mode, "
                           "mouse-3: Switch to char mode")
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
                      (format "(%s)"  (eat-term-title
                                       eat--terminal))))
                   help-echo "Title"))))))
  (eat-emacs-mode)
  ;; Make sure glyphless character don't display a huge box glyph,
  ;; that would break the display.
  (eat--setup-glyphless-chars)
  (when eat-enable-blinking-text
    (eat-blink-mode +1)))


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
        (when eat-term-scrollback-size
          (delete-region
           (point-min)
           (max (point-min)
                (- (eat-term-display-beginning eat--terminal)
                   eat-term-scrollback-size))))
        (when synchronize-scroll
          (funcall eat--synchronize-scroll-function))))))

(defun eat--filter (process output)
  "Handle OUTPUT from PROCESS."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (when eat--process-output-queue-timer
        (cancel-timer eat--process-output-queue-timer))
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
                (eat--process-output-queue (current-buffer))
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
  (let* ((size (funcall window-adjust-process-window-size-function
                        process windows)))
    (when size
      (let ((width (max (car size) 1))
            (height (max (cdr size) 1))
            (inhibit-read-only t)
            (synchronize-scroll
             (and (<= (eat-term-display-beginning eat--terminal)
                      (point))
                  (or (< (point) (eat-term-end eat--terminal))
                      (= (point) (eat-term-end eat--terminal)
                         (point-max))))))
        (eat-term-resize eat--terminal width height)
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
      (when-let ((window (get-buffer-window nil t)))
        (with-selected-window window
          (eat-term-resize eat--terminal (window-max-chars-per-line)
                           (window-text-height))))
      (setf (eat-term-input-function eat--terminal) #'eat--send-input)
      (setf (eat-term-set-cursor-function eat--terminal)
            #'eat--set-cursor)
      (setf (eat-term-grab-mouse-function eat--terminal)
            #'eat--grab-mouse)
      (setf (eat-term-manipulate-selection-function eat--terminal)
            #'eat--manipulate-kill-ring)
      (setf (eat-term-ring-bell-function eat--terminal) #'eat--bell)
      ;; Crank up a new process.
      (let* ((size (eat-term-size eat--terminal))
             (process-environment
              (nconc
               (list
                (concat "TERM=" (eat-term-name))
                (concat "TERMINFO=" eat-term-terminfo-directory)
                (concat "INSIDE_EMACS=" eat-term-inside-emacs))
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
  "Start a new Eat terminal emulator in a new buffer.

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

(defvar eat-eshell-emacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?\C-s] #'eat-eshell-semi-char-mode)
    (define-key map [?\C-c ?\C-j] #'eat-eshell-char-mode)
    map)
  "Keymap for Eat Eshell when no process is running.")

(defvar eat-eshell-semi-char-mode-map
  (let ((map (eat-term-make-keymap
              #'eat-self-input
              '(:ascii :arrow :navigation)
              '( ?\C-\\ ?\C-q ?\C-c ?\C-x ?\C-g ?\C-h ?\C-\M-c ?\C-u
                 ?\M-x ?\M-: ?\M-! ?\M-& ?\C-y ?\M-y))))
    (define-key map [?\C-q] #'eat-quoted-input)
    (define-key map [?\C-y] #'eat-yank)
    (define-key map [?\M-y] #'eat-yank-pop)
    (define-key map [?\C-c ?\C-e] #'eat-eshell-emacs-mode)
    (define-key map [?\C-c ?\C-j] #'eat-eshell-char-mode)
    map)
  "Keymap for Eat Eshell semi-char mode.")

(defvar eat-eshell-char-mode-map
  (let ((map (eat-term-make-keymap
              #'eat-self-input
              '(:ascii :arrow :navigation :function)
              nil)))
    (define-key map [?\C-\M-m] #'eat-eshell-semi-char-mode)
    map)
  "Keymap for Eat Eshell char mode.")

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

(defun eat--eshell-output-filter ()
  "Handle output from subprocess."
  (defvar eshell-last-output-start) ; In `esh-mode'.
  (defvar eshell-last-output-end) ; In `esh-mode'.
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
  "Setup a PROC and a new terminal for it."
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
    (when-let ((window (get-buffer-window nil t)))
      (with-selected-window window
        (eat-term-resize eat--terminal (window-max-chars-per-line)
                         (window-text-height))))
    (make-local-variable 'eshell-output-filter-functions)
    (setq eshell-output-filter-functions '(eat--eshell-output-filter))
    (eat-eshell-semi-char-mode)
    (eat--cursor-blink-mode -1)))

(defun eat--eshell-cleanup ()
  "Cleanup everything."
  (defvar eshell-last-output-start) ; In `esh-mode'.
  (defvar eshell-last-output-end) ; In `esh-mode'.
  (when eat--terminal
    (let ((inhibit-read-only t))
      (goto-char (eat-term-end eat--terminal))
      (unless (or (= (point) (point-min))
                  (= (char-before) ?\n))
        (insert ?\n))
      (set-marker eshell-last-output-start (point))
      (set-marker eshell-last-output-end (point))
      (eat--cursor-blink-mode -1)
      (eat-term-delete eat--terminal)
      (set-process-filter eat--process #'eshell-output-filter)
      (setq eat--terminal nil)
      (setq eat--process nil)
      (kill-local-variable 'eshell-output-filter-functions)
      (eat--eshell-semi-char-mode -1)
      (eat--eshell-char-mode -1))))

(defun eat--eshell-process-output-queue (process buffer)
  "Process the output queue on BUFFER from PROCESS."
  (declare-function eshell-output-filter "esh-mode" (process string))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when eat--process-output-queue-timer
        (cancel-timer eat--process-output-queue-timer))
      (setq eat--output-queue-first-chunk-time nil)
      (let ((queue eat--pending-output-chunks))
        (setq eat--pending-output-chunks nil)
        (eshell-output-filter
         process (string-join (nreverse queue)))))))

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

(defun eat--eshell-sentinel (process message)
  "Process status message MESSAGE from PROCESS."
  (declare-function eshell-sentinel "esh-proc" (proc string))
  (when (buffer-live-p (process-buffer process))
    (cl-letf* ((process-send-string
                (symbol-function #'process-send-string))
               ((symbol-function #'process-send-string)
                (lambda (proc string)
                  (when (or (not (eq proc process))
                            (process-live-p proc))
                    (funcall process-send-string proc string)))))
      (eat--eshell-process-output-queue process (current-buffer)))
    (when (memq (process-status process) '(signal exit))
      (with-current-buffer (process-buffer process)
        (eat--eshell-cleanup))))
  (eshell-sentinel process message))

;; HACK: This is a dirty hack, it can break easily.
(defun eat--eshell-adjust-make-process-args (fn command args)
  "Setup an environment to adjust `make-process' arguments.

Call FN with COMMAND and ARGS, and whenever `make-process' is called,
modify its argument to change the filter, the sentinel and invoke
`stty' from the new process."
  (cl-letf*
      ((make-process (symbol-function #'make-process))
       ((symbol-function #'make-process)
        (lambda (&rest plist)
          ;; Make sure we don't attack wrong process.
          (if (not (and (eq (plist-get plist :filter)
                            #'eshell-output-filter)
                        (eq (plist-get plist :sentinel)
                            #'eshell-sentinel)
                        (equal (plist-get plist :command)
                               (cons (file-local-name
                                      (expand-file-name command))
                                     args))))
              (apply make-process plist)
            (plist-put plist :filter #'eat--eshell-filter)
            (plist-put plist :sentinel #'eat--eshell-sentinel)
            (plist-put
             plist :command
             `("/usr/bin/env" "sh" "-c"
               ,(format "stty -nl echo rows %d columns %d \
sane 2>%s ; if [ $1 = .. ]; then shift; fi; exec \"$@\""
                        (window-text-height)
                        (window-max-chars-per-line) null-device)
               ".."
               ,@(plist-get plist :command)))
            (let ((process (apply make-process plist)))
              (eat--eshell-setup-proc-and-term process)
              process)))))
    (funcall fn command args)))


;;;;; Minor Modes.

(defun eat--eshell-synchronize-scroll ()
  "Synchronize scrolling and point between terminal and window."
  (when-let ((window (get-buffer-window (current-buffer))))
    (set-window-start
     window
     (if (or (eat-term-in-alternative-display-p eat--terminal)
             eat--eshell-char-mode)
         (eat-term-display-beginning eat--terminal)
       (save-restriction
         (narrow-to-region
          (eat-term-beginning eat--terminal)
          (eat-term-end eat--terminal))
         (let ((start-line (- (window-text-height window)
                              (line-number-at-pos (point-max)))))
           (goto-char (point-min))
           (widen)
           (if (<= start-line 0)
               (eat-term-display-beginning eat--terminal)
             (vertical-motion (- start-line))
             (point)))))))
  (goto-char (eat-term-display-cursor eat--terminal)))

(define-minor-mode eat--eshell-local-mode
  "Toggle Eat terminal emulation is Eshell."
  :interactive nil
  :keymap eat-eshell-emacs-mode-map
  (cond
   (eat--eshell-local-mode
    (make-local-variable 'cursor-type)
    (make-local-variable 'glyphless-char-display)
    (make-local-variable 'track-mouse)
    (make-local-variable 'filter-buffer-substring-function)
    (make-local-variable 'eat--terminal)
    (make-local-variable 'eat--process)
    (make-local-variable 'eat--synchronize-scroll-function)
    (make-local-variable 'eat--mouse-grabbing-type)
    (make-local-variable 'eat--pending-output-chunks)
    (make-local-variable 'eat--output-queue-first-chunk-time)
    (make-local-variable 'eat--process-output-queue-timer)
    (setq eat--synchronize-scroll-function
          #'eat--eshell-synchronize-scroll)
    (setq filter-buffer-substring-function
          #'eat--filter-buffer-substring)
    ;; Make sure glyphless character don't display a huge box glyph,
    ;; that would break the display.
    (eat--setup-glyphless-chars)
    (when eat-enable-blinking-text
      (eat-blink-mode +1)))
   (t
    (when eat-enable-blinking-text
      (eat-blink-mode -1))
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'glyphless-char-display)
    (kill-local-variable 'track-mouse)
    (make-local-variable 'filter-buffer-substring-function)
    (kill-local-variable 'eat--terminal)
    (kill-local-variable 'eat--process)
    (kill-local-variable 'eat--synchronize-scroll-function)
    (kill-local-variable 'eat--mouse-grabbing-type)
    (kill-local-variable 'eat--pending-output-chunks)
    (kill-local-variable 'eat--output-queue-first-chunk-time)
    (kill-local-variable 'eat--process-output-queue-timer))))

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
  (defvar eshell-variable-aliases-list) ; In `esh-var'.
  (defvar eshell-last-async-procs) ; In `esh-cmd'.
  (declare-function eshell-gather-process-output "esh-proc"
                    (command args))
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
            (when eshell-last-async-procs
              (user-error
               (concat "Can't toggle Eat Eshell mode while"
                       " any Eshell process is running")))
            (push buffer buffers))))
      (setq eat-eshell-mode t)
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (eat--eshell-local-mode +1))))
    (add-hook 'eshell-mode-hook #'eat--eshell-local-mode)
    (setq eshell-variable-aliases-list
          `(("TERM" eat-term-name t t)
            ("TERMINFO" eat-term-terminfo-directory t)
            ("INSIDE_EMACS" eat-term-inside-emacs t)
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
            (when eshell-last-async-procs
              (user-error
               (concat "Can't toggle Eat Eshell mode while"
                       " any Eshell process is running")))
            (push buffer buffers))))
      (setq eat-eshell-mode nil)
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (eat--eshell-local-mode -1))))
    (remove-hook 'eshell-mode-hook #'eat--eshell-local-mode)
    (setq eshell-variable-aliases-list
          (cl-delete-if
           (lambda (elem)
             (member elem
                     '(("TERM" eat-term-name t t)
                       ("TERMINFO" eat-term-terminfo-directory t)
                       ("INSIDE_EMACS" eat-term-inside-emacs t))))
           eshell-variable-aliases-list))
    (advice-remove #'eshell-gather-process-output
                   #'eat--eshell-adjust-make-process-args))))


;;;; Eshell Visual Command Handling.

;; Adapted from `em-term'.
(defun eat--eshell-visual-sentinel (proc _msg)
  "Clean up the buffer visiting PROC.

If `eshell-destroy-buffer-when-process-dies' is non-nil, destroy
the buffer.

MSG describes PROC's status."
  (defvar eshell-destroy-buffer-when-process-dies) ; In `em-term'.
  (when eshell-destroy-buffer-when-process-dies
    (let ((proc-buf (process-buffer proc)))
      (when (and proc-buf (buffer-live-p proc-buf)
                 (not (eq 'run (process-status proc)))
                 (= (process-exit-status proc) 0))
        (if (eq (current-buffer) proc-buf)
            (when-let ((buf (and (boundp 'eshell-parent-buffer)
                                 (buffer-live-p eshell-parent-buffer)
                                 eshell-parent-buffer)))
              (switch-to-buffer buf)))
        (kill-buffer proc-buf)))))

(defun eat--eshell-exec-visual (&rest args)
  "Run the specified PROGRAM in a terminal emulation buffer.

ARGS are passed to the program.  At the moment, no piping of input is
allowed."
  (declare-function eshell-find-interpreter "esh-ext"
                    (file args &optional no-examine-p))
  (declare-function eshell-stringify-list "esh-util" (args))
  (defvar eshell-interpreter-alist) ; In `esh-ext'.
  (require 'esh-ext)
  (require 'esh-util)
  (let* (eshell-interpreter-alist
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

;;;###autoload
(define-minor-mode eat-eshell-visual-command-mode
  "Toggle running Eshell visual commands with Eat."
  :group 'eat-eshell
  :global t
  (declare-function eshell-exec-visual "em-term" (&rest args))
  (if eat-eshell-visual-command-mode
      (advice-add #'eshell-exec-visual :override
                  #'eat--eshell-exec-visual)
    (advice-remove #'eshell-exec-visual #'eat--eshell-exec-visual)))


;;;; Project integration.

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
  (declare-function project-root "project" (project))
  (declare-function project-prefixed-buffer-name "project" (mode))
  (require 'project)
  (let* ((default-directory (project-root (project-current t)))
         (eat-buffer-name (project-prefixed-buffer-name "eat")))
    (eat nil arg)))

;;;###autoload
(with-eval-after-load 'project
  (when (and (boundp 'project-switch-commands)
             (boundp 'project-prefix-map)
             (not (member '(eat-project "Eat")
                          project-switch-commands)))
    (setq project-switch-commands (append project-switch-commands
                                          '((eat-project "Eat"))))
    (define-key project-prefix-map "E" #'eat-project)))

(provide 'eat)
;;; eat.el ends here
