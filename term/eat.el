;;; eat.el --- Terminal initialization for Eat  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-12-04

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

;; Extra support for the Eat terminal emulator.

;;; Code:

(require 'term/xterm)

(defcustom xterm-eat-extra-capabilities
  '(modifyOtherKeys reportBackground getSelection setSelection)
  "List of extra capabilities supported in Eat.

Each element of the list enables a feature.  The elements can be:

`modifyOtherKeys'  More key bindings work (e.g., \"\\C-,\")
`reportBackground' Eat reports the terminal background color.
`getSelection'     Eat yanks text from parent Emacs `kill-ring'.
`setSelection'     Eat saves killed text to parent Emacs `kill-ring'."
  :type xterm--extra-capabilities-type
  :group 'eat)

(defun terminal-init-eat ()
  "Terminal initialization function for Eat."
  (let ((xterm-extra-capabilities xterm-eat-extra-capabilities))
    (tty-run-terminal-initialization (selected-frame) "xterm")))

(provide 'term/eat)
;;; eat.el ends here
