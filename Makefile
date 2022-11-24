# Makefile --- Build configuration

# Copyright (C) 2022 Akib Azmain Turja.

# This file is not part of GNU Emacs.

# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# For a full copy of the GNU General Public License
# see <https://www.gnu.org/licenses/>.

EMACS ?= emacs
TIC ?= tic

all: eat.elc terminfo check changelog

terminfo: e/eat-mono e/eat-color eat-256color e/eat-truecolor

check: eat.el
	$(EMACS) -batch -l eat.el -l eat-tests.el \
		-f ert-run-tests-batch-and-exit

changelog:
	./make-changelog

.PHONY: all terminfo check changelog

eat.elc: eat.el
	$(EMACS) -batch --eval '(byte-compile-file "eat.el")'

e/eat-mono e/eat-color eat-256color e/eat-truecolor: eat.ti
	env TERMINFO=. $(TIC) -x eat.ti
