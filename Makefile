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
TEXI2INFO ?= texi2any
TEXI2DVI ?= texi2dvi
TEXI2HTML ?= texi2any --no-split --html
TEXI2PDF ?= texi2pdf
TEXI2PS ?= texi2any --ps

all: eat.elc terminfo info check changelog

info: eat.info

dvi: eat.dvi

html: eat.html

pdf: eat.pdf

terminfo: e/eat-mono e/eat-color eat-256color e/eat-truecolor

check: eat.el
	$(EMACS) -batch -l eat.el -l eat-tests.el \
		-f ert-run-tests-batch-and-exit

changelog:
	./make-changelog

.PHONY: all terminfo info dvi html pdf check changelog

eat.elc: eat.el
	$(EMACS) -batch --eval '(byte-compile-file "eat.el")'

e/eat-mono e/eat-color eat-256color e/eat-truecolor: eat.ti
	env TERMINFO=. $(TIC) -x eat.ti

eat.info: eat.texi gpl.texi fdl.texi
	$(TEXI2INFO) eat.texi

eat.dvi: eat.texi gpl.texi fdl.texi texinfo.tex
	$(TEXI2DVI) eat.texi

eat.html: eat.texi gpl.texi fdl.texi
	$(TEXI2HTML) eat.texi

eat.pdf: eat.texi gpl.texi fdl.texi texinfo.tex
	$(TEXI2PDF) eat.texi
