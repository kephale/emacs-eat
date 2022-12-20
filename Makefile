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

all: eat.elc terminfo info

info: eat.info

dvi: eat.dvi

html: eat.html

pdf: eat.pdf

terminfo: eat.ti
	test -d terminfo || mkdir terminfo
	env TERMINFO=./terminfo $(TIC) -x eat.ti
# We don't know which directory was created, it depend on the
# case-sensitivity of the file-system.  So make sure both are created.
	test -d terminfo/e || mkdir terminfo/e
	test -d terminfo/65 || mkdir terminfo/65
	if test terminfo/e/eat-mono -nt terminfo/65/eat-mono ; \
		then \
		cp terminfo/e/eat-mono terminfo/e/eat-color \
		terminfo/e/eat-256color terminfo/e/eat-truecolor \
		terminfo/65 ; \
		else \
		cp terminfo/65/eat-mono terminfo/65/eat-color \
		terminfo/65/eat-256color terminfo/65/eat-truecolor \
		terminfo/e ; \
		fi

check: eat.el
	$(EMACS) -batch -l eat.el -l eat-tests.el \
		-f ert-run-tests-batch-and-exit

changelog:
	./make-changelog

.PHONY: all terminfo info dvi html pdf check changelog

eat.elc: eat.el
	$(EMACS) -batch --eval '(byte-compile-file "eat.el")'

eat.info: eat.texi gpl.texi fdl.texi
	$(TEXI2INFO) eat.texi

eat.dvi: eat.texi gpl.texi fdl.texi texinfo.tex
	$(TEXI2DVI) eat.texi

eat.html: eat.texi gpl.texi fdl.texi
	$(TEXI2HTML) eat.texi

eat.pdf: eat.texi gpl.texi fdl.texi texinfo.tex
	$(TEXI2PDF) eat.texi
