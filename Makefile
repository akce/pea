# PEA GNU Makefile.
#
# This makefile installs the source at the destination and then uses
# (compile-program) and (compile-imported-libraries #t) to compile pea lib
# files as well as program exes.
#
# This seems to be quicker than manually compiling lib files and takes care
# of dependency ordering, which I always seem to get wrong anyway.
#
# SPDX-License-Identifier: Unlicence

# Library destination directory. This must be an object directory contained in
# (library-directories).  eg, set in CHEZSCHEMELIBDIRS environment variable.
# NOTE: using ~ in PREFIX fails.  I *think* changing (library-directories) in
# the exe target could solve if it becomes an issue.
PREFIX := $(HOME)
LIBDIR := $(PREFIX)/lib
BINDIR := $(PREFIX)/bin

# Path to chez scheme executable.
SCHEME = /usr/bin/chez-scheme

# Scheme compile flags.
SFLAGS = -q

# Path to shell exes.
ECHO = /bin/echo
INSTALL = /usr/bin/install
MV = /bin/mv

## Should be no need to edit anything below here.

.PHONY: install clean

all: install

LIBS =	\
	pea/client.sls		\
	pea/omxplayer.sls	\
	pea/exewrap.sls		\
	pea/path.sls		\
	pea/player.sls		\
	pea/playlist.sls	\
	pea/server.sls		\
	pea/util.sls		\
	pea/vfs.sls

BINS =	\
	bin/pead.ss	\
	bin/peace.ss	\
	bin/peash.ss	\
	bin/pea-uade.ss

# installed lib sources.
ILIBS = $(addprefix $(LIBDIR)/,$(LIBS))

# installed shared object binaries (executables) will be without file extension.
IBINEXE = $(addprefix $(BINDIR)/,$(basename $(notdir $(BINS))))

$(LIBDIR)/%.sls: %.sls
	$(INSTALL) -D -p $< $@

$(BINDIR)/%: bin/%.ss
	$(ECHO) '(reset-handler abort) (library-directories "'$(LIBDIR)'") (compile-imported-libraries #t) (compile-program "'$<'")' | $(SCHEME) $(SFLAGS)
	$(MV) -vi $(<:.ss=.so) $@ 

install: $(ILIBS) $(IBINEXE)

clean:
	$(RM) $(ILIBS) $(ILIBS:.sls=.so) $(IBINEXE)
