#  Makefile --- root makefile to build everything

#  Copyright (C) 2004-2006, 2010 by Walter C. Pelissero

#  Author: Walter C. Pelissero <walter@pelissero.de>
#  Project: CLPMR a Procmail replacement in CL

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2, or (at
# your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

#  Commentary:
#  
# See the variable LISP below to change the Lisp compiler.
# Default is CMUCL.
#

RM = rm -f
MV = mv
CFLAGS = -g -Wall
LDFLAGS = -g
LDLIBS =
INSTALL = install
VERSION != date +%Y%m%d

# This should be either cmucl or sbcl
LISP = cmucl
# This must be either clpmr.cmucl-server or clpmr.sbcl-server
SERVER = clpmr.$(LISP)-server


default: clpmr $(SERVER) clpmr.locker

all: clpmr $(SERVER) clpmr.locker clpmr-$(VERSION).tbz


clpmr: clpmr.o locking.o
	$(CC) $(LDFLAGS) -o clpmr clpmr.o locking.o

clpmr.cmucl-server: clpmr.lisp clpmr.asd
	lisp -eval "(asdf:operate 'asdfa:make-exe-op :clpmr :initial-function \"MAIN\" :initial-package \"CLPMR\") (quit)"
	$(MV) clpmr.exe clpmr.cmucl-server

clpmr.sbcl-server: clpmr.lisp clpmr.asd
	sbcl --eval "(load \"compile\")"
	$(MV) clpmr.exe clpmr.sbcl-server

clpmr.locker: clpmr.locker.o locking.o
	$(CC) $(LDFLAGS) -o clpmr.locker clpmr.locker.o locking.o

clpmr.o: clpmr.c locking.h
	$(CC) $(CFLAGS) -c -o clpmr.o clpmr.c

clpmr.locker.o: clpmr.locker.c locking.h
	$(CC) $(CFLAGS) -c -o clpmr.locker.o clpmr.locker.c

locking.o: locking.c locking.h
	$(CC) $(CFLAGS) -c -o locking.o locking.c

install:
	$(INSTALL) $(SERVER) /usr/local/bin/clpmr.server
	$(INSTALL) -s clpmr /usr/local/bin
	$(INSTALL) -m 644 etc/clpmr.conf.sample /usr/local/etc
	$(INSTALL) -s -g mail -m 02755 clpmr.locker /usr/local/bin

# The FreeBSD package file
clpmr-$(VERSION).tbz:
	pkg_create -f etc/packing-list -c etc/COMMENT -d etc/DESC clpmr-$(VERSION).tbz

clean:
	$(RM) *.o clpmr clpmr.*-server clpmr.locker clpmr.exe *.fasl *.x86f clpmr-*.tbz clpmr-*.tbz2
	$(RM) -r optimised-compilation

dist: clean
	$(RM) *~ etc/*~
