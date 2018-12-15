;;;  clpmr.asd --- declaration of this system

;;;  Copyright (C) 2005, 2006 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: CLPMR a Procmail replacement in CL

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at
;;; your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.

(defpackage :clpmr-system
  (:use :common-lisp :asdf))

(in-package :clpmr-system)

(defsystem clpmr
  :name "CLPMR"
  :author "Walter C. Pelissero <walter@pelissero.de>"
  :maintainer "Walter C. Pelissero <walter@pelissero.de>"
  :licence "General Public License"
  :description "CLPMR a replacement for Procmail written in Common Lisp"
  :long-description
  "CLPMR is a replacement for Procmail.  It should do a superset
of what Procmail is but with a human-readable syntax and the full
power of Common Lisp."
  :depends-on (:sclf :net4cl :mime4cl :smtp4cl :cl-ppcre)
  :components
  ((:doc-file "README")
   (:doc-file "COPYING")
   ;; Lots of static files that are used elsewhere.  they are listed
   ;; here to create the tarball.
   (:static-file "Makefile")
   (:static-file "locking.h")
   (:static-file "locking.c")
   (:static-file "clpmr.c")
   (:static-file "clpmr.locker.c")
   (:static-file ".project")
   (:static-file "compile.lisp")
   (:module "etc"
	    :components ((:static-file "COMMENT")
			 (:static-file "DESC")
			 (:static-file "clpmr.conf.sample")
			 (:static-file "packing-list")))
   (:file "clpmr")))
