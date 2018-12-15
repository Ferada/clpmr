;;;  compile.lisp --- create a standalone image

;;;  Copyright (C) 2010 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: clpmr

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

(cl:in-package :cl-user)

(proclaim '(optimize
	    (debug 0)
	    (space 2)
	    (speed 3)
	    (compilation-speed 0)
	    (safety 0)))

#+asdfa
(setf asdfa:*output-root-directory*
      (merge-pathnames (make-pathname :directory '(:relative "optimised-compilation"))
		       (truename (make-pathname :directory '(:relative)))))

(asdf:oos 'asdf:load-op :clpmr)

;; Although CMUCL 19e could in theory create executables files, it is
;; not supported in the FreeBSD distribution.
#+cmu
(save-lisp "clpmr.core"
	   :init-function #'clpmr-internal::image-main
	   :load-init-file nil
	   :print-herald nil
	   :process-command-line nil
	   :batch-mode nil
	   :executable nil)

#+sbcl
(save-lisp-and-die "clpmr.exe"
		   :toplevel #'clpmr-internal::image-main
		   :executable t)
