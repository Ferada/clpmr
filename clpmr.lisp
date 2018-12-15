;;;  clpmr.lisp --- Common Lisp Procmail Replacement

;;;  Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010 by Walter C. Pelissero

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

(in-package :cl-user)

(defpackage :clpmr-internal
  (:use :common-lisp
	:sclf
	:mime4cl
	:smtp4cl
	:cl-ppcre)
  (:export #:get-header #:get-headers
	   #:h~ #:match-header
	   #:*reply-to-in-replies*
	   #:*from-in-replies*
	   #:*current-message* #:current-mime
	   #:*debug*
	   #:mailbox
	   #:user
	   #:forward #:bounce
	   #:answer
	   #:save
	   #:stop
	   #:on-exit
	   #:rule-set
	   #:blacklist
	   #:blacklisted-p
	   #:forward-to-program
	   #:process-message
	   #:server #:debug-server
	   #:add-clpmr-headers
	   #:main))

(defpackage :clpmr
  (:use :common-lisp :clpmr-internal :mime4cl :sclf)
  (:export #:process-message
	   #:server
	   #:debug-server
	   #:main))

(in-package :clpmr-internal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configurables

(defvar *xloop-id* "nobody@null.dev")
(defvar *from-in-replies* "MAILER-DELIVERY-AGENT (clpmr)")
(defvar *reply-to-in-replies* "MAILER-DELIVERY-AGENT@do-not.answer (clpmr)")
(defvar *error-file* #P"clpmr.error")
(defvar *debug* nil)
(defvar *max-idle-time* 60
  "Time in seconds before the server decides to die because of
boredom.")

;; End of Configurables.  Don't edit below this point.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *current-message* nil
  "During message processing this is bound to the pathname of the
unparsed message.")
(defvar *current-mime* nil
  "During message processing this is bound to a \"promise\" of the
MIME-PART parsed from the message.")

(define-symbol-macro current-mime (force *current-mime*))

(defun get-headers (name &optional (mime-part current-mime))
  "Just like GET-HEADER but return a list of strings; one for
each occurrence of NAME header."
  (mapcar #'cdr (remove name (mime-message-headers mime-part)
			:test-not #'string-equal
			:key #'car)))

(defun get-header (name &optional (mime-part current-mime))
  "Return the header associated to NAME of MIME-PART.
Return NIL if NAME doesn't exist among the headers."
  (cdr (assoc name (mime-message-headers mime-part) :test #'string-equal)))

(defun (setf get-header) (value name &optional (mime-part current-mime))
  "Set the header associated to HEADER-SYMBOL of MIME-PART with VALUE."
  (be element (assoc name (mime-message-headers mime-part) :test #'string-equal)
    (if element
	(setf (cdr element) value)
	(push (cons name value) (mime-message-headers mime-part)))))

(eval-when (:compile-toplevel :load-toplevel)
  (defun read-header (stream char1 char2)
    (declare (ignore char1 char2))
    (list 'get-header
	  (symbol-name (read stream t nil t))))

  (set-dispatch-macro-character #\# #\? #'read-header))

(deflazy home-directory
  (pathname-as-directory (or (getenv "HOME")
			     (sclf:get-user-home))))

(deflazy server-socket-pathname
    (make-pathname :defaults (force home-directory)
		   :name ".clpmr" :type "server"))

;; We must be able to override the uid via environment variables or in
;; the configuration file.
(deflazy user
  (or (getenv "USER")
      (getenv "LOGNAME")
      (getenv "USERNAME")
      (get-logname)))

(deflazy mailbox
  (aif (getenv "MAIL")
       (pathname it)
       (make-pathname :directory '(:absolute "var" "mail") :name (force user))))

(deflazy user-mail-directory
  (make-pathname :directory
		 (append (pathname-directory (force home-directory))
			 (list "Mail"))
		 :defaults (force home-directory)))


(defun absolute-filename (name)
  (merge-pathnames name (force user-mail-directory)))

(defun sendmail (addresses message)
  (smtp:send-via-smtp (concatenate 'string (force user) "@localhost")
		      addresses (list message)))

(defun merge-headers (hl1 hl2)
  (dolist (h hl1 hl2)
    (unless (assoc (car h) hl2 :test #'string-equal)
      (push h hl2))))

(defun make-answer-headers (additional-headers message)
  (declare (type list additional-headers)
	   (type mime-message message))
  (let ((headers (list (cons "Subject" (get-header :subject message))
		       (cons "To" (format nil "~{~A~^, ~}" (reply-addresses message)))
		       (cons "From" *from-in-replies*)
		       (cons "Reply-To" *reply-to-in-replies*)
		       (cons "X-Loop" *xloop-id*)))
	(id (get-header :message-id message)))
    (merge-headers
     (merge-headers (if id
			`(("In-Reply-To" . ,id) ("References" . ,id))
			'())
		    headers)
     additional-headers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun match-header (header regular-expression)
  "Match all occurrences of HEADER to REGULAR-EXPRESSION."
  (be strings (get-headers header)
    (some #'(lambda (hdr)
	      (ppcre:scan regular-expression hdr))
	  strings)))

(defmacro h~ (h re)
  "Abbreviation for MATCH-HEADER."
  `(match-header ,h ,re))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *on-exit-calls* '())

(defmacro on-exit (&body body)
  `(push #'(lambda () ,@body) *on-exit-calls*))

(defun stop ()
  "Stop immediately any further processing on current message."
  (throw 'stop-processing nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The locking and unlocking of system mailboxes cannot be done
;;; within Lisp as, on some OSs, it requires additional privileges.
;;; This is thus best left to an external program that, being
;;; suig/sgid, can safely just do that.  In our case that program is
;;; called clpmr.locker.  Change the value of *clpmr-locker* if the
;;; default pathname doesn't suit you.

(defvar *clpmr-locker*
  #P"/usr/local/bin/clpmr.locker"
  "Pathname of the program responsible for the locking and
unlocking of mailboxes.")

(defvar *lock-expiry* (* 2 60 60)
  "How long to wait before assuming the lock was left around by a
dead process.")

(defun make-lock-file (pathname)
  "Create a lock file PATHNAME.  Due to the way *CLPMR-LOCKER*
works, the lock file is actually PATHNAME plus a \"lock\"
extension (type, in Lisp slang)."
  (be process (run-program (namestring *clpmr-locker*)
			   (list "-p" (getpid)
				 "-e" *lock-expiry*
				 (namestring pathname)))
    (zerop (sysproc-exit-code process))))

(defun delete-lock-file (pathname)
  "Remove a lock file PATHNAME."
  (be process (run-program (namestring *clpmr-locker*)
			   (list "-u"
				 (namestring pathname)))
    (zerop (sysproc-exit-code process))))

(defmacro with-lock-file ((pathname) &body body)
  "Execute BODY keeping a lock file.  The lock file is just like
PATHNAME but with type \"lock\"."
  (with-gensyms (file)
    `(be ,file ,pathname
       (unless (make-lock-file ,file)
	 (error "Cannot lock ~S." ,file))
       (unwind-protect
	    (progn ,@body)
	 (delete-lock-file ,file)))))

(defconst LOCK_SH #x01)			; shared file lock
(defconst LOCK_EX #x02)			; exclusive file lock
(defconst LOCK_NB #x04)			; don't block when locking
(defconst LOCK_UN #x08)			; unlock file

#+cmu
(eval-when (:load-toplevel :compile-toplevel :execute)
  (alien:def-alien-routine flock alien:integer (fd alien:integer :in) (op alien:integer :in))
  (defun lock-file (stream)
    (be fd (system:fd-stream-fd stream)
      (flock fd LOCK_EX)))

  (defun unlock-file (stream)
    (be fd (system:fd-stream-fd stream)
      (flock fd LOCK_UN))))

#+sbcl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (sb-alien:define-alien-routine flock sb-alien:integer (fd sb-alien:integer :in) (op sb-alien:integer :in))
  (defun lock-file (stream)
    (be fd (sb-sys:fd-stream-fd stream)
      (flock fd LOCK_EX)))
  (defun unlock-file (stream)
    (be fd (sb-sys:fd-stream-fd stream)
      (flock fd LOCK_UN))))

;; SysVs like Linux still use lock files.
#-BSD (eval-when (:load-toplevel :compile-toplevel :execute)
	(pushnew :clpmr-uses-lock-files *features*))


(defmacro with-file-lock ((stream) &body forms)
  "While executing BODY hold a Unix advisory lock on STREAM's
file descriptor."
  `(progn
     (lock-file ,stream)
     (unwind-protect (progn ,@forms)
       (unlock-file ,stream))))

;; This macro uses what it takes to guarantee an exclusive access to
;; the file on the targe OS.  On BSD it uses advisory locks (flock)
;; and on SysV the old fashioned *.lock files.
(defmacro with-exclusive-append ((var name) &body body)
  "Execute BODY with VAR bound to a stream open in append mode on
file NAME.  Care is taken to lock the file before opening it."
  (with-gensyms (pathname)
    (let ((open-forms `(with-open-file (,var ,pathname :direction :output :if-exists :append :if-does-not-exist :create))))
      `(be ,pathname ,name
	   #+clpmr-uses-lock-files
	     (with-lock-file (,pathname)
	       (,@open-forms
		,@body))
	   #-clpmr-uses-lock-files
	     (,@open-forms
	      (with-file-lock (,var)
		;; The :append option of OPEN may not be enough to
		;; guarantee that we are actually appending when
		;; between the OPEN and the obtaining of the lock
		;; another process may extend the file.
		(file-position ,var :end)
		,@body))))))

(deflazy rules-file
  (aif (getenv "CLPMRRC")
       (pathname it)
       (make-pathname :defaults (force home-directory)
		      :name ".clpmr" :type "conf")))

(let ((rules nil)
      (rules-epoch 0))

  (defun load-rules (file)
    (let ((*package* (find-package :clpmr))
	  (*load-verbose* nil)
	  (*load-print* nil)
	  (*compile-print* nil)
	  (*compile-verbose* nil))
      (handler-case
	  (load-compiled file)
	(#+sbcl sb-ext:invalid-fasl
	  #+cmu ext:invalid-fasl ()
	  (compile-file file)
	  (load-compiled file)))))

  (defun ensure-rules ()
    (be file-epoch (file-write-date (force rules-file))
      (when (> file-epoch rules-epoch)
	(load-rules (force rules-file))
	(setf rules-epoch file-epoch))))

  (defun set-rules (proc)
    (setf rules proc))

  (defmacro rule-set (&body body)
    `(set-rules #'(lambda ()
		    ,@body)))

  (defun run-rules ()
    (ensure-rules)
    (funcall rules)
    ;; By default we save the message in the usual mailbox.  We may
    ;; not reach this statement if rules contain a STOP statement.
    (save)))

(defun quote-message (message)
  (s+ " > "
      (ppcre:regex-replace-all "\\n" (with-output-to-string (out)
				       (print-mime-part (mime-body message) out))
			       (s+ (string #\newline) " > "))))

(defun reply-addresses (message)
  (mapcar #'mbx-address
	  (parse-addresses (or (get-header :reply-to message)
			       (get-header :from message)
			       "")
			   :no-groups t)))

(defun blacklist (list)
  (with-exclusive-append (stream (absolute-filename list))
    (format stream "~A~%" #?from)
    (awhen #?sender
      (format stream "~A~%" it))))

(defun blacklisted-p (address list)
  (with-open-file (stream (absolute-filename list))
    (loop
       for line = (read-line stream nil)
       while line
       when (search line address :test #'char-equal)
       return t
       finally (return nil))))

(defun looping-message-p (message)
  (awhen (get-header :x-loop message)
    (ppcre:scan *xloop-id* it)))

(defun answer (body &key attachments blacklist)
  (flet ((do-answer ()
	   (be* message (make-instance 'mime-text
				       :subtype "plain"
				       :body (format nil "~&~A~2&~A wrote:~%~A~%" body #?from
						     (quote-message current-mime)))
		answer (make-instance 'mime-message
				      :headers (make-answer-headers '() current-mime)
				      :body (if attachments
						(make-instance 'mime-multipart
							       :subtype "mixed"
							       :body (cons message
									   (mapcar #'mime-part attachments)))
						message))
	     (add-clpmr-headers answer)
	     (sendmail (reply-addresses current-mime)
		       answer))))
    (unless (looping-message-p current-mime)
      (if blacklist
	  (be sender #?from
	    (unless (blacklisted-p sender blacklist)
	      (do-answer)
	      (blacklist blacklist)))
	  (do-answer)))))

(defun forward (&rest addresses)
  "Send current message to ADDRESSES."
  (sendmail addresses *current-message*))

(defun bounce ()
  "Send back to the sender and discard it."
  (forward #?from)
  (stop))

(defun forward-to-program (program &rest args)
  (multiple-value-bind (out process) (run-pipe :output program args)
    (with-open-file (in *current-message*)
      (copy-stream in out))
    (exit-code process)))

(defun save (&key (file (force mailbox)))
  (with-exclusive-append (out (absolute-filename file))
    (if (forced-p *current-mime*)
	(progn
	  ;; copy the "From " line verbatim
	  (write-line (with-open-file (in *current-message*)
			(read-line in))
		      out)
	  (encode-mime-part current-mime out))
	(with-open-file (in *current-message*)
	  (copy-stream in out)))
    (terpri out)))

(defun add-clpmr-headers (&optional (mime-message current-mime))
  (setf (get-header :x-clpmr-version mime-message)
	(format nil "~A, ~A ~A"
		#.(sclf:iso-time-string
		   (get-universal-time))
		(lisp-implementation-type)
		(lisp-implementation-version)))
  (setf (get-header :x-clpmr-processed mime-message)
	(sclf:iso-time-string (get-universal-time))))

(defvar *max-stack-trace* 300)

(defun report-error (condition debugger-handler)
  (declare (ignore debugger-handler))
  ;; make sure we don't lose the message
  (save)
  (with-open-file (out (merge-pathnames *error-file* (force home-directory))
		       :direction :output
		       :if-exists :append
		       :if-does-not-exist :create)
    (format out "~&An error occurred during the processing of a mail message on ~A.~%  ~A~%"
	    (time-rfc822-string) condition)
    #+cmu (debug:backtrace *max-stack-trace* out)
    #+sbcl (sb-debug:print-backtrace :count *max-stack-trace* :stream out)
    (format out "~2&Message has been saved in the default mailbox.~%"))
  #+sbcl(sb-ext:quit :recklessly-p t :unix-status 42)
  #+cmu (ext:quit t))

(defgeneric process-message (input)
  (:documentation
   "Process a mail message coming from INPUT.  INPUT can be anything
that can be promoted to a MIME-MESSAGE."))

(defmethod process-message ((input stream))
  (with-temp-file (out)
    (copy-stream input out)
    (close out)
    (process-message (pathname out))))

(defmethod process-message ((input pathname))
  (be *debugger-hook* (if *debug* *debugger-hook* #'report-error)
    (catch 'stop-processing
      (be *current-message* input
	  *current-mime* (lazy (mime-message input))
	(run-rules)))
    (mapc #'funcall *on-exit-calls*)))

(defun remove-server-socket ()
  "If present, remove the server socket that might have been left
around by previous runs."
  (when (probe-file (force server-socket-pathname))
    (ignore-errors
      (delete-file (force server-socket-pathname)))))

(defun server ()
  "Enter server mode and start accepting connections on a Unix socket."
  ;; get rid of socket files left around accidentally
  (remove-server-socket)
  (net4cl:do-connections (socket (force server-socket-pathname)
				 :element-type 'character
				 :timeout *max-idle-time*)
    (process-message (net4cl:socket-stream socket))))

(defun exit (value)
  (throw 'exit value))

(defun interrupt-handler (signal code scp)
  (declare (ignore signal code scp))
  (exit 2))

(defun setup-interrupts ()
  (dolist (int (list :hangup
		     :quit
		     ;; don't trap SIGINT as it may be useful
		     ;; for debugging purposes
		     #+(OR):interrupt
		     :terminate))
    (sclf:sysproc-set-signal-callback int #'interrupt-handler)))

(defun parse-args (args)
  "Set global vars based on command line options and return anything
after the last optional argument."
  (loop
     for arg = (pop args)
     while arg
     do (gcase (arg string=)
	  (("-h" "-?")
	   (usage))
	  ("-d"
	   (setf *debug* t)
	   (trace remove-server-socket)
	   (trace probe-file)
	   (trace delete-file)
	   (trace process-message)
	   (trace run-rules)
	   (trace forward)
	   (trace sendmail)
	   (trace blacklist)
	   (trace load-rules)
	   (trace lock-file)
	   (trace unlock-file))
	  (t
	   (push arg args)
	   (return))))
  args)

(defun usage ()
  (format t "usage:
  clpmr -h
or
  clpmr [-d]

    -h          print this usage message
    -d          run in debug mode
~%")
  (exit 1))

(defun main ()
  (prog1
      (catch 'exit
	#+sbcl (parse-args (cdr sb-ext:*posix-argv*))
	(setup-interrupts)
	(let #+sbcl ((sb-impl::*default-external-format* :latin-1)
		     (sb-alien::*default-c-string-external-format* :latin-1))
	     #-sbcl ()
	  (server))
	0)
    ;; this shouldn't be necessary but is left here for good measure
    (remove-server-socket)))

(defun image-main ()
  (be return-value (main)
      #+sbcl(sb-ext:quit :unix-status return-value)
      #+cmu(ext:quit return-value)
      return-value))

(defun debug-server ()
  (be *debug* t
      *max-idle-time* nil
    (server)))
