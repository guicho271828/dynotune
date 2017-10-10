#|
  This file is a part of dynotune project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage dynotune
  (:use :cl :iterate :alexandria :trivia
        ;; :introspect-environment
        :type-r)
  (:shadow :double-float
           :single-float
           :short-float
           :long-float
           :integer
           :float
           :member)
  (:export
   #:tune
   #:categorical
   #:ordinal
   #:interval))
(in-package :dynotune)

;; blah blah blah.

(declaim (ftype (function (cl:double-float) cl:double-float) ^2))
(defun ^2 (x)
  (* x x))

(defun tune (function &optional parameters method)
  (unless parameters
    (assert (typep function '(or symbol cons))
            nil "parameters are missing, and ~a is not of type '(or symbol cons). Cannot guess the input region!"
            function)
    (handler-case
        (ematch (introspect-environment:function-type function)
          ((function-type (and args-types (type list)))
           (setf parameters (mapcar #'parse-generator args-types))))
      (match-error ()
        (error "could not guess the generators from the function information!"))))
  (funcall method function parameters))

