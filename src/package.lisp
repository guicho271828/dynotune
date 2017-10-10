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


(defun tune (function &optional parameters method)
  )

