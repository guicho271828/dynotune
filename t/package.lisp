#|
  This file is a part of dynotune project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :dynotune.test
  (:use :cl
        :dynotune
        :fiveam
        :iterate :alexandria :trivia))
(in-package :dynotune.test)



(def-suite :dynotune)
(in-suite :dynotune)

;; run test with (run! test-name) 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-coefficient (stream char)
    (let ((current (read-preserving-whitespace stream t nil))
          (next (peek-char nil stream nil :eof)))
      (cond
        ((eq :eof next)
         current)
        ((char= char next)
         `(* ,current ,(read stream t nil)))
        (t current))))

  (defun read-math (stream char)
    `(* ,@(read-delimited-list char stream t)))
  
  (named-readtables:defreadtable coefficient-helper
    (:merge :standard)
    (:macro-char #\_ #'read-coefficient)
    (:macro-char #\$ #'read-math)))

(named-readtables:in-readtable coefficient-helper)

;; _2_pi_x -> (* 2 pi x)

(declaim (inline ^2))
(defun ^2 (x) (* x x))

(define-symbol-macro ^d (sqrt (+ _x_x _y_y)))

(define-constant e (exp 1.0d0))

(print "Initializing an lparallel kernel")
(setf lparallel:*kernel* (lparallel:make-kernel 2))
