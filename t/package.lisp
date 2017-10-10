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

;; https://en.wikipedia.org/wiki/Test_functions_for_optimization

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

(declaim (ftype (function (double-float double-float) double-float)
                rastrigin
                ackley
                sphere
                rosenbrock
                beale
                goldstein-price
                booth
                bukin-n6
                matyas
                levi
                three-hump-camel
                easom
                cross-in-tray
                eggholder
                holder-table
                mccormick
                schaffer-n2
                schaffer-n4
                styblinski-tang
                ))

(defun rastrigin (x y)
  (declare (optimize (speed 3)))
  (+ (* 10 2)
     (+ _x_x _-10_(cos _2_pi_x)
        _y_y _-10_(cos _2_pi_y))))

(defun ackley (x y)
  (declare (optimize (speed 3)))
  (+ (* -20 (exp _-0.2_(sqrt 0.5)_^d))
     (- (exp _0.5_(+ (cos _2_pi_x) (cos _2_pi_y))))
     e 20))

(defun sphere (x y)
  (declare (optimize (speed 3)))
  (+ _x_x _y_y))

(defun rosenbrock (x y)
  (declare (optimize (speed 3)))
  (+ (+ _100_(^2 (- y _x_x))
        (^2 (- x 1)))))

(defun beale (x y)
  (declare (optimize (speed 3)))
  (+ (^2 (+ 1.5   _x_(- _y 1)))
     (^2 (+ 2.25  _x_(- _y_y 1)))
     (^2 (+ 2.625 _x_(- _y_y_y 1)))))

(defun goldstein-price (x y)
  (declare (optimize (speed 3)))
  (* (+ 1 (* (^2 (+ x y 1))
             (+ 19
                _-14_x
                _3_x_x
                _-14_y
                _5_x_y
                _3_y_y)))
     (+ 30 (* (^2 (+ _2_x _-3_y))
              (+ 18
                 _-32_x
                 _12_x_x
                 _48_
                 _-36_x_y
                 _27_y_y)))))

(defun booth (x y)
  (declare (optimize (speed 3)))
  (+ (^2 (+ x _2_y -7))
     (^2 (+ _2_x y -5))))

(defun bukin-n6 (x y)
  (declare (optimize (speed 3)))
  (+ _100_(sqrt (abs (- y _0.01_x_x)))
     _0.01_(abs (+ x 10))))

(defun matyas (x y)
  (declare (optimize (speed 3)))
  (- _0.26_(+ _x_x _y_y)
     _0.48_x_y))

(defun levi (x y)
  (declare (optimize (speed 3)))
  (+ (^2 (sin _3_pi_x))
     (* (^2 (- x 1))
        (1+ (sin _3_pi_y)))
     (* (^2 (- y 1))
        (1+ (sin _2_pi_y)))))

(defun three-hump-camel (x y)
  (declare (optimize (speed 3)))
  (+ _2_x_x
     _-1.05_x_x_x_x
     (/ _x_x_x_x_x_x 6)                 ;; I know this is a terrible way to compute floats.
     _x_y
     _y_y))

(defun easom (x y)
  (declare (optimize (speed 3)))
  (- (* (cos x)
        (cos y)
        (exp (- (+ (^2 (- x pi))
                   (^2 (- y pi))))))))

(defun cross-in-tray  (x y)
  (declare (optimize (speed 3)))
  (* 0.0001
     (expt (1+ (abs (* (sin x)
                       (sin y)
                       (exp (abs (- 100 (/ ^d pi)))))))
           0.1)))

(defun eggholder (x y)
  (declare (optimize (speed 3)))
  (+ (* -1
        (+ y 47)
        (sin (sqrt (abs (+ (/ x 2) y 47)))))
     (* -1
        x
        (sin (sqrt (abs (- x y 47)))))))

(defun holder-table (x y)
  (declare (optimize (speed 3)))
  (- (abs (* (sin x)
             (cos y)
             (exp (abs (1- (/ ^d pi))))))))

(defun mccormick (x y)
  (declare (optimize (speed 3)))
  (+ (sin (+ x y))
     (^2 (- x y))
     _-1.5_x
     _2.5_y
     1))

(defun schaffer-n2 (x y)
  (declare (optimize (speed 3)))
  (+ 0.5
     (/ (- (^2 (sin (- _x_x _y_y))) 0.5)
        (^2 (1+ _0.001_(+ _x_x _y_y))))))

(defun schaffer-n4 (x y)
  (declare (optimize (speed 3)))
  (+ 0.5
     (/ (- (^2 (cos (sin (abs (- _x_x _y_y))))) 0.5)
        (^2 (1+ _0.001_(+ _x_x _y_y))))))

(defun styblinski-tang (x y)
  (declare (optimize (speed 3)))
  (/ (+ _x_x_x_x _-16_x_x _5_x
        _y_y_y_y _-16_y_y _5_y)
     2))

(test dynotune

  )



