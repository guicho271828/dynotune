(in-package :dynotune.test)
(in-suite :dynotune)
(named-readtables:in-readtable coefficient-helper)

;;; evolutionary algorithm benchmark functions
;;; https://en.wikipedia.org/wiki/Test_functions_for_optimization

(declaim (ftype (function ((double-float -5.12d0 5.12d0)    (double-float -5.12d0 5.12d0)) double-float) rastrigin))
(declaim (ftype (function ((double-float -5d0    5d0)       (double-float -5d0   5d0))     double-float) ackley))
(declaim (ftype (function ((double-float -100d0  100d0)     (double-float -100d0 100d0)) double-float) sphere))
(declaim (ftype (function ((double-float -100d0  100d0)     (double-float -100d0 100d0)) double-float) rosenbrock))
(declaim (ftype (function ((double-float -4.5d0  4.5d0)     (double-float -4.5d0 4.5d0)) double-float) beale))
(declaim (ftype (function ((double-float -2d0    2d0)       (double-float -2d0   2d0))   double-float) goldstein-price))
(declaim (ftype (function ((double-float -10d0   10d0)      (double-float -10d0  10d0))  double-float) booth))
(declaim (ftype (function ((double-float -15d0   -5d0)      (double-float -3d0   3d0))   double-float) bukin-n6))
(declaim (ftype (function ((double-float -10d0   10d0)      (double-float -10d0  10d0))  double-float) matyas))
(declaim (ftype (function ((double-float -10d0   10d0)      (double-float -10d0  10d0))  double-float) levi))
(declaim (ftype (function ((double-float -5d0    5d0)       (double-float -5d0   5d0))   double-float) three-hump-camel))
(declaim (ftype (function ((double-float -100d0  100d0)     (double-float -100d0 100d0)) double-float) easom))
(declaim (ftype (function ((double-float -10d0   10d0)      (double-float -10d0  10d0))  double-float) cross-in-tray))
(declaim (ftype (function ((double-float -512d0  512d0)     (double-float -512d0 512d0)) double-float) eggholder))
(declaim (ftype (function ((double-float -10d0   10d0)      (double-float -10d0  10d0))  double-float) holder-table))
(declaim (ftype (function ((double-float -1.5d0  4d0)       (double-float -3d0   4d0))   double-float) mccormick))
(declaim (ftype (function ((double-float -100d0  100d0)     (double-float -100d0 100d0)) double-float) schaffer-n2))
(declaim (ftype (function ((double-float -100d0  100d0)     (double-float -100d0 100d0)) double-float) schaffer-n4))
(declaim (ftype (function ((double-float -5d0    5d0)       (double-float -5d0   5d0))   double-float) styblinski-tang))

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
  (- (* (+ 1 (* (^2 (+ x y 1))
                (+ 19
                   _-14_x
                   _3_x_x
                   _-14_y
                   _6_x_y
                   _3_y_y)))
        (+ 30 (* (^2 (+ _2_x _-3_y))
                 (+ 18
                    _-32_x
                    _12_x_x
                    _48_y
                    _-36_x_y
                    _27_y_y))))
     ;; make the optimal solution ~0
     3))

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
  (+ (- (* (cos x)
           (cos y)
           (exp (- (+ (^2 (- x pi))
                      (^2 (- y pi)))))))
     ;; make the optimal solution ~0
     1))

(defun cross-in-tray  (x y)
  (declare (optimize (speed 3)))
  (+ (* 0.0001
        (expt (1+ (abs (* (sin x)
                          (sin y)
                          (exp (abs (- 100 (/ ^d pi)))))))
              0.1))
     ;; make the optimal solution ~0
     2.06261))

(defun eggholder (x y)
  (declare (optimize (speed 3)))
  (+ (* -1
        (+ y 47)
        (sin (sqrt (abs (+ (/ x 2) y 47)))))
     (* -1
        x
        (sin (sqrt (abs (- x y 47)))))
     ;; make the optimal solution ~0
     959.6407))

(defun holder-table (x y)
  (declare (optimize (speed 3)))
  (+ (- (abs (* (sin x)
                (cos y)
                (exp (abs (1- (/ ^d pi)))))))
     ;; make the optimal solution ~0
     19.2085))

(defun mccormick (x y)
  (declare (optimize (speed 3)))
  (+ (sin (+ x y))
     (^2 (- x y))
     _-1.5_x
     _2.5_y
     1
     ;; make the optimal solution ~0
     1.9133))

(defun schaffer-n2 (x y)
  (declare (optimize (speed 3)))
  (+ 0.5
     (/ (- (^2 (sin (- _x_x _y_y))) 0.5)
        (^2 (1+ _0.001_(+ _x_x _y_y))))))

(defun schaffer-n4 (x y)
  (declare (optimize (speed 3)))
  (+ 0.5
     (/ (- (^2 (cos (sin (abs (- _x_x _y_y))))) 0.5)
        (^2 (1+ _0.001_(+ _x_x _y_y))))
     ;; make the optimal solution ~0
     -0.292579))

(defun styblinski-tang (x y)
  (declare (optimize (speed 3)))
  (+ (/ (+ _x_x_x_x _-16_x_x _5_x
           _y_y_y_y _-16_y_y _5_y)
        2)
     ;; make the optimal solution ~0
     78.33232))

;;; test

(test continuous
  (dolist (fn '(rastrigin
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
                styblinski-tang))
    (print fn)
    (dolist (optimizer (list (random-search 1000000)
                             (gradient-descent)))
      (finishes
        (handler-case (print (subseq (multiple-value-list (tune fn optimizer)) 0 2))
          (arithmetic-error ()
            (print :arithmetic-error)))))))
