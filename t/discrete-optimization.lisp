(in-package :dynotune.test)
(in-suite :dynotune)
(named-readtables:in-readtable coefficient-helper)

;;; evolutionary algorithm benchmark functions
;;; https://en.wikipedia.org/wiki/Test_functions_for_optimization

(declaim (ftype (function ((integer -5 5)          (integer -5 5))     real) int-rastrigin))
(declaim (ftype (function ((integer -5    5)       (integer -5   5))   real) int-ackley))
(declaim (ftype (function ((integer -100  100)     (integer -100 100)) real) int-sphere))
(declaim (ftype (function ((integer -100  100)     (integer -100 100)) real) int-rosenbrock))
;; (declaim (ftype (function ((integer -4.5  4.5)     (integer -4.5 4.5)) real) int-beale))
(declaim (ftype (function ((integer -2    2)       (integer -2   2))   real) int-goldstein-price))
(declaim (ftype (function ((integer -10   10)      (integer -10  10))  real) int-booth))
(declaim (ftype (function ((integer -15   -5)      (integer -3   3))   real) int-bukin-n6))
(declaim (ftype (function ((integer -10   10)      (integer -10  10))  real) int-matyas))
(declaim (ftype (function ((integer -10   10)      (integer -10  10))  real) int-levi))
(declaim (ftype (function ((integer -5    5)       (integer -5   5))   real) int-three-hump-camel))
;; (declaim (ftype (function ((integer -100  100)     (integer -100 100)) real) int-easom))
;; (declaim (ftype (function ((integer -10   10)      (integer -10  10))  real) int-cross-in-tray))
;; (declaim (ftype (function ((integer -512  512)     (integer -512 512)) real) int-eggholder))
;; (declaim (ftype (function ((integer -10   10)      (integer -10  10))  real) int-holder-table))
;; (declaim (ftype (function ((integer -1.5  4)       (integer -3   4))   real) int-mccormick))
(declaim (ftype (function ((integer -100  100)     (integer -100 100)) real) int-schaffer-n2))
;; (declaim (ftype (function ((integer -100  100)     (integer -100 100)) real) int-schaffer-n4))
;; (declaim (ftype (function ((integer -5    5)       (integer -5   5))   real) int-styblinski-tang))

(defun int-rastrigin (x y)
  (declare (optimize (speed 3)))
  (+ (* 10 2)
     (+ _x_x _-10_(cos _2_pi_x)
        _y_y _-10_(cos _2_pi_y))))

(defun int-ackley (x y)
  (declare (optimize (speed 3)))
  (+ (* -20 (exp _-0.2_(sqrt 0.5)_^d))
     (- (exp _0.5_(+ (cos _2_pi_x) (cos _2_pi_y))))
     e 20))

(defun int-sphere (x y)
  (declare (optimize (speed 3)))
  (+ _x_x _y_y))

(defun int-rosenbrock (x y)
  (declare (optimize (speed 3)))
  (+ (+ _100_(^2 (- y _x_x))
        (^2 (- x 1)))))

;; (defun int-beale (x y)
;;   (declare (optimize (speed 3)))
;;   (+ (^2 (+ 1.5   _x_(- _y 1)))
;;      (^2 (+ 2.25  _x_(- _y_y 1)))
;;      (^2 (+ 2.625 _x_(- _y_y_y 1)))))

(defun int-goldstein-price (x y)
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

(defun int-booth (x y)
  (declare (optimize (speed 3)))
  (+ (^2 (+ x _2_y -7))
     (^2 (+ _2_x y -5))))

(defun int-bukin-n6 (x y)
  (declare (optimize (speed 3)))
  (+ _100_(sqrt (abs (- y _0.01_x_x)))
     _0.01_(abs (+ x 10))))

(defun int-matyas (x y)
  (declare (optimize (speed 3)))
  (- _0.26_(+ _x_x _y_y)
     _0.48_x_y))

(defun int-levi (x y)
  (declare (optimize (speed 3)))
  (+ (^2 (sin _3_pi_x))
     (* (^2 (- x 1))
        (1+ (sin _3_pi_y)))
     (* (^2 (- y 1))
        (1+ (sin _2_pi_y)))))

(defun int-three-hump-camel (x y)
  (declare (optimize (speed 3)))
  (+ _2_x_x
     _-1.05_x_x_x_x
     (/ _x_x_x_x_x_x 6)                 ;; I know this is a terrible way to compute floats.
     _x_y
     _y_y))

;; (defun int-easom (x y)
;;   (declare (optimize (speed 3)))
;;   (+ (- (* (cos x)
;;            (cos y)
;;            (exp (- (+ (^2 (- x pi))
;;                       (^2 (- y pi)))))))
;;      ;; make the optimal solution ~0
;;      1))

;; (defun int-cross-in-tray  (x y)
;;   (declare (optimize (speed 3)))
;;   (+ (* 0.0001
;;         (expt (1+ (abs (* (sin x)
;;                           (sin y)
;;                           (exp (abs (- 100 (/ ^d pi)))))))
;;               0.1))
;;      ;; make the optimal solution ~0
;;      2.06261))

;; (defun int-eggholder (x y)
;;   (declare (optimize (speed 3)))
;;   (+ (* -1
;;         (+ y 47)
;;         (sin (sqrt (abs (+ (/ x 2) y 47)))))
;;      (* -1
;;         x
;;         (sin (sqrt (abs (- x y 47)))))
;;      ;; make the optimal solution ~0
;;      959.6407))
;; 
;; (defun int-holder-table (x y)
;;   (declare (optimize (speed 3)))
;;   (+ (- (abs (* (sin x)
;;                 (cos y)
;;                 (exp (abs (1- (/ ^d pi)))))))
;;      ;; make the optimal solution ~0
;;      19.2085))
;; 
;; (defun int-mccormick (x y)
;;   (declare (optimize (speed 3)))
;;   (+ (sin (+ x y))
;;      (^2 (- x y))
;;      _-1.5_x
;;      _2.5_y
;;      1
;;      ;; make the optimal solution ~0
;;      1.9133))

(defun int-schaffer-n2 (x y)
  (declare (optimize (speed 3)))
  (+ 0.5
     (/ (- (^2 (sin (- _x_x _y_y))) 0.5)
        (^2 (1+ _0.001_(+ _x_x _y_y))))))

;; (defun int-schaffer-n4 (x y)
;;   (declare (optimize (speed 3)))
;;   (+ 0.5
;;      (/ (- (^2 (cos (sin (abs (- _x_x _y_y))))) 0.5)
;;         (^2 (1+ _0.001_(+ _x_x _y_y))))
;;      ;; make the optimal solution ~0
;;      -0.292579))
;; 
;; (defun int-styblinski-tang (x y)
;;   (declare (optimize (speed 3)))
;;   (+ (/ (+ _x_x_x_x _-16_x_x _5_x
;;            _y_y_y_y _-16_y_y _5_y)
;;         2)
;;      ;; make the optimal solution ~0
;;      78.33232))

;;; test

(test discrete
  (dolist (fn '(int-rastrigin
                int-ackley
                int-sphere
                int-rosenbrock
                ;; int-beale
                int-goldstein-price
                int-booth
                int-bukin-n6
                int-matyas
                int-levi
                int-three-hump-camel
                ;; int-easom
                ;; int-cross-in-tray
                ;; int-eggholder
                ;; int-holder-table
                ;; int-mccormick
                int-schaffer-n2
                ;; int-schaffer-n4
                ;; int-styblinski-tang
                ))
    (print fn)
    (dolist (optimizer (list (grid-search)))
      (finishes
        (print
         (multiple-value-list
          (tune fn optimizer))))))

  (signals error
    ;; because grid-search does not accept floats
    (tune 'rastrigin (grid-search))))
