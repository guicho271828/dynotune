(in-package :dynotune.test)
(in-suite :dynotune)

;;; unroller utils
;; copied from https://github.com/guicho271828/play-on-matrix

(defmacro dotimes-inline ((var count &optional result-form) &body body &environment env)
  (check-type var symbol)
  (let ((count (macroexpand count env)))
    (assert (and (constantp count) (numberp count)))
    (iter (for c to count)
          (when (first-iteration-p)
            (collect 'progn))
          (collect
              (if (< c count)
                  `(symbol-macrolet ((,var ,c))
                     ,@body)
                  result-form)))))

;; much simpler syntax
(defmacro dotimes-unroll ((base offset
                            count unroll
                            &optional result)
                           &body body
                           &environment env)
  "
BASE: symbol 
OFFSET: symbol
COUNT: number
UNROLL: number, the unroll factor. It should expand into a constant.

BASE is bound to the base index of a single unrolled loop.
OFFSET is bound to the offset of the current index from the base index.
The current index should be computed manually, which is base + offset.
"
  (with-gensyms (quat mod nth-loop)
    (let ((unroll (macroexpand unroll env))
          (delta (gensym "D")))
      (if (= unroll 1)
          `(symbol-macrolet ((,base 0))
             (dotimes (,offset ,count)
               ,@body))
          `(let ((,base 0))
             (declare (fixnum ,base))
             (multiple-value-bind (,quat ,mod) (floor ,count ,unroll)
               (declare (fixnum ,quat)
                        ((mod ,unroll) ,mod))
               (symbol-macrolet ((,delta ,unroll))
                 (dotimes (,nth-loop ,quat)
                   (declare (ignorable ,nth-loop))
                   (dotimes-inline (,offset ,unroll)
                     ,@body)
                   (incf ,base ,delta)))
               (symbol-macrolet ((,delta 1)
                                 (,offset 0))
                 (dotimes (,nth-loop ,mod)
                   ,@body
                   (incf ,base ,delta)))
               ,result))))))


(test unroll
  (finishes
    (dotimes-unroll (base offset 16 2)
      (print (list base offset (+ base offset))))))


(defun make-matrix (rows cols)
  (make-array (list rows cols) :element-type 'double-float))

(deftype matrix (&optional rows cols)
  `(simple-array double-float (,rows ,cols)))

(defun compile* (form &optional verbose)
  (let ((fn (compile nil form)))
    (when verbose
      (disassemble fn))
    fn))

(defparameter *a* (make-matrix 512 1024))
(defparameter *b* (make-matrix 1024 512))
(defparameter *c* (make-matrix 512 512))

(defmacro benchmark (&body body)
  (with-gensyms (start end)
    `(let ((,start (get-internal-real-time)))
       ,@body
       (let ((,end (get-internal-real-time)))
         (/ (float (- ,end ,start))
            internal-time-units-per-second)))))

;;; simple loops

(defun gemm-inner-prod (a b c)
  "ijk loop. 6.712sec"
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (declare (type (matrix 512 1024) a))
  (declare (type (matrix 1024 512) b))
  (declare (type (matrix 512 512) c))
  (let ((N (array-dimension a 0))       ;i
        (M (array-dimension a 1))       ;k
        (L (array-dimension b 1)))      ;j
    (dotimes (i N)
      (dotimes (j L)
        (let ((tmp-c 0.0d0))
          (declare (double-float tmp-c))
          (dotimes (k M)
            (incf tmp-c (* (aref a i k)
                           (aref b k j))))
          (setf (aref c i j) tmp-c))))))

(defun gemm-outer-prod (a b c)
  "kji loop. 7.181sec"
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (declare (type (matrix 512 1024) a))
  (declare (type (matrix 1024 512) b))
  (declare (type (matrix 512 512) c))
  (let ((N (array-dimension a 0))       ;i
        (M (array-dimension a 1))       ;k
        (L (array-dimension b 1)))      ;j
    (dotimes (i N)
      (dotimes (j L)
        (setf (aref c i j) 0.0d0)))
    (dotimes (k M)
      (dotimes (j L)
        (let ((tmp-b (aref b k j)))
          (declare (double-float tmp-b))
          (dotimes (i N)
            (incf (aref c i j)
                  (* (aref a i k)
                     tmp-b))))))))

(defun gemm-middle-prod (a b c)
  "ikj loop. 1.176sec"
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (declare (type (matrix 512 1024) a))
  (declare (type (matrix 1024 512) b))
  (declare (type (matrix 512 512) c))
  (let ((N (array-dimension a 0))       ;i
        (M (array-dimension a 1))       ;k
        (L (array-dimension b 1)))      ;j
    (dotimes (i N)
      (dotimes (j L)
        (setf (aref c i j) 0.0d0))
      (dotimes (k M)
        (let ((tmp-a (aref a i k)))
          (declare (double-float tmp-a))
          (dotimes (j L)
            (incf (aref c i j)
                  (* tmp-a (aref b k j)))))))))

(test simple-gemm
  (finishes (print (benchmark (gemm-inner-prod *a* *b* *c*))))
  (finishes (print (benchmark (gemm-outer-prod *a* *b* *c*))))
  (finishes (print (benchmark (gemm-middle-prod *a* *b* *c*)))))

;;; unrolled loops

(defun make-unrolled-gemm-middle-prod (N M L ui uk uj)
  (assert (zerop (rem N ui)))
  (assert (zerop (rem M uk)))
  (assert (zerop (rem L uj)))
  `(lambda (a b c)
     (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
     (declare (type (matrix ,N ,M) a))
     (declare (type (matrix ,M ,L) b))
     (declare (type (matrix ,N ,L) c))
     #+sbcl
     (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
     (dotimes-unroll (bi oi ,N ,ui)
       (let ((i (+ bi oi)))
         (dotimes-unroll (bj oj ,L ,uj)
           (setf (aref c i (+ bj oj)) 0.0d0))
         (dotimes-unroll (bk ok ,M ,uk)
           (let* ((k (+ bk ok))
                  (tmp-a (aref a i k)))
             (declare (double-float tmp-a))
             (dotimes-unroll (bj oj ,L ,uj)
               (let ((j (+ bj oj)))
                 (incf (aref c i j)
                       (* tmp-a (aref b k j)))))))))))


(function-cache:defcached unrolled-gemm-middle-prod (ui uk uj)
  (let ((fn (compile* (make-unrolled-gemm-middle-prod 512 1024 512 ui uk uj)))
        (a (make-matrix 512 1024))
        (b (make-matrix 1024 512))
        (c (make-matrix 512 512)))
    (let ((result (benchmark (funcall fn a b c))))
      (print (list ui uk uj :-> result))
      result)))

(test matrix-unroll
  (finishes
    (print
     (multiple-value-list
      (tune 'unrolled-gemm-middle-prod
            'random-restart
            '((ordinal 1 2 4)
              (ordinal 1 2 4)
              (ordinal 1 2 4 8)))))))

;; 0.636
;; (4 1 8)

;;; blocked loops

(defun make-blocked-gemm-middle-prod (N M L iw kw jw)
  (assert (zerop (rem N iw)))
  (assert (zerop (rem M kw)))
  (assert (zerop (rem L jw)))
  `(lambda (a b c)
     (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
     (declare (type (matrix ,N ,M) a))
     (declare (type (matrix ,M ,L) b))
     (declare (type (matrix ,N ,L) c))
     #+sbcl
     (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
     (loop for ib below ,N by ,iw do
          (loop for kb below ,L by ,kw do
               (loop for jb below ,M by ,jw do
                    (loop for i from ib below (+ ib ,iw) do
                         (loop for j from jb below (+ jb ,jw) do
                              (setf (aref c i j) 0.0d0))
                         (loop for k from kb below (+ kb ,kw) do
                              (let ((tmp-a (aref a i k)))
                                (declare (double-float tmp-a))
                                (loop for j from jb below (+ jb ,jw) do
                                     (incf (aref c i j)
                                           (* tmp-a (aref b k j))))))))))))

(function-cache:defcached blocked-gemm-middle-prod (iw kw jw)
  (let ((fn (compile* (make-blocked-gemm-middle-prod 512 1024 512 iw kw jw)))
        (a (make-matrix 512 1024))
        (b (make-matrix 1024 512))
        (c (make-matrix 512 512)))
    (let ((result (benchmark (funcall fn a b c))))
      (print (list iw kw jw :-> result))
      result)))

(test matrix-blocked
  (finishes
    (print
     (multiple-value-list
      (tune 'blocked-gemm-middle-prod
            'random-restart
            '((ordinal 2 4 8 16)
              (ordinal 2 4 8 16)
              (ordinal 2 4 8 16)))))))

;; 0.811
;; (2 8 8)

;;; blocked + unrolled

(defun make-unrolled+blocked-gemm-middle-prod (N M L iw kw jw uj) ; ui uk
  (assert (zerop (rem N iw)))
  (assert (zerop (rem M kw)))
  (assert (zerop (rem L jw)))
  ;; (assert (zerop (rem iw ui)))
  ;; (assert (zerop (rem kw uk)))
  (assert (zerop (rem jw uj)))
  `(lambda (a b c)
     (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
     (declare (type (matrix ,N ,M) a))
     (declare (type (matrix ,M ,L) b))
     (declare (type (matrix ,N ,L) c))
     #+sbcl
     (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
     
     (loop for ib below ,N by ,iw do
          (loop for kb below ,L by ,kw do
               (loop for jb below ,M by ,jw do
                    (loop for i from ib below (+ ib ,iw)
                       do
                         (loop
                            with j = jb
                            repeat ,(/ jw uj)
                            do
                              (progn
                                ,@(iter (for jo below uj)
                                        (unless (first-iteration-p)
                                          (collecting '(incf j)))
                                        (collecting
                                         `(setf (aref c i j) 0.0d0)))))
                       do
                         (loop for k from kb below (+ kb ,kw) do
                              (let ((tmp-a (aref a i k)))
                                (declare (double-float tmp-a))
                                (loop
                                   with j = jb
                                   repeat ,(/ jw uj)
                                   do
                                     (progn
                                       ,@(iter (for jo below uj)
                                               (unless (first-iteration-p)
                                                 (collecting '(incf j)))
                                               (collecting
                                                `(incf (aref c i j) (* tmp-a (aref b k j)))))))))))))))

(function-cache:defcached unrolled+blocked-gemm-middle-prod (iw kw jw uj)
  (let ((fn (compile* (make-unrolled+blocked-gemm-middle-prod 512 1024 512 iw kw jw uj)))
        (a (make-matrix 512 1024))
        (b (make-matrix 1024 512))
        (c (make-matrix 512 512)))
    (let ((result (benchmark (funcall fn a b c))))
      (print (list iw kw jw uj :-> result))
      result)))

(test matrix-unrolled+blocked
  (finishes
    (print
     (multiple-value-list
      (tune 'unrolled+blocked-gemm-middle-prod
            'random-restart
            '((ordinal 8 16 32)
              (ordinal 8 16 32)
              (ordinal 8 16 32)
              (ordinal 1 2 4 8)))))))

;; 0.691
;; (8 8 16 4)
