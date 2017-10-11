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
           ,result)))))


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

(defvar *a* (make-matrix 1024 2048))
(defvar *b* (make-matrix 2048 1024))
(defvar *c* (make-matrix 1024 1024))

;;; simple loops

(defun gemm-inner-prod (a b c)
  "ijk loop. 20sec"
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (declare (type (matrix) a b c))
  ;; (N,M)x(M,L)=(N,L)
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
  "kji loop. 98sec"
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (declare (type (matrix) a b c))
  ;; (N,M)x(M,L)=(N,L)
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
  "ikj loop. 8.9sec"
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (declare (type (matrix) a b c))
  ;; (N,M)x(M,L)=(N,L)
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

;;; unrolled loops

(defun make-unrolled-gemm-middle-prod (ui uk uj)
  (compile*
   `(lambda (a b c)
      (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
      (declare (type (matrix) a b c))
      #+sbcl
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (let ((N (array-dimension a 0))       ;i
            (M (array-dimension a 1))       ;k
            (L (array-dimension b 1)))      ;j
        (dotimes-unroll (bi oi N ,ui)
          (let ((i (+ bi oi)))
            (dotimes-unroll (bj oj L ,uj)
              (setf (aref c i (+ bj oj)) 0.0d0))
            (dotimes-unroll (bk ok M ,uk)
              (let* ((k (+ bk ok))
                     (tmp-a (aref a i k)))
                (declare (double-float tmp-a))
                (dotimes-unroll (bj oj L ,uj)
                  (let ((j (+ bj oj)))
                    (incf (aref c i j)
                          (* tmp-a (aref b k j)))))))))))))

(defmacro benchmark (&body body)
  (with-gensyms (start end)
    `(let ((,start (get-internal-run-time)))
       ,@body
       (let ((,end (get-internal-run-time)))
         (/ (float (- ,end ,start))
            internal-time-units-per-second)))))

(function-cache:defcached unrolled-gemm-middle-prod (ui uk uj)
  (let ((fn (make-unrolled-gemm-middle-prod ui uk uj)))
    (let ((result (benchmark
                    (funcall fn
                             (make-matrix 512 1024)
                             (make-matrix 1024 512)
                             (make-matrix 512 512)))))
      (print (list ui uk uj '-> result))
      result)))

;;; test

(test matrix-unroll
  (finishes
    (print
     (multiple-value-list
      (tune 'unrolled-gemm-middle-prod
            'random-restart
            '((ordinal 1 2 4)
              (ordinal 1 2 4)
              (ordinal 1 2 4 8)))))))
