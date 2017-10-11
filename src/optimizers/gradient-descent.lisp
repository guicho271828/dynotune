
(in-package :dynotune)


(defun converged (threshold)
  (lambda (acc)
    (match acc
      ((list* (list* now _)
              (list* prev _)
              _)
       (< (abs (- now prev)) threshold)))))
       

(defun gradient-descent (&key (predicate #'<) keep-results (dx 0.001) (lr 0.001) (epoch 1000) (stop (converged 0.001)) &allow-other-keys)
  ""
  (declare (boolean keep-results))
  (lambda (function generators)
    (assert (every (of-type 'continuous) generators))
    (let* ((x (mapcar #'generate generators))
           (y (apply function x))
           (acc (if keep-results (list (list y x)) nil)))
      (iter (repeat epoch)
            (for ∇f = (iter (for xi in x)
                             (for i from 0)
                             (for _x = (copy-seq x))
                             (setf (elt _x i) (coerce (+ xi dx) (type-of xi)))
                             (for _y = (apply function _x))
                             (for dy = (- _y y))
                             (collect (/ dy dx))))
            (format t "~&y=~a~%" y)
            (setf x (mapcar (lambda (xi ∇fi) (- xi (* lr ∇fi))) x ∇f)
                  y (apply function x))
            (push (list y x) acc)
            (until (funcall stop acc)))
      (values y x
              (when keep-results
                (sort acc predicate :key #'first))))))
