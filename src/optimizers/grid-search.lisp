
(in-package :dynotune)


(defun grid-search (&key (predicate #'<) keep-results)
  (declare (boolean keep-results))
  (lambda (function generators)
    (assert (every (of-type 'discrete) generators))
    (flet ((reducer (a b)
             (if (funcall predicate (first a) (first b)) a b)))
      
      (let* ((futures (apply #'map-product
                             (lambda (&rest parameters)
                               (future
                                 (let ((result (apply function parameters)))
                                   (list result parameters))))
                             (mapcar #'all generators)))
             (acc (mapcar #'force futures))
             (best (preduce #'reducer acc)))
        (if keep-results
            (values (first best) (second best) acc)
            (values (first best) (second best)))))))

