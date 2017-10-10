(in-package :dynotune)


(defun simulated-annealing (&key (predicate #'<) keep-results)
  (declare (boolean keep-results))
  (lambda (function generators)
    (assert (every (of-type 'discrete) generators))
    (let (acc best-result best-params)
      (apply #'map-product
             (lambda (&rest parameters)
               (let ((result (apply function parameters)))
                 (when keep-results
                   (push (list result parameters) acc))
                 (when (or (null best-result)
                           (funcall predicate result best-result))
                   (setf best-result result
                         best-params parameters))))
             (mapcar #'all generators))
      (values best-result
              best-params
              acc))))

