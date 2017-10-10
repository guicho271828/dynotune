
(in-package :dynotune)


(defun grid-search (&key (mode :minimize) keep-results)
  (lambda (function generators)
    (assert (every (of-type '(or member integer)) generators))
    (let (acc best-result best-param)
      (apply #'map-product
             (lambda (&rest parameters)
               (let ((result (apply function parameters)))
                 (when keep-results
                   (push (list result parameters) acc))
                 (ecase mode
                   (:minimize
                    (when (or (null best-result) (< result best-result))
                      (setf best-result result
                            best-param parameters)))
                   (:maximize
                    (when (or (null best-result) (> result best-result))
                      (setf best-result result
                            best-param parameters))))))
             (mapcar #'all generators))
      (values best-result
              best-param
              acc))))

