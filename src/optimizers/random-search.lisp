
(in-package :dynotune)


(defun random-search (max-trials &key (mode :minimize) keep-results)
  (lambda (function generators)
    (let (acc)
      (let ((best (ecase mode
                    (:minimize
                     (iter (repeat max-trials)
                           (for parameters = (mapcar #'generate generators))
                           (for result = (apply function parameters))
                           (when keep-results
                             (push (list result parameters) acc))
                           (finding (list result parameters)
                                    minimizing result)))
                    (:maximize
                     (iter (repeat max-trials)
                           (for parameters = (mapcar #'generate generators))
                           (for result = (apply function parameters))
                           (when keep-results
                             (push (list result parameters) acc))
                           (finding (list result parameters)
                                    minimizing result))))))
        (match best
          ((list result parameters)
           (values
            result
            parameters
            acc)))))))

