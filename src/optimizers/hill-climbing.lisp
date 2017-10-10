
(in-package :dynotune)


(defun hill-climbing (max-trials &optional (mode :minimize) keep-results)
  (lambda (function generators)
    (let (acc)
      (values
       (ecase mode
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
                         minimizing result))))
       acc))))

