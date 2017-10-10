
(in-package :dynotune)


(defun random-search (max-trials &key (predicate #'<) keep-results)
  (declare (cl:integer max-trials))
  (declare (boolean keep-results))
  (lambda (function generators)
    (let (acc)
      (let (best-result best-params)
        (iter (repeat max-trials)
              (for parameters = (mapcar #'generate generators))
              (for result = (apply function parameters))
              (when keep-results
                (push (list result parameters) acc))
              (when (or (null best-result)
                        (funcall predicate result best-result))
                (setf best-result result
                      best-params parameters)))
        (values best-result
                best-params
                acc)))))

