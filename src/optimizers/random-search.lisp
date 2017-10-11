
(in-package :dynotune)


(defun random-search (max-trials &key (predicate #'<) keep-results)
  (declare (cl:integer max-trials))
  (declare (boolean keep-results))
  (lambda (function generators)
    (flet ((mapper (i)
             (declare (ignorable i))
             (let* ((parameters (mapcar #'generate generators))
                    (result (apply function parameters)))
               (list result parameters)))
           (reducer (a b)
             (if (funcall predicate (first a) (first b)) a b)))
      (if keep-results
          (let* ((acc (lparallel:pmap 'vector #'mapper (iota max-trials)))
                 (best (lparallel:preduce #'reducer acc)))
            (values (first best) (second best) acc))
          
          (values-list
           (lparallel:pmap-reduce #'mapper #'reducer (iota max-trials)))))))

