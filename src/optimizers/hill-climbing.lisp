
(in-package :dynotune)

(defun hill-climbing (&key (predicate #'<) keep-results &allow-other-keys)
  "Evaluate the neighbors of the current state and move to the first neighbor that improves the result.
Also known as first-choice hill-climbing.

In each neighbor, one of the parameters has the different value from the current state.
The neighbor of a parameter is defined for each generator.
See also: hill-climbing2."
  (declare (boolean keep-results))
  (lambda (function generators)
    (assert (every (of-type 'discrete) generators))
    (let* ((best-params (mapcar #'generate generators))
           (best-result (apply function best-params))
           (acc (if keep-results (list best-params) nil)))
      (iter outer
            (for old-best = best-result)
            (iter (for p in best-params)
                  (for i from 0)
                  (iter (for new-p in (neighbor (elt generators i) p))
                        (for parameters =
                             (append
                              (subseq best-params 0 i)
                              (list new-p)
                              (subseq best-params (1+ i))))
                        (let ((result (apply function parameters)))
                          (when keep-results
                            (push (list result parameters) acc))
                          (when (or (null best-result)
                                    (funcall predicate result best-result))
                            (setf best-result result
                                  best-params parameters)
                            (in outer
                                (next-iteration))))))
            (while (or (null old-best) (funcall predicate best-result old-best))))
      (values best-result best-params acc))))

(defun hill-climbing2 (&key (predicate #'<) keep-results &allow-other-keys)
  "Evaluate ALL neighbors of the current state and move to the BEST neighbor.

In each neighbor, one of the parameters has the different value from the current state.
The neighbor of a parameter is defined for each generator.

See also: hill-climbing."
  (declare (boolean keep-results))
  (lambda (function generators)
    (assert (every (of-type 'discrete) generators))
    (let* ((best-params (mapcar #'generate generators))
           (best-result (apply function best-params))
           (acc (if keep-results (list best-params) nil)))
      (iter (for old-best = best-result)
            (iter (for p in best-params)
                  (for i from 0)
                  (iter (for new-p in (neighbor (elt generators i) p))
                        (for parameters =
                             (append
                              (subseq best-params 0 i)
                              (list new-p)
                              (subseq best-params (1+ i))))
                        (let ((result (apply function parameters)))
                          (when keep-results
                            (push (list result parameters) acc))
                          (when (or (null best-result)
                                    (funcall predicate result best-result))
                            (setf best-result result
                                  best-params parameters)))))
            (while (or (null old-best) (funcall predicate best-result old-best))))
      (values best-result best-params acc))))

(defun random-restart (&rest args &key (predicate #'<) keep-results (restart 10) (optimizer 'hill-climbing))
  (declare (cl:integer restart))
  (declare (boolean keep-results))
  (when (symbolp optimizer)
    (setf optimizer (apply optimizer args)))
  (lambda (function generators)
    (flet ((mapper (i)
             (declare (ignorable i))
             (future
              (multiple-value-list (funcall optimizer function generators))))
           (reducer (a b)
             (if (funcall predicate (first a) (first b)) a b)))
      (if keep-results
          (let* ((acc (mapcar #'force (mapcar #'mapper (iota restart))))
                 (best (reduce #'reducer acc)))
            (values (first best) (second best)
                    (reduce #'append acc :key #'third)))
          (values-list
           (reduce #'reducer (mapcar #'force (mapcar #'mapper (iota restart)))))))))
