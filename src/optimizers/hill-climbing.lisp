
(in-package :dynotune)

(defun hill-climbing (&key (predicate #'<) keep-results &allow-other-keys)
  (declare (boolean keep-results))
  (lambda (function generators)
    (assert (every (of-type 'discrete) generators))
    (let* ((best-params (mapcar #'generate generators))
           (best-result (apply function best-params))
           (acc (if keep-results (list best-params) nil)))
      (iter (for old-best = best-result)
            (apply #'map-product
                   (lambda (&rest parameters)
                     (let ((result (apply function parameters)))
                       (when keep-results
                         (push (list result parameters) acc))
                       (when (or (null best-result)
                                 (funcall predicate result best-result))
                         (setf best-result result
                               best-params parameters))))
                   (mapcar #'neighbor generators best-params))
            (while (or (null old-best) (funcall predicate best-result old-best))))
      (values best-result best-params acc))))

(defun random-restart (&rest args &key (predicate #'<) keep-results (restart 10)
                                    (optimizer (apply #'hill-climbing args)))
  (declare (cl:integer restart))
  (declare (boolean keep-results))
  (lambda (function generators)
    (let (best-result best-params acc)
      (iter (repeat restart)
            (for (values result result-parameters result-acc) = (funcall optimizer function generators))
            (when keep-results
              (appendf acc result-acc))
            (when (or (null best-result)
                      (funcall predicate result best-result))
              (setf best-result result
                    best-params result-parameters)))
      (values best-result best-params acc))))
