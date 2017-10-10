
(in-package :dynotune)

;;; abstract classes

(defclass set ()
  ((value-type :initarg :type :initform t)))

(defclass categorical (set) ()
  (:documentation "Ordering does not exist."))

(defclass ordinal (set)
  ((high :initarg :high :initform (error ":high required!"))
   (low :initarg :low :initform (error ":low required!")))
  (:documentation "Ordering exists."))

(defclass interval (ordinal) ()
  (:documentation "The intervals between adjacent elements are equal, i.e. the distance makes sense."))

(defclass continuous (interval) ()
  (:documentation "You can take an arbitrary small unit amount."))

(defclass member ()
  ((objects :initarg :objects :initform (error ":objects required!"))))

;;; instances actually being used

(defclass categorical-member (member categorical) ())
(defclass ordinal-member     (member ordinal) ())
(defclass interval-member    (member interval) ())

(defclass integer (interval) () (:default-initargs :type 'cl:integer))
(defclass single-float (continuous) ()  (:default-initargs :type 'cl:single-float))
(defclass double-float (continuous) () (:default-initargs :type 'cl:double-float))
(defclass short-float (continuous) () (:default-initargs :type 'cl:short-float))
(defclass long-float (continuous) () (:default-initargs :type 'cl:long-float))

(deftype discrete () `(or member integer))

(defgeneric generate (set))

(defmethod generate ((set interval))
  (with-slots (low high) set
    (+ low (random (- high low)))))

(defmethod generate ((set member))
  (with-slots (objects) set
    (random-elt objects)))

(defgeneric all (finite-set))
(defmethod all ((set member))
  (with-slots (objects) set
    objects))
(defmethod all ((set integer))
  (with-slots (low high) set
    (iota (- high low) :start low)))

(defgeneric neighbor (finite-set current))
(defmethod neighbor ((set categorical-member) current)
  (all set))
(defmethod neighbor ((set ordinal-member) current)
  (with-slots (objects) set
    (let ((pos (position current objects)))
      (list (elt objects (1- pos)) (elt objects (1+ pos))))))
(defmethod neighbor ((set integer) (current cl:integer))
  (with-slots (low high) set
    (let (acc)
      (when (<= (1+ current) high)
        (push (1+ current) acc))
      (when (<= low (1- current))
        (push (1- current) acc))
      acc)))



;;; specifiers

(defun parse-generator (form)
  (match form
    ;; commented out until this is absolutely necessary
    ;; ((single-float-type (or '* (= most-negative-single-float)) (or '* (= most-positive-single-float)))
    ;;  (make-instance 'single-float :low (/ most-negative-single-float 4) :high (/ most-positive-single-float 4)))
    ;; ((double-float-type (or '* (= most-negative-double-float)) (or '* (= most-positive-double-float)))
    ;;  (make-instance 'double-float :low (/ most-negative-double-float 4) :high (/ most-positive-double-float 4)))
    ;; ((short-float-type (or '* (= most-negative-short-float)) (or '* (= most-positive-short-float)))
    ;;  (make-instance 'short-float :low (/ most-negative-short-float 4) :high (/ most-positive-short-float 4)))
    ;; ((long-float-type (or '* (= most-negative-long-float)) (or '* (= most-positive-long-float)))
    ;;  (make-instance 'long-float :low (/ most-negative-long-float 4) :high (/ most-positive-long-float 4)))
    ((single-float-type low high)
     (make-instance 'single-float :low low :high high))
    ((double-float-type low high)
     (make-instance 'double-float :low low :high high))
    ((short-float-type low high)
     (make-instance 'short-float :low low :high high))
    ((long-float-type low high)
     (make-instance 'long-float :low low :high high))
    ((integer-subtype low high)
     (make-instance 'integer :low low :high high))
    ((list* (or 'cl:member 'categorical) objects) ;member is a synonym for categorical
     (make-instance 'categorical-member :objects objects))
    ((list* 'ordinal objects)
     (make-instance 'ordinal-member :objects objects))
    ((list* 'interval objects)
     (make-instance 'interval-member :objects objects))
    (_
     (error "Failed to parse the generator specifier form:~% ~a~%
should be of integer, short/single/double/long-float, member, categorical, ordinal, interval."
            form))))
