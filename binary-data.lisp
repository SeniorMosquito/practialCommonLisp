(in-package :cl-user)

(defpackage :com.gigamonkeys.binary-data
  (:use :common-lisp :com.gigamonkeys.macro-utilities)
  (:export :define-binary-class
           :define-tagged-binary-class
           :define-binary-type
           :read-value
           :write-value
           :*in-progress-objects*
           :parent-of-type
           :current-binary-object
           :+null+))

(in-package :com.gigamonkeys.binary-data)

(defun as-keyword (sym) (intern (string sym) :keyword))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defmacro define-binary-class (name (&rest superclasses) slots)
  (with-gensyms (objectvar streamvar)
    `(define-generic-binary-class
	 ,name
	 ,superclasses
         ,slots
	 (defmethod read-object progn ((,objectvar ,name) ,streamvar)
	   (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
	     ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))))))

(defmacro define-tagged-binary-class (name (&rest superclasses) slots &rest options)
  (with-gensyms (typevar objectvar streamvar)
    `(define-generic-binary-class
	 ,name
	 ,superclasses
         ,slots
	 (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
	   (let ((,objectvar
		  (make-instance
		   ,@(or (cdr (assoc :dispatch options))
			 (error "Must supply :dispatch form."))
		   ,@(mapcan #'slot->keyword-arg slots))))
	     (read-object ,objectvar ,streamvar)
	     ,objectvar)))))

(defun slot->binding (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(,name (read-value ',type ,stream ,@args))))

(defun slot->keyword-arg (spec)
  (let ((name (first spec)))
    `(,(as-keyword name) ,name)))



(defmacro define-generic-binary-class (name (&rest superclasses) slots read-method)
  (with-gensyms (objectvar streamvar)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'slots) ',(mapcar #'first slots))
       (setf (get ',name 'superclasses) ',superclasses))

     (defclass ,name ,superclasses
	,(mapcar #'slot->defclass-slot slots))

     ,read-method

     (defmethod write-object progn ((objectvar ,name) ,streamvar)
       (declare (ignoreable ,streamvar))
       (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
	 ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))


(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

;(defmethod read-value ((type (eql 'iso-8859-1-string)) in &key length)

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun mklist (x) (if (listp x) x (list x)))

(defgeneric write-value (type stream value &key)
  (:documentation "Write a value as the given type to the stream."))

(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Fill in the slots of object from stream."))

(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Write out the slots of object to the stream."))

(defmethod read-value ((type symbol) stream &key)
  (let ((object (make-instance type)))
    (read-object object stream)
    object))

(defmethod write-value ((type symbol) stream value &key)
  (assert (typep value type))
  (write-object value stream))

(defun direct-slots (name)
  (copy-list (get name 'slots)))

(defun inherited-slots (name)
  (loop for super in (get name 'superclasses)
       nconc (direct-slots super)
       nconc (inherited-slots super)))

(defun all-slots (name)
  (nconc (direct-slots name) (inherited-slots name)))

(defun new-class-all-slots (slots superclasses)
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))

(defmacro define-binary-type (name (&rest args) &body spec)
  (ecase (length spec)
    (1
     (with-gensyms (type stream value)
       (destructuring-bind (derived-from &rest derived-args) (mklist (first spec))
	 `(progn
	    (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
	      (read-value ',derived-from ,stream ,@derived-args))
	    (defmethod write-value ((,type (eql ',name)) ,stream ,value &key ,@args)
	      (write-value ',derived-from ,stream ,value ,@derived-args))))))
    (2
     (with-gensyms (type)
       `(progn
	  ,(destructuring-bind ((in) &body body) (rest (assoc :reader spec))
			       `(defmethod read-value ((,type (eql ',name)) ,in &key ,@args)
				  ,@body))
	  ,(destructuring-bind ((out value) &body body) (rest (assoc :writer spec))
			       `(defmethod write-value ((,type (eql ',name)) ,out ,value &key ,@args)
				  ,@body)))))))

(defvar *in-progress-objects* nil)

(defmethod read-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defmethod write-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defun current-binary-object () (first *in-progress-objects*))

(defun parent-of-type (type)
  (find-if #'(lambda (x) (typep x type)) *in-progress-objects*))