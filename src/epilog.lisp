(in-package :cl-user)
(defpackage epilog
  (:use :cl)
  (:import-from :anaphora
                :aif :awhen :acond)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :epilog.pattern
		:match
		:pattern-match
                :vars-in
		:simple?)
  (:export :*default-epilog*
           :epilog
           :make-epilog
	   :clear
	   :epilog-query
	   :epilog-push
	   :fact
	   :pattern-match))
(in-package :epilog)

(defclass epilog ()
  ((database :initarg :hash-table :initform (make-hash-table :size 10000) :accessor database)))

(defun make-epilog ()
  (make-instance 'epilog))

(defparameter *default-epilog* (make-epilog))

(defgeneric clear (epilog))
(defmethod clear ((epi epilog))
  (clrhash (database epi)))

(defmacro epilog-query (key &optional (epi *default-epilog*))
  `(gethash ,key (database ,epi)))
  
(defgeneric epilog-push (symbol list epilog))
(defmethod epilog-push ((key symbol) (val list) (epi epilog))
  (push val (epilog-query key epi)))

(defmacro fact (predicate param-list &optional (epi *default-epilog*))
  `(progn
     (epilog-push ',predicate ',param-list ,epi)
     ',param-list))


(defmacro with-answer (query &body body)
  `(with-gensyms ,(vars-in query #'simple?)
     ,(compile-query query `(progn ,@body))))

(defun compile-query (q body)
  (case (car q)
    (and (compile-and (cdr q) body))
    (or (compile-or (cdr q) body))
    (not (compile-not (cadr q) body))
    (listp `(if ,(cadr q) ,body))
    (t (compile-simple q body))))

(defun compile-simple (q body)
  (let ((fact (gensym)))
    `(dolist (,fact (epilog-query ',(car q)))
       (pattern-match ,(cdr q) ,fact ,body nil))))


(defun compile-and (clauses body)
  (if (null clauses)
      body
      (compile-query (car clauses)
		     (compile-and (cdr clauses) body))))

(defun compile-or (clauses body)
  (if (null clauses)
      nil
      (let ((gbod (gensym))
	    (vars (vars-in body #'simple?)))
	`(labels ((,gbod ,vars ,body))
	   ,@(mapcar #'(lambda (cl)
			 (compile-query cl `(,gbod ,@vars)))
		     clauses)))))

(defun compile-not (q body)
  (let ((tag (gensym)))
    `(if (block ,tag
	   ,(compile-query q `(return-from ,tag nil))
	   t)
	 ,body)))
