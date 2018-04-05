(in-package :cl-user)
(defpackage epilog.basic
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
  (:export :epilog-system
           :make-epilog
	   :clear
	   :epilog-query
	   :epilog-push
	   :fact
	   :pattern-match))
(in-package :epilog.basic)

(defclass epilog-system ()
  ((database :initarg :hash-table :initform (make-hash-table :size 10000) :accessor database)))

(defun make-epilog ()
  (make-instance 'epilog-system))

(defparameter *default-epilog* (make-epilog))

(defgeneric clear (epilog-system))
(defmethod clear ((epi epilog-system))
  (clrhash (database epi)))

(defmacro epilog-query (key &optional (epilog-system *default-epilog*))
  `(gethash ,key (database ,epilog-system)))
  
(defgeneric epilog-push (symbol list epilog-system))
(defmethod epilog-push ((key symbol) (val list) (epilog-system epilog-system))
  (push val (epilog-query key epilog-system)))

;; Usage: (epilog:fact painter (reynolds) *a*)
(defmacro fact (predicate param-list &optional (epilog-system *default-epilog*))
  `(progn
     (epilog-push ',predicate
		  ',param-list
		  ,epilog-system)
     ',param-list))


(defmacro with-answer (query &body body)
  `(with-gensyms ,(vars-in query #'simple?)
     ,(compile-query query `(progn ,@body))))

(defun compile-query (q body)
  (cond
    ((string= (format nil "~A" (car q)) "AND") (compile-and (cdr q) body))
    ((string= (format nil "~A" (car q)) "OR") (compile-or (cdr q) body))
    ((string= (format nil "~A" (car q)) "NOT") (compile-not (cadr q) body))
    ((string= (format nil "~A" (car q)) "LISP") `(if ,(cadr q) ,body))
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
