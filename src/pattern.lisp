(in-package :cl-user)
(defpackage epilog.pattern
  (:use :cl)
  (:import-from :anaphora
		:aif :awhen :acond)
  (:import-from :alexandria
		:with-gensyms)
  (:import-from :epilog.destruct
		:dbind
		:destruc)
  (:export :match
	   :vars-in
	   :pattern-match
	   :if-match
	   :get-match
	   :match1
	   :simple?
           :varsym?
	   :gensym?))
(in-package :epilog.pattern)

;; acondではたちを返すアナフォリックマクロとして実現していないため
(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
	    (val (gensym))
	    (win (gensym)))
	`(multiple-value-bind (,val ,win) ,(car cl1)
	  (if (or ,val ,win)
	      (let ((it ,val)) ,@(cdr cl1))
	      (acond2 ,@(cdr clauses)))))))

#|
pattern match
|#
(defun match (x y &optional binds)
  (acond2
   ((or (eql x y) (string= (format nil "~A" x) '_) (string= (format nil "~A" y) '_))
    (values binds t))
   ((binding x binds) (match it y binds))
   ((binding y binds) (match x it binds))
   ((varsym? x) (values (cons (cons x y) binds) t))
   ((varsym? y) (values (cons (cons y x) binds) t))
   ((and (consp x) (consp y) (match (car x) (car y) binds))
    (match (cdr x) (cdr y) it))
   (t
    (values nil nil))))

(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun binding (x binds)
  (labels ((recbind (x binds)
	     (let ((search-result (assoc x binds)))
	       (if search-result
                  (or (recbind (cdr search-result) binds) search-result)))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr) atom?)
             (vars-in (cdr expr) atom?))))

(defun var? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defmacro if-match (pattern sequence then &optional else)
  `(let ,(mapcar #'(lambda (v)
                     `(,v ',(gensym)))
                 (vars-in pattern #'simple?))
     (pattern-match ,pattern ,sequence ,then ,else)))

(defmacro pattern-match (pattern sequence then else)
  (if (simple? pattern)
      (match1 `((,pattern ,sequence)) then else)
      (with-gensyms (goal-sequence goal-else)
        `(labels ((,goal-else () ,else))
                 ,(gen-match (cons (list goal-sequence sequence)
                                  (destruc pattern goal-sequence #'simple?))
                 then
                 `(,goal-else))))))

(defun simple? (x)
  (or (atom x) (eq (car x) 'quote)))

(defun gen-match (refs then else)
  (if (null refs)
      then
      (let ((then (gen-match (cdr refs) then else)))
        (if (simple? (caar refs))
            (match1 refs then else)
            (gen-match (car refs) then else)))))

(defun match1 (refs then else)
  (dbind ((pattern expr) . rest) refs
    (cond ((gensym? pattern)
	   `(let ((,pattern ,expr))
	      (if (and (typep ,pattern 'sequence)
		       ,(length-test pattern rest))
		  ,then
		  ,else)))
	  ((string= (format nil "~A" pattern) '_) then)
	  ((var? pattern)
	   (let ((ge (gensym)))
	     `(let ((,ge ,expr))
		(if (or (gensym? ,pattern) (equal ,pattern ,ge))
		    (let ((,pattern ,ge)) ,then)
		    ,else))))
	  (t `(if (equal ,pattern ,expr) ,then ,else)))))

(defun gensym? (s)
  (and (symbolp s) (not (symbol-package s))))

(defun length-test (pattern rest)
  (let ((fin (caadar (last rest))))
    (if (or (consp fin) (eq fin 'elt))
        `(= (length ,pattern) ,(length rest))
        `(> (length ,pattern) ,(- (length rest) 2)))))
