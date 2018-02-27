(in-package :cl-user)
(defpackage epilog.destruct
  (:use :cl)
  (:import-from :anaphora
                :aif :awhen :acond)
  (:import-from :alexandria
                :with-gensyms)
  (:export :dbind
	   :destruc))
(in-package :epilog.destruct)



#|
Distribute operators
|#

(defun dbind-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(let ,(mapcar #'(lambda (b)
                         (if (consp (car b))
                             (car b)
                             b))
                     binds)
         ,(dbind-ex (mapcan #'(lambda (b)
                                (if (consp (car b))
                                    (cdr b)))
                            binds)
                    body))))
                                  

(defmacro dbind (pattern sequence &body body)
  (let ((goal-sequence (gensym)))
    `(let ((,goal-sequence ,sequence))
       ,(dbind-ex (destruc pattern goal-sequence #'atom) body))))


(defun destruc (pattern sequence &optional (atom? #'atom) (n 0))
  (if (null pattern)
      nil
      (let ((rest (cond ((funcall atom? pattern) pattern)
                        ((eq (car pattern) '&rest) (cadr pattern))
                        ((eq (car pattern) '&body) (cadr pattern))
                        (t nil))))
        (if rest
            `((,rest (subseq ,sequence ,n)))
            (let ((p (car pattern))
                  (rec (destruc (cdr pattern) sequence atom? (1+ n))))
              (if (funcall atom? p)
                  (cons `(,p (elt ,sequence ,n)) rec)
                  (let ((var (gensym)))
                    (cons (cons `(,var (elt ,sequence ,n))
                                (destruc p var atom?))
                          rec))))))))
