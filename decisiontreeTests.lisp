(in-package :decisiontree)


(defparameter *instance-list* (loop for i from 1 to 14 collect (make-instance 'instance)))

(mapcar #'(lambda(x y)
	    (mapcar #'(lambda(z)
			(set-attribute-value (first z) (second z) y))
		    x))
;; 	'(((height short) (hair blond)    (eyecolor blue) )
;; 	  ((height tall)  (hair blond)    (eyecolor brown) )
;; 	  ((height tall)  (hair red)      (eyecolor blue) )
;; 	  ((height short) (hair dark)     (eyecolor blue) )
;; 	  ((height tall)  (hair dark)     (eyecolor blue) )
;; 	  ((height tall)  (hair blond)    (eyecolor blue) )
;; 	  ((height tall)  (hair dark)     (eyecolor brown) )
;; 	  ((height short) (hair blond)    (eyecolor brown) ))
;; 	*instance-list*)
	'(((outlook Sunny) (temp Hot) (humidity High) (wind Weak))
	  ((outlook Sunny) (temp Hot) (humidity High) (wind Strong))
	  ((outlook Overcast) (temp Hot) (humidity High) (wind Weak))
	  ((outlook Rain) (temp Mild) (humidity High) (wind Weak))
	  ((outlook Rain) (temp Cool) (humidity Normal) (wind Weak))
	  ((outlook Rain) (temp Cool) (humidity Normal) (wind Strong))
	  ((outlook Overcast) (temp Cool) (humidity Normal) (wind Strong))
	  ((outlook Sunny) (temp Mild) (humidity High) (wind Weak))
	  ((outlook Sunny) (temp Cool) (humidity Normal) (wind Weak))
	  ((outlook Rain) (temp Mild) (humidity Normal) (wind Weak))
	  ((outlook Sunny) (temp Mild) (humidity Normal) (wind Strong))
	  ((outlook Overcast) (temp Mild) (humidity High) (wind Strong))
	  ((outlook Overcast) (temp Hot) (humidity Normal) (wind Weak))
	  ((outlook Rain) (temp Mild) (humidity High) (wind Strong)))
	*instance-list*)

(mapcar #'(lambda(x y)
	    (setf (class-name x) y))
	*instance-list*
	 '(No No Yes Yes Yes No Yes No Yes Yes Yes Yes Yes No)) ;o
;	'(yes no yes no no yes no no))



(lisp-unit:define-test populate-hash-test
  (lisp-unit:assert-error 'simple-error (populate-attr-hash t))
  (let ((instance (make-instance 'instance)))
    (setf (gethash 'a (attributes instance)) 0)
    (let ((test-hash (populate-attr-hash instance)))
      (maphash #'(lambda(k v)
		   (declare (ignore v))
		   (lisp-unit:assert-true (gethash k test-hash))
		   (lisp-unit:assert-equal 0 (gethash k test-hash)))
	       (attributes instance)))))

(lisp-unit:define-test get-class-list-test
  (let ((instance-1 (make-instance 'instance))
	(instance-2 (make-instance 'instance))
	(instance-3 (make-instance 'instance))
	(instance-list nil))
    (push instance-1 instance-list)
    (push instance-2 instance-list)
    (push instance-3 instance-list)
    (setf (class-name instance-1) 'a)
    (setf (class-name instance-2) 'b)
    (setf (class-name instance-3) 'a)
    (lisp-unit:assert-true (every #'(lambda(x)
				      (member x '(a b)))
				  (get-class-list instance-list)))))

(lisp-unit:define-test log-function-test
  (lisp-unit:assert-equal 0 (log-function 0))
  (lisp-unit:assert-equal 0.0 (log-function 1)))

(lisp-unit:define-test class-sort-test
  (let ((instance-1 (make-instance 'instance))
	(instance-2 (make-instance 'instance))
	(instance-3 (make-instance 'instance))
	(instance-list nil))
    (push instance-1 instance-list)
    (push instance-2 instance-list)
    (push instance-3 instance-list)
    (setf (class-name instance-1) 'a)
    (setf (class-name instance-2) 'b)
    (setf (class-name instance-3) 'a)
    (lisp-unit:assert-equal nil (sort-instances nil))
    (lisp-unit:assert-equal '((a a) (b)) (sort-instances instance-list))))

(lisp-unit:define-test calculate-entropy
  (let ((instance-1 (make-instance 'instance))
	(instance-2 (make-instance 'instance))
	(instance-3 (make-instance 'instance))
	(instance-list nil))
    (push instance-1 instance-list)
    (push instance-2 instance-list)
    (push instance-3 instance-list)
    
    (setf (class-name instance-1) 'a)
    (setf (class-name instance-2) 'b)
    (setf (class-name instance-3) 'a)
    (lisp-unit:assert-equal 0 (calculate-entropy '()))
    (lisp-unit:assert-equal (+ (log-function (/ 2 3))
			       (log-function (/ 1 3)))

			    (calculate-entropy instance-list))))


(lisp-unit:define-test get-attribute-list
  (let ((instance-1 (make-instance 'instance))
	(instance-2 (make-instance 'instance))
	(instance-3 (make-instance 'instance))
	(instance-list nil))
    (push instance-1 instance-list)
    
    (setf (class-name instance-1) 'a)

    (set-attribute-value 'attr1 'a instance-1)
    (set-attribute-value 'attr2 'b instance-1)

    (mapcar #'(lambda(x)
		(lisp-unit:assert-true (member x '(attr1 attr2))))
	    (get-attribute-list instance-1))
    (setf (gethash 'attr1 (attribute-considered-hash instance-1)) t)
    
    (mapcar #'(lambda(x)
;		(print x)
		(lisp-unit:assert-true (member x '(attr2))))
	    (get-attribute-list instance-1))))

(lisp-unit:define-test get-possible-attribute-value-test
  (let ((instance-1 (make-instance 'instance))
	(instance-2 (make-instance 'instance))
	(instance-3 (make-instance 'instance))
	(instance-list nil))
    (push instance-1 instance-list)
    (push instance-2 instance-list)
    (push instance-3 instance-list)
    
    (setf (class-name instance-1) 'a)
    (setf (class-name instance-2) 'b)
    (setf (class-name instance-3) 'a)
    
    (set-attribute-value 'v1 'a instance-1)
    (set-attribute-value 'v2 'b instance-1)
    
    (set-attribute-value 'v1 'b instance-2)
    (set-attribute-value 'v2 'a instance-2)

    (set-attribute-value 'v1 'c instance-3)
    (set-attribute-value 'v2 'c instance-3)
    
    (mapcar #'(lambda(x)
		(lisp-unit:assert-true (member x '(a b c))))
	    (get-possible-attribute-values instance-list 'v1))))
  
(lisp-unit:define-test get-instances-with-specific-instance-values-test
   (let ((instance-1 (make-instance 'instance))
	(instance-2 (make-instance 'instance))
	(instance-3 (make-instance 'instance))
	(instance-list nil))
    (push instance-1 instance-list)
    (push instance-2 instance-list)
    (push instance-3 instance-list)
    
    (setf (class-name instance-1) 'a)
    (setf (class-name instance-2) 'b)
    (setf (class-name instance-3) 'a)
    
    (set-attribute-value 'v1 'a instance-1)
    (set-attribute-value 'v2 'b instance-1)
    
    (set-attribute-value 'v1 'b instance-2)
    (set-attribute-value 'v2 'a instance-2)

    (set-attribute-value 'v1 'c instance-3)
    (set-attribute-value 'v2 'c instance-3)
    
    (mapcar #'(lambda(x)
		(lisp-unit:assert-true (member x (list instance-1))))
	    (get-instances-with-specific-instance-values instance-list 'v1 'a))))


   (let ((instance-1 (make-instance 'instance))
	(instance-2 (make-instance 'instance))
	(instance-3 (make-instance 'instance))
	(instance-list nil))
    (push instance-1 instance-list)
    (push instance-2 instance-list)
    (push instance-3 instance-list)
    
    (setf (class-name instance-1) 'a)
    (setf (class-name instance-2) 'b)
    (setf (class-name instance-3) 'a)
    
    (set-attribute-value 'v1 'a instance-1)
    (set-attribute-value 'v2 'b instance-1)
    
    (set-attribute-value 'v1 'b instance-2)
    (set-attribute-value 'v2 'a instance-2)

    (set-attribute-value 'v1 'a instance-3)
    (set-attribute-value 'v2 'c instance-3)

   (print (select-attribute *instance-list*)))

(lisp-unit:run-tests get-instances-with-specific-instance-values-test)

(select-attribute *instance-list*)
;	  ((outlook Rain) (temp Mild) (humidity High) (wind Strong)))

(defparameter *level-1-node-2* (remove-if #'(lambda(x) ;;normal humidity
					     (equal (gethash 'humidity (attributes x)) 'normal))
					  *instance-list*))
(select-attribute *level-1-node-1*)
(get-possible-attribute-values *instance-list* 'outlook)
(defparameter *level-2-node-1* (remove-if-not #'(lambda(x)  ;; Sunny -End  No
						  (equal (gethash 'outlook (attributes x)) 'sunny))
					      *level-1-node-1*))
(defparameter *level-2-node-2* (remove-if-not #'(lambda(x)  ;; Overcast - Yes
						  (equal (gethash 'outlook (attributes x)) 'overcast))
					      *level-1-node-1*))
(defparameter *level-2-node-3* (remove-if-not #'(lambda(x)  
						  (equal (gethash 'outlook (attributes x)) 'rain))
					      *level-1-node-1*))

(select-attribute *level-2-node-3*)

(mapcar #'(lambda(x)
	    (setf (gethash 'outlook (attribute-considered-hash x)) t))
	*instance-list*)


(print *level-2-node-1*)
(print *level-2-node-2*)
(print *level-2-node-3*)

;(select-attribute *level-2-node-3*)
(mapcar #'(lambda(x)
	    (maphash #'(lambda(k v)
			 (format t "~a ~a ~a ~%" k v (class-name x)))
		     (attributes x))
	    (print "==="))
	*instance-list*)

(select-attribute *level-1-node-2*)
(get-possible-attribute-values *level-1-node-2* 'temp)
(defparameter *level-2-node-4* (remove-if-not #'(lambda(x)  ;; cool 
						  (equal (gethash 'temp (attributes x)) 'cool))
					      *level-1-node-2*))
(defparameter *level-2-node-5* (remove-if-not #'(lambda(x)  ;; mild -End  yes
						  (equal (gethash 'temp (attributes x)) 'mild))
					      *level-1-node-2*))

(defparameter *level-2-node-6* (remove-if-not #'(lambda(x)  ;; hot -End  yes
						  (equal (gethash 'temp (attributes x)) 'hot))
					      *level-1-node-2*))


;(select-attribute *level-2-node-6*)