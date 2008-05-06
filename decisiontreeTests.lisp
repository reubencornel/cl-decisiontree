(in-package :decisiontree)


(defparameter *instance-list* (loop for i from 1 to 14 collect (make-instance 'instance)))


;;Below is a very ugly method of initializing the instance list. A better way would be to 
;; Use "define-instance" and push the result into a list.
;; Eg: (push (define-instance <Class> <attr-value pair>) <list>)
(mapcar #'(lambda(x y)
	    (mapcar #'(lambda(z)
			(set-attribute-value (first z) (second z) y))
		    x))
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
	'(No No Yes Yes Yes No Yes No Yes Yes Yes Yes Yes No))


;; End of ugly initialization

(defparameter *root-node* (create-classifier *instance-list*))

(classify *root-node* ;;Root of the tree
	  (define-instance ? (outlook Sunny) (temp Mild) (humidity High) (wind Strong)))


;;; Start of unit tests
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


(lisp-unit:define-test all-instances-of-same-class-test
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
    
    (lisp-unit:assert-false (all-instances-of-same-class instance-list))
    (setf (class-name instance-2) 'a)
    (lisp-unit:assert-true (all-instances-of-same-class instance-list))))

(lisp-unit:define-test sort-attr-value-test
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
    
    (print (sort-on-attribute-value instance-list 'v1))))

(lisp-unit:define-test sort-on-attr-val-test
  (let ((instance-list nil))
       
    (push (define-instance warm (temp 30)) instance-list)
    (push (define-instance cold (temp 10)) instance-list)
    (push (define-instance warm (temp 25)) instance-list)
    
    (lisp-unit:assert-equal '(10 25 30) (mapcar #'(lambda(x)
						    (gethash 'temp (attributes x)))
						(sort-on-continuous-valued-attribute instance-list 'temp)))))



(lisp-unit:define-test generate-range-symbols-test
  (let ((instance-list nil))
       
    (push (define-instance warm (temp 30)) instance-list)
    (push (define-instance cold (temp 10)) instance-list)
    (push (define-instance warm (temp 25)) instance-list)
    (push (define-instance hot (temp 75)) instance-list)
    
    (format t "List Length: ~a~%" (length instance-list))
    (setf instance-list (sort-on-continuous-valued-attribute instance-list 'temp))
    ;;range symbol hash is a mapping between the classes and the range that was originally present
    (defparameter range-symbol-hash (generate-range-symbols instance-list 'temp))
    (maphash #'(lambda(k v)
		 (format t "~a ~a ~%" k v))
	     range-symbol-hash)

    (mapcar #'(lambda(x)
		(format t "~a ~%" (class-name x))
		(maphash #'(lambda(k v)
			   
			     (format t "~a ~a ~%" k v))
			 (attributes x)))
	    instance-list)
    (maphash #'(lambda(k v)
		 (format t "~a ~a ~%" k v))
	     range-symbol-hash)))

(lisp-unit:run-tests generate-range-symbols-test)

