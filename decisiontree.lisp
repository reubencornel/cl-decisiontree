(in-package #:decisiontree)

(defclass node()
  ((subtree-hash :initform (make-hash-table :test #'equal)
		 :accessor subtree-hash)
   (instance-list :initform nil
		  :accessor instance-list)
   (label :initform nil
	  :accessor label)
   (attribute-name :initform nil
		   :accessor attribute-name)
   (attribute-selection-function :initform #'select-attribute
				 :accessor attribute-selection-function)))

(defclass instance()
  ((attribute-value-hash :initform (make-hash-table :test #'equal)
			 :accessor attributes)
   (attribute-considered-hash :initform (make-hash-table :test #'equal)
			      :accessor attribute-considered-hash)
   (class-name :initform nil
	       :accessor class-name)))


(defmethod set-attribute-value(attribute value inst)
  (error "Not Implemented"))

(defmethod set-attribute-value(attribute value (inst instance))
  (setf (gethash attribute (attributes inst)) value)
  (setf (gethash attribute (attribute-considered-hash inst)) nil))

(defmethod set-attribute-values (inst &rest attribute-value-lists)
  (error "Not implemented"))


(defmethod set-attribute-values((inst instance) &rest attribute-value-lists)
  (mapcar #'(lambda(x)
	      (set-attribute-value (first x) (second x) inst))
	  attribute-value-lists))

(defmethod populate-attr-hash(instance)
  (error "Not defined"))

(defmethod populate-attr-hash((instance instance))
  (let ((hash (make-hash-table :test #'equal)))
    (maphash #'(lambda(k v)
		 (declare (ignore v))
		 (setf (gethash k hash) 0))
	    (attributes instance))
    hash))

(defun get-class-list(list-of-instances)
  (remove-duplicates (mapcar #'(lambda(x)
				 (class-name x))
			     list-of-instances)))

(defun log-function(p-i)
  (if (= p-i 0)
      0
      (if (= 0.0 (* p-i (log p-i 2)))
	  0.0
	  (* -1 (* p-i (log p-i 2))))))

(defun insert-element-in-list (sorted-list element test)
  (cond ((null sorted-list) (list (list element))) ;; we have run to the end of the list and we have not found the element
	;;so create a new list
	((funcall test element (first sorted-list)) (append (list (cons element (first sorted-list)))
							    (cdr sorted-list)))
	(t (cons (car sorted-list) (insert-element-in-list (cdr sorted-list) element test)))))

(defun sort-instances-helper(list-of-instances &optional (sorted-list nil) (sort-test #'member))
  (cond ((null list-of-instances) sorted-list)
	(t (sort-instances-helper (cdr list-of-instances)
				  (insert-element-in-list sorted-list (car list-of-instances) sort-test)
				  sort-test))))

(defun sort-on-attribute-value (list-of-instances attribute)
  (sort-instances-helper list-of-instances nil #'(lambda(element list)
						   (equal (gethash attribute (attributes element))
							  (gethash attribute (attributes (first list)))))))

(defun sort-instances(list-of-instances)
  (sort-instances-helper (mapcar #'(lambda(x)
				     (class-name x))
				 list-of-instances)))

(defmacro spy(form)
  "Is takes a form evaluates it and prints it out in the format 
   form => output. The value of the evaluated form is returned"
  (let ((return-val (gensym)))
    `(let ((,return-val (multiple-value-list  ,form)))
       (format t "~s => ~{~s ~}~%" ',form ,return-val)
       (values-list ,return-val))))


(defun sum-up-individual-sets(sort-class-list number-of-instances)
  (cond ((null sort-class-list) 0)
	(t (+  (log-function (/ (length (car sort-class-list))
			        number-of-instances))
	       (sum-up-individual-sets (cdr sort-class-list)
				      number-of-instances)))))

(defun calculate-entropy(list-of-instances)
  (sum-up-individual-sets  (sort-instances list-of-instances) (length list-of-instances)))

(defmethod get-attribute-list(inst)
  (error "Not implemented"))

(defmethod get-attribute-list((inst instance))
  (let ((attr-list nil))
    (maphash #'(lambda(k v)
		 (declare (ignore v))
		 (if (not (gethash k (attribute-considered-hash inst)))
		     (push k attr-list)))
	     (attributes inst))
    attr-list))

(defun get-possible-attribute-values(list-of-instances attr)
  (remove-duplicates (mapcar #'(lambda(x)
				 (gethash attr (attributes x)))
			     list-of-instances)))

(defun get-instances-with-specific-instance-values(list-of-instances attr value)
  (remove-if-not #'(lambda(x)
		     (equal value (gethash attr (attributes x))))
		 list-of-instances))

(defun sum-of-attribute-entropies(list-of-instances attr)
  (let ((list-of-attribute-values (get-possible-attribute-values list-of-instances attr)))
    (reduce #'+ 
	    (mapcar #'(lambda(x)
			(*
			 (/ (length (get-instances-with-specific-instance-values list-of-instances attr x))
			    (length list-of-instances))
			 (calculate-entropy (get-instances-with-specific-instance-values list-of-instances attr x))))
		    list-of-attribute-values))))


(defun get-attribute-info-gain-list(list-of-instances)
  "returns a list of list each element is of the type (attribute info-gain)"
  (let ((overall-entropy (calculate-entropy list-of-instances))
	(attribute-list (get-attribute-list (first list-of-instances))))
    (mapcar #'(lambda(attr)
		(list attr
		      (- overall-entropy
			 (sum-of-attribute-entropies list-of-instances attr))))
	    attribute-list)))

(defun select-attribute(list-of-instances)
  (let ((attribute-info-gain-list (get-attribute-info-gain-list list-of-instances)))
    (first (first (sort attribute-info-gain-list #'> :key #'second)))))


(defun all-instances-of-same-class(list-of-instances)
  (< (length (sort-instances list-of-instances)) 2))


(defun create-classifier(list-of-instances)
  (let ((root-node (make-instance 'node)))
    ;;If all examples belong to one class then label the node to be of the same
    ;;class and return it
    (cond ((all-instances-of-same-class list-of-instances)
	   (setf (label root-node) (class-name (first list-of-instances)))
	   root-node)
	  (t  
	   (let* ((best-attribute (funcall (attribute-selection-function root-node) list-of-instances))
		  (sorted-instance-list (sort-on-attribute-value list-of-instances best-attribute)))
	     (setf (attribute-name root-node) best-attribute)
	     (mapcar #'(lambda(x)
			 (setf (gethash (gethash best-attribute (attributes (first x))) (subtree-hash root-node))
			       (create-classifier x)))
		     sorted-instance-list))))
    root-node))