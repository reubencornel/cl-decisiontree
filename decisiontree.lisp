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

(defclass range() ;;This is private only to this package
  ((lower-bound :initarg :lower-bound
		:accessor lower-bound)
   (upper-bound :initarg :upper-bound
		:accessor upper-bound)
   (range-class :initarg :range-class 
		:accessor range-class)))


(defmacro define-instance(instance-class &rest attr-value-pairs)
  `(let ((inst (make-instance 'instance)))
     (setf (class-name inst) ',instance-class)
     (dolist (attr-val-pair ',attr-value-pairs)
       (set-attribute-value (first attr-val-pair)
			    (second attr-val-pair) 
			    inst))
     inst))
			    
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

(defparameter *attribute-selection-function* #'select-attribute)

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
	   ;;otherwise find the best attribute, sort the instance list based on attribute values 
	   (let* ((best-attribute (funcall *attribute-selection-function* list-of-instances))
		  (sorted-instance-list (sort-on-attribute-value list-of-instances best-attribute))) ;;This is a list of lists such that each element in the inner list  has the same value for the specified attribute
	     (setf (attribute-name root-node) best-attribute)                                        
	     ;;generate the rest of the tree
	     (mapcar #'(lambda(x)
			 (setf (gethash (gethash best-attribute (attributes (first x))) (subtree-hash root-node)) ;;Create an entry in the subtree hash for each set of
			       (create-classifier x))) ;; elements in the sorted element list and associate a child node with that.
		     sorted-instance-list)))) 
    root-node))

(defun keys(hash-table)
  "Get a list of keys in a given table"
  (let ((keys-list nil))
    (maphash #'(lambda(k v)
		 (declare (ignore v))
		 (push k keys-list))
	     hash-table)
    (reverse keys-list)))

(defun classify(tree-root-node instance)
  (if (null tree-root-node)
      (error "Not trained for this instance"))
  (cond ((= (length (keys (subtree-hash tree-root-node))) 0) (label  tree-root-node))
	(t (classify (gethash (gethash (attribute-name tree-root-node) ;;Get the value of the attribute associated with the current tree node
				       (attributes instance))          ;; from the instance
			      (subtree-hash tree-root-node)) instance)))) ;;Use that value to get to the next node


;;;;;;;;;;;; BEGIN OF CONTINUOUS VALUED ATTRIBUTES ;;;;;;;;;;;;;;;;;;
(defun sort-on-continuous-valued-attribute(list-of-instances attribute-name &optional (predicate #'<))
  "Sort elements of a list on based on the value of the attribute"
  (sort list-of-instances predicate :key (lambda(x)
					    (gethash attribute-name (attributes x)))))


(defun generate-list-of-ranges(sorted-instance-list attribute current-class lower-bound )
  (cond ((null sorted-instance-list) (error "Empty Sorted instance list")) ;; we always expect something to be present in the sorted instance list
	((and (= (length sorted-instance-list) 1)
	      (not (equal current-class (class-name (first sorted-instance-list))))) 
	 ;; In this case we are at the last element and the it is not the same class as the current class
	 ;; So we create two new range objects 
	 ;; One for the old range and one for the last element.
	 (list (make-instance 'range 
			      :lower-bound lower-bound
			      :upper-bound (gethash attribute 
						    (attributes (first sorted-instance-list)))
			      :range-class current-class)
	       (make-instance 'range 
			      :lower-bound (gethash attribute 
						    (attributes (first sorted-instance-list))) 
			      :upper-bound (gethash attribute 
						    (attributes (first sorted-instance-list)))
			      :range-class (class-name (first sorted-instance-list)))))
	((= (length sorted-instance-list) 1) ;;Otherwise
	 (list (make-instance 'range
			      :lower-bound lower-bound
			      :upper-bound (gethash attribute 
						    (attributes (first sorted-instance-list)))
			      :range-class current-class)))
	((not (equal current-class (class-name (first sorted-instance-list))))
	 ;;The previous class and the current class do not match
	 ;;so, this is another range...create a range object and call 
	 ;;generate-list-of-ranges recursively
	 (cons (make-instance 'range
			      :lower-bound lower-bound 
			      :upper-bound (gethash attribute (attributes (first sorted-instance-list)))
			      :range-class current-class)
	       (generate-list-of-ranges (cdr sorted-instance-list)
					attribute
					(class-name (first sorted-instance-list))
					(gethash attribute (attributes (first sorted-instance-list))))))
	(t
	 (generate-list-of-ranges (cdr sorted-instance-list)
				  attribute
				  current-class
				  lower-bound))))
			      
(defun generate-class-symbol-alist(range-list)
  (let ((class-list (remove-duplicates (mapcar #'(lambda(x)
						   (range-class x))
					       range-list) :test #'equal)))
    (mapcar #'(lambda(x)
		(list x (gensym)))
	    class-list)))
	
				 
;; What we will do here is a but ugly..but seems reasonable given what we have to do
;; We will generate a list of range objects for ranges of different classes from the 
;; training instances
;; Once we get that...we will allocate a symbol for each unique class
;; The drawback of this approach is that if we have a class that is missing in the 
;; training data we will not be able to handle it.
(defun generate-range-symbols(instance-list attribute)
  (let* ((sorted-list (sort-on-continuous-valued-attribute instance-list attribute))
	 (range-list (generate-list-of-ranges sorted-list attribute
					      (class-name (first sorted-list)) 
					      (gethash attribute (attributes (first sorted-list)))))
	 (class-symbol-alist (generate-class-symbol-alist range-list)))
    
    (mapcar #'(lambda(x)
		(setf (gethash attribute (attributes x))
		      (second (assoc (class-name x) class-symbol-alist))))
	    instance-list)
    (mapcar #'(lambda(x)
		(let ((classname (range-class x)))
		  (setf (range-class x)
			(second (assoc classname class-symbol-alist)))))
	    range-list)
    range-list))
    
