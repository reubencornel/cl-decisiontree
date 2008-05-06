(defpackage #:decisiontree
  (:use #:asdf #:common-lisp))

(in-package #:decisiontree)

(defsystem decisiontree
    :name "decisiontree"
    :depends-on (:lisp-unit)
    :components ((:file "decisiontree")
		 (:file "decisiontreeTests" :depends-on ("decisiontree"))))

(defpackage #:decisiontree
  (:use #:asdf #:common-lisp)
  (:export #:create-classifier
           #:classify
	   #:node
           #:instance
           #:range
           #:subtree-hash
           #:instance-list
           #:label
           #:attribute-name

           #:attributes
           #:class-name

           #:lower-bound
           #:upper-bound
           #:range-class
           #:define-instance))
