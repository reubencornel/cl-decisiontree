(defpackage #:decisiontree
  (:use #:asdf #:common-lisp))

(in-package #:decisiontree)

(defsystem decisiontree
    :name "decisiontree"
    :depends-on (:lisp-unit)
    :components ((:file "decisiontree")
		 (:file "decisiontreeTests" :depends-on ("decisiontree"))))
