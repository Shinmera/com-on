(asdf:load-system :staple-markless)

(defpackage "com-on-docs"
  (:use #:cl)
  (:local-nicknames
   (#:com #:org.shirakumo.com-on)))

(defclass page* (staple:simple-page)
  ()
  (:default-initargs :document-package (find-package "com-on-docs")))

(defmethod staple:page-type ((system (eql (asdf:find-system :com-on))))
  'page*)

(defmethod staple:packages ((system (eql (asdf:find-system :com-on))))
  (list (find-package (string '#:org.shirakumo.com-on))))

#+sbcl
(defmethod staple:definition-wanted-p ((definition definitions:source-transform) (page page*)) NIL)
