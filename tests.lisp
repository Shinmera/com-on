#|
 This file is a part of com-on
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:com-on-test
  (:nicknames #:org.shirakumo.com-on.test)
  (:use #:cl #:org.shirakumo.parachute)
  (:local-nicknames
   (#:com #:org.shirakumo.com-on)))
(in-package #:org.shirakumo.com-on.test)

(define-test com-on)

(cffi:defcallback guid-pointer :void ((guid :pointer))
  (cffi:pointer-address guid))

(cffi:defcallback guid-string :string ((guid com:guid))
  (com:guid-string guid))

(cffi:defcstruct (struct :conc-name struct-)
  (before :int)
  (guid com:guid)
  (after :int))

(defun random-guid ()
  (com:guid (loop repeat 16 collect (random 255))))

(define-test guid
    :parent com-on
  (of-type com:guid (com:guid))
  (is com:guid= (com:guid) (com:guid))
  (finish (com:guid "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF"))
  (is com:guid= (com:guid "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF") (com:guid "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF"))
  (is string= "00000000-0000-0000-0000-000000000000" (com:guid-string (com:guid)))
  (is string= "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF" (com:guid-string (com:guid "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF")))
  (is com:guid= (com:guid "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF")
      (com:guid #(#xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF
                  #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF)))
  (group (cffi:mem-ref "Write and read GUIDs from memory")
    (cffi:with-foreign-object (ptr 'com:guid)
      (let ((guid (random-guid)))
        (finish (setf (cffi:mem-ref ptr 'com:guid) guid))
        (is com:guid= guid (cffi:mem-ref ptr 'com:guid)))))
  (group (cffi:mem-aref "Retrieve GUIDs from a packed array")
    (cffi:with-foreign-object (ptr 'com:guid 2)
      (let ((a (random-guid))
            (b (random-guid)))
        (finish (setf (cffi:mem-aref ptr 'com:guid 0) a))
        (finish (setf (cffi:mem-aref ptr 'com:guid 1) b))
        (is com:guid= a (cffi:mem-aref ptr 'com:guid 0))
        (is com:guid= b (cffi:mem-aref ptr 'com:guid 1)))))
  (group (cffi:foreign-funcall "Call GUID functions by translating to pointer")
    (let ((guid (random-guid)))
      (finish (cffi:foreign-funcall-pointer (cffi:callback guid-pointer) ()
                                            com:guid guid
                                            :void))
      (is string= (com:guid-string guid)
          (cffi:foreign-funcall-pointer (cffi:callback guid-string) ()
                                        com:guid guid
                                        :string))))
  (group (cffi:foreign-slot-value "Access a packed GUID in a struct")
    (cffi:with-foreign-object (struct '(:struct struct))
      (let ((guid (random-guid)))
        (finish (setf (struct-before struct) 1))
        (finish (setf (struct-guid struct) guid))
        (finish (setf (struct-after struct) 2))
        (is = 1 (struct-before struct))
        (is com:guid= guid (struct-guid struct))
        (is = 2 (struct-after struct))))))
