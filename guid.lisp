#|
 This file is a part of com-on
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.com-on)

;;;; Note:
;; We define the actual type here to be a pointer in order to allow
;; defining arguments and return values of functions to work properly
;; without complaining about libffi, as usually GUIDs are in fact
;; passed by pointer and not by value (thank god).
;;
;; However, doing so means that when we want to allocate a GUID
;; object, we would be allocating only enough space for a pointer,
;; which is not going to be enough on any current platform. So we
;; have to override FOREIGN-TYPE-SIZE as well to fake being a
;; structure.
(cffi:define-foreign-type guid ()
  ((bytes :initform (make-array 16 :element-type '(unsigned-byte 8)) :initarg :bytes :reader bytes))
  (:actual-type :pointer))

(cffi:define-parse-method guid ()
  (make-instance 'guid :bytes NIL))

(defmethod cffi:foreign-type-size ((type guid))
  (cffi:foreign-type-size '(:struct com:guid)))

(defmethod cffi:foreign-type-alignment ((type guid))
  (cffi:foreign-type-alignment '(:struct com:guid)))

(defmethod cffi::aggregatep ((type guid)) T)

(defmethod cffi::translate-aggregate-to-foreign (ptr (guid guid) (type guid))
  (cffi:translate-into-foreign-memory guid type ptr))

(defmethod cffi::translate-aggregate-to-foreign (ptr guid-ish (type guid))
  (cffi:translate-into-foreign-memory guid-ish type ptr))

(defmethod cffi:translate-to-foreign ((guid guid) (type guid))
  (cffi:translate-into-foreign-memory guid type (cffi:foreign-alloc :uint8 :count 16)))

(defmethod cffi:translate-to-foreign (guid-ish (type guid))
  (typecase guid-ish
    (cffi:foreign-pointer guid-ish)
    (T (cffi:translate-to-foreign (guid guid-ish) type))))

(defmethod cffi:translate-from-foreign (ptr (type guid))
  (make-instance 'guid :id ptr))

(defmethod cffi:free-translated-object (ptr (type guid) param)
  (declare (ignore param))
  (cffi:foreign-free ptr))

(defmethod cffi:translate-into-foreign-memory ((guid guid) (type guid) ptr)
  (let ((dat (bytes guid)))
    (dotimes (i 16 ptr)
      (setf (cffi:mem-aref ptr :uint8 i) (aref dat i)))))

(defmethod cffi:translate-into-foreign-memory (guid-ish (type guid) ptr)
  (typecase guid-ish
    (guid
     (let ((dat (bytes guid-ish)))
       (dotimes (i 16 ptr)
         (setf (cffi:mem-aref ptr :uint8 i) (aref dat i)))))
    (T
     (cffi:translate-into-foreign-memory (guid guid-ish) type ptr))))

(defmethod cffi:expand-from-foreign (ptr (type guid))
  `(make-instance 'guid :id ,ptr))

(defmethod cffi:expand-into-foreign-memory (val (type guid) ptr)
  (let ((dat (gensym "DAT"))
        (i (gensym "I"))
        (write-guid (gensym "WRITE-GUID")))
    `(flet ((,write-guid (,dat)
              (let ((,dat (bytes ,dat)))
                (dotimes (,i 16)
                  (setf (cffi:mem-aref ,ptr :uint8 ,i) (aref ,dat ,i))))))
       (let ((,dat ,val))
         (etypecase ,dat
           (guid (,write-guid ,dat))
           (T (,write-guid (guid ,dat))))))))

(defmethod cffi:expand-to-foreign-dyn (val var body (type guid))
  (let ((dat (gensym "DAT"))
        (i (gensym "I"))
        (thunk (gensym "THUNK")))
    `(flet ((,thunk (,var)
              ,@body))
       (let ((,dat ,val))
         (etypecase ,dat
           (guid
            (cffi:with-foreign-object (,var 'guid)
              (let ((,dat (bytes ,dat)))
                (dotimes (,i 16)
                  (setf (cffi:mem-aref ,var :uint8 ,i) (aref ,dat ,i))))
              (,thunk ,var)))
           (cffi:foreign-pointer
            (,thunk ,dat))
           (T
            (cffi:with-foreign-object (,var 'guid)
              (let ((,dat (bytes (guid ,dat))))
                (dotimes (,i 16)
                  (setf (cffi:mem-aref ,var :uint8 ,i) (aref ,dat ,i))))
              (,thunk ,var))))))))

(defmethod initialize-instance :after ((guid guid) &key (id NIL id-p))
  (when id-p
    (let ((dat (bytes guid)))
      (etypecase id
        (string
         (let ((i 0))
           (flet ((read-bytes (start end mode)
                    (let ((int (parse-integer id :start start :end end :radix 16))
                          (cnt (/ (- end start) 2)))
                      (loop for j from 0 below cnt
                            for byte = (ecase mode
                                         (:lsb (ldb (byte 8 (* 8 j)) int))
                                         (:msb (ldb (byte 8 (* 8 (- cnt j 1))) int)))
                            do (setf (aref dat i) byte)
                               (incf i)))))
             (read-bytes 0 8 :lsb)
             (read-bytes 9 13 :lsb)
             (read-bytes 14 18 :lsb)
             (read-bytes 19 23 :msb)
             (read-bytes 24 36 :msb))))
        (cons
         (ecase (length id)
           (16 (replace dat id))
           (11 (destructuring-bind (d1 d2 d3 &rest d4) id
                 (setf (aref dat 0) (ldb (byte 8 0) d1))
                 (setf (aref dat 1) (ldb (byte 8 8) d1))
                 (setf (aref dat 2) (ldb (byte 8 16) d1))
                 (setf (aref dat 3) (ldb (byte 8 24) d1))
                 (setf (aref dat 4) (ldb (byte 8 0) d2))
                 (setf (aref dat 5) (ldb (byte 8 8) d2))
                 (setf (aref dat 6) (ldb (byte 8 0) d3))
                 (setf (aref dat 7) (ldb (byte 8 8) d3))
                 (loop for i from 8 below 16
                       for byte in d4
                       do (setf (aref dat i) byte))))))
        (cffi:foreign-pointer
         (if (cffi:null-pointer-p id)
             (fill dat 0)
             (dotimes (i 16)
               (setf (aref dat i) (cffi:mem-aref id :uint8 i)))))
        (vector
         (replace dat id))
        (null
         (fill dat 0))))))

(defmethod print-object ((guid guid) stream)
  (if (bytes guid)
      (format stream "(~s ~s)" 'guid (guid-string guid))
      (call-next-method)))

(defmethod make-load-form ((guid guid) &optional env)
  (declare (ignore env))
  `(guid ,(bytes guid)))

(defun guid-string (guid-ish)
  (typecase guid-ish
    (guid
     (with-output-to-string (out)
       (let ((dat (bytes guid-ish)))
         (flet ((print-bytes (start end mode)
                  (ecase mode
                    (:lsb (loop for i downfrom (1- end) to start
                                do (format out "~2,'0x" (aref dat i))))
                    (:msb (loop for i from start below end
                                do (format out "~2,'0x" (aref dat i)))))))
           (print-bytes 0 4 :lsb)
           (write-char #\- out)
           (print-bytes 4 6 :lsb)
           (write-char #\- out)
           (print-bytes 6 8 :lsb)
           (write-char #\- out)
           (print-bytes 8 10 :msb)
           (write-char #\- out)
           (print-bytes 10 16 :msb)))))
    (T (guid-string (guid guid-ish)))))

(defun guid= (a b)
  (loop for ab across (bytes a)
        for bb across (bytes b)
        always (= ab bb)))

(defun guid (&rest id)
  (make-instance 'guid :id (if (cdr id) id (first id))))

(defmacro define-guid (name &rest id)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defconstant ,name (cond ((boundp ',name) (symbol-value ',name))
                              (T (guid ,@id))))))
