#|
 This file is a part of com-on
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.com-on)

(defvar *initialized* NIL)

(defun wstring->string (pointer &optional (chars -1))
  (let ((bytes (wide-char-to-multi-byte CP-UTF8 0 pointer chars (cffi:null-pointer) 0 (cffi:null-pointer) (cffi:null-pointer))))
    (cffi:with-foreign-object (string :uchar bytes)
      (wide-char-to-multi-byte CP-UTF8 0 pointer chars string bytes (cffi:null-pointer) (cffi:null-pointer))
      (cffi:foreign-string-to-lisp string :encoding :utf-8))))

(defun string->wstring (string)
  (cffi:with-foreign-string (string string)
    (let* ((chars (multi-byte-to-wide-char CP-UTF8 0 string -1 (cffi:null-pointer) 0))
           (pointer (cffi:foreign-alloc :uint16 :count chars)))
      (multi-byte-to-wide-char CP-UTF8 0 string -1 pointer chars)
      pointer)))

(defun error-message (&optional (errno (get-last-error)))
  (cffi:with-foreign-object (string 'wchar 256)
    (format-message (logior format-message-from-system format-message-ignore-inserts)
                    (cffi:null-pointer) errno 0 string 256 (cffi:null-pointer))
    (wstring->string string)))

(define-condition win32-error (error)
  ((function-name :initarg :function-name :initform NIL :reader function-name)
   (code :initarg :code :reader code)
   (message :initarg :message :initform NIL :reader message))
  (:report (lambda (c s) (format s "The call ~@[to~%  ~a~%~]returned with unexpected result code ~a.~@[~%  ~a~]"
                                 (function-name c) (code c) (message c)))))

(declaim (inline win32-error))
(defun win32-error (code &key function-name message)
  (error 'win32-error :code code :function-name function-name
                      :message (or message
                                   (error-message
                                    (etypecase code
                                      (keyword (cffi:foreign-enum-value 'hresult code))
                                      (integer code))))))

(defmacro check-errno (predicate &body cleanup)
  `(unless ,predicate
     ,@cleanup
     (let ((errno (get-last-error)))
       (win32-error errno))))

(defmacro check-return (value-form &rest expected)
  (let ((value (gensym "VALUE")))
    `(let ((,value ,value-form))
       (if (find ,value ',(or expected '(:ok)))
           ,value
           (win32-error ,value :function-name ',(first value-form))))))

(defun release (pointer)
  (cffi:foreign-funcall-pointer
   (cffi:mem-aref (vtbl pointer) :pointer 2)
   ()
   :pointer pointer
   :unsigned-long))

(defclass guid ()
  ((bytes :initform (make-array 16 :element-type '(unsigned-byte 8)) :reader bytes)))

(defmethod initialize-instance :after ((guid guid) &key id)
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
       (loop with dat = dat
             for i from 0 below 16
             do (setf (aref dat i) (cffi:mem-aref id :uint8 i))))
      (vector
       (replace dat id))
      (null
       (fill dat 0)))))

(defmethod print-object ((guid guid) stream)
  (print-unreadable-object (guid stream :type T)
    (write-sequence (guid-string guid) stream)))

(defun guid-string (guid)
  (with-output-to-string (out)
    (let ((dat (bytes guid)))
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

(defmethod cffi:translate-to-foreign ((guid guid) (type guid-tclass))
  (cffi:translate-into-foreign-memory guid type (cffi:foreign-alloc :uint8 :count 16)))

(defmethod cffi:translate-from-foreign (ptr (type guid-tclass))
  (make-instance 'guid :id ptr))

(defmethod cffi:free-translated-object (ptr (type guid-tclass) param)
  (declare (ignore param))
  (cffi:foreign-free ptr))

(defmethod cffi:translate-into-foreign-memory ((guid guid) (type guid-tclass) ptr)
  (let ((dat (bytes guid)))
    (dotimes (i 16 ptr)
      (setf (cffi:mem-aref ptr :uint8 i) (aref dat i)))))

(defun make-guid (&rest id)
  (make-instance 'guid :id (if (cdr id) id (first id))))

(defmacro define-guid (name &rest id)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defconstant ,name (cond ((boundp ',name) (symbol-value ',name))
                              (T (make-guid ,@id))))))

(defmacro define-comfun ((struct method &rest options) return-type &body args)
  (let* ((*print-case* (readtable-case *readtable*))
         (structg (gensym "STRUCT"))
         (name (intern (format NIL "~a-~a" struct method))))
    `(progn
       (declaim (inline ,name))
       (defun ,name (,structg ,@(mapcar #'first args))
         (cffi:foreign-funcall-pointer
          (,(intern (format NIL "%~a" name))
           (vtbl ,structg))
          ,options
          :pointer ,structg
          ,@(loop for (name type) in args
                  collect type collect name)
          ,return-type)))))

(defmacro define-comstruct (name &body methods)
  (let ((methods (list* `(query-interface (uid :pointer) (out :pointer))
                        `(add-ref :unsigned-long)
                        `(release :unsigned-long)
                        methods)))
    `(progn
       (cffi:defcstruct (,name :conc-name ,(format NIL "%~a-" name))
         ,@(loop for method in methods
                 collect (list (first method) :pointer)))

       ,@(loop for (method return . args) in methods
               ;; Default to hresult return
               do (when (consp return)
                    (push return args)
                    (setf return 'hresult))
               collect `(define-comfun (,name ,method) ,return
                          ,@args)))))

(defun init ()
  (unless *initialized*
    (cffi:load-foreign-library 'ole32)
    (check-return
     (co-initialize (cffi:null-pointer) :multi-threaded))
    (setf *initialized* T)))

(defun shutdown ()
  (when *initialized*
    (co-uninitialize)
    (setf *initialized* NIL)))
