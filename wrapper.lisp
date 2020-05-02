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

(define-condition win32-error (gamepad:gamepad-error)
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

(defun make-guid (d1 d2 d3 &rest d4)
  (let ((ptr (cffi:foreign-alloc '(:struct guid))))
    (setf (guid-data1 ptr) d1)
    (setf (guid-data2 ptr) d2)
    (setf (guid-data3 ptr) d3)
    (loop for i from 0 below 8
          for d in d4
          do (setf (cffi:mem-aref (cffi:foreign-slot-pointer ptr '(:struct guid) 'data4) :uint8 i)
                   d))
    ptr))

(defun guid-string (guid)
  (let ((data4 (cffi:foreign-slot-pointer guid '(:struct guid) 'data4)))
    (with-output-to-string (out)
      (format out "~8,'0x-~4,'0x-~4,'0x-~2,'0x~2,'0x-"
              (guid-data1 guid)
              (guid-data2 guid)
              (guid-data3 guid)
              (cffi:mem-aref data4 :uint8 0)
              (cffi:mem-aref data4 :uint8 1))
      (dotimes (i 6)
        (format out "~2,'0x" (cffi:mem-aref data4 :uint8 (+ 2 i)))))))

(defun guid-integer (guid)
  (let ((integer 0)
        (data4 (cffi:foreign-slot-pointer guid '(:struct guid) 'data4)))
    (declare (optimize speed))
    (declare (type (unsigned-byte 128) integer))
    (setf (ldb (cl:byte 32 96) integer) (guid-data1 guid))
    (setf (ldb (cl:byte 16 80) integer) (guid-data2 guid))
    (setf (ldb (cl:byte 16 64) integer) (guid-data3 guid))
    (dotimes (i 8)
      (setf (ldb (cl:byte 8 (- 56 (* i 8))) integer) (cffi:mem-aref data4 :uint8 i)))
    integer))

(defun integer-guid (integer)
  (make-guid (ldb (cl:byte 32 96) integer)
             (ldb (cl:byte 16 80) integer)
             (ldb (cl:byte 16 64) integer)
             (ldb (cl:byte 8 56) integer)
             (ldb (cl:byte 8 48) integer)
             (ldb (cl:byte 8 40) integer)
             (ldb (cl:byte 8 32) integer)
             (ldb (cl:byte 8 24) integer)
             (ldb (cl:byte 8 16) integer)
             (ldb (cl:byte 8 8) integer)
             (ldb (cl:byte 8 0) integer)))

(defmacro define-guid (name &rest guid)
  ;; Lazy evaluation on first access to avoid problems with foreign memory
  ;; allocation during/after dump to image.
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let (value)
       (defun ,name ()
         (or value (setf value (make-guid ,@guid))))
       (define-symbol-macro ,name (,name)))))

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
