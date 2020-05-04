#|
 This file is a part of com-on
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.com-on)

(defun wstring->string (pointer &optional (chars -1))
  (let ((bytes (com:wide-char-to-multi-byte com:CP-UTF8 0 pointer chars (cffi:null-pointer) 0 (cffi:null-pointer) (cffi:null-pointer))))
    (cffi:with-foreign-object (string :uchar bytes)
      (com:wide-char-to-multi-byte com:CP-UTF8 0 pointer chars string bytes (cffi:null-pointer) (cffi:null-pointer))
      (cffi:foreign-string-to-lisp string :encoding :utf-8))))

(defun string->wstring (string)
  (cffi:with-foreign-string (string string)
    (let* ((chars (com:multi-byte-to-wide-char com:CP-UTF8 0 string -1 (cffi:null-pointer) 0))
           (pointer (cffi:foreign-alloc :uint16 :count chars)))
      (com:multi-byte-to-wide-char com:CP-UTF8 0 string -1 pointer chars)
      pointer)))

(defun error-message (&optional (errno (get-last-error)))
  (cffi:with-foreign-object (string 'wchar 256)
    (com:format-message (logior com:FORMAT-MESSAGE-FROM-SYSTEM com:FORMAT-MESSAGE-IGNORE-INSERTS)
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
                                      (keyword (cffi:foreign-enum-value 'com:hresult code))
                                      (integer code))))))

(defmacro check-last-error (predicate &body cleanup)
  `(unless ,predicate
     ,@cleanup
     (let ((errno (com:get-last-error)))
       (win32-error errno))))

(defmacro check-hresult (value-form &rest expected)
  (let ((value (gensym "VALUE")))
    `(let ((,value ,value-form))
       (if (find ,value ',(or expected '(:ok)))
           ,value
           (win32-error ,value :function-name ',(first value-form))))))

(defmacro with-deref ((var type) &body init)
  `(cffi:with-foreign-object (,var ,type)
     (check-hresult ,@init)
     (cffi:mem-ref ,var ,type)))
