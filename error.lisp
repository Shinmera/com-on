(in-package #:org.shirakumo.com-on)

(cffi:define-foreign-type wstring ()
  ()
  (:actual-type :pointer))

(cffi:define-parse-method wstring ()
  (make-instance 'wstring))

(defmethod cffi:translate-to-foreign ((string string) (type wstring))
  (values (string->wstring string) T))

(defmethod cffi:translate-to-foreign (obj (type wstring))
  (if (cffi:pointerp obj)
      (values obj NIL)
      (error "~a is neither a string nor pointer." obj)))

(defmethod cffi:translate-from-foreign (ptr (type wstring))
  (unless (cffi:null-pointer-p ptr)
    (wstring->string ptr)))

(defmethod cffi:free-translated-object (ptr (type wstring) free-p)
  (when free-p
    (cffi:foreign-free ptr)))

(defun wstring->string (pointer &optional (chars -1))
  (let ((bytes (com:wide-char-to-multi-byte com:CP-UTF8 0 pointer chars (cffi:null-pointer) 0 (cffi:null-pointer) (cffi:null-pointer))))
    (cffi:with-foreign-object (string :uchar bytes)
      (com:wide-char-to-multi-byte com:CP-UTF8 0 pointer chars string bytes (cffi:null-pointer) (cffi:null-pointer))
      (let ((babel::*suppress-character-coding-errors* T))
        (cffi:foreign-string-to-lisp string :encoding :utf-8)))))

(defun string->wstring (string &optional buffer)
  (cffi:with-foreign-string (string string)
    (let* ((chars (com:multi-byte-to-wide-char com:CP-UTF8 0 string -1 (cffi:null-pointer) 0))
           (pointer (or buffer (cffi:foreign-alloc :uint16 :count chars))))
      (com:multi-byte-to-wide-char com:CP-UTF8 0 string -1 pointer chars)
      (values pointer (* 2 chars)))))

(defun wstring-length (string)
  (cffi:with-foreign-string (string string)
    (com:multi-byte-to-wide-char com:CP-UTF8 0 string -1 (cffi:null-pointer) 0)))

(defmacro with-wstring ((var string) &body body)
  `(let ((,var (string->wstring ,string)))
     (unwind-protect
          (let ((,var ,var))
            ,@body)
       (cffi:foreign-free ,var))))

(defun error-message (&optional (errno (com:get-last-error)))
  (let ((errno (etypecase errno
                 (integer errno)
                 (symbol (cffi:foreign-enum-value 'com:hresult errno)))))
    (cffi:with-foreign-object (string 'com:wchar 256)
      (com:format-message (logior com:FORMAT-MESSAGE-FROM-SYSTEM com:FORMAT-MESSAGE-IGNORE-INSERTS)
                          (cffi:null-pointer) errno 0 string 256 (cffi:null-pointer))
      (wstring->string string))))

(define-condition win32-error (error)
  ((function-name :initarg :function-name :initform NIL :reader function-name)
   (code :initarg :code :reader code)
   (message :initarg :message :initform NIL :reader message))
  (:report (lambda (c s) (format s "The call ~@[to~%  ~a~%~]returned with unexpected result code ~a.~@[~%  ~a~]"
                                 (function-name c) (code c) (message c)))))

(declaim (inline win32-error))
(defun win32-error (code &key function-name message (type 'win32-error))
  (let ((code (if (eql T code) (com:get-last-error) code)))
    (error type :code code
                :function-name function-name
                :message (or message
                             (error-message
                              (etypecase code
                                (keyword (cffi:foreign-enum-value 'com:hresult code))
                                (integer code)))))))

(defmacro check-last-error (predicate &body cleanup)
  `(unless ,predicate
     ,@cleanup
     (let ((errno (com:get-last-error)))
       (win32-error errno))))

(defmacro check-hresult (value-form &rest expected)
  (let ((value (gensym "VALUE")))
    `(let ((,value ,value-form))
       (if (find ,value ',(or expected '(:ok :false)))
           ,value
           (win32-error ,value :function-name ',(first value-form))))))

(defmacro with-deref ((var type) &body init)
  `(cffi:with-foreign-object (,var ,type)
     (check-hresult ,@init)
     (cffi:mem-ref ,var ,type)))

(defun add-hresult (&rest pairs)
  (let ((type (cffi::parse-type 'com:hresult)))
    (loop for (key val) on pairs by #'cddr
          do (when (gethash key (cffi::keyword-values type))
               (assert (= val (gethash key (cffi::keyword-values type)))))
             (setf (gethash key (cffi::keyword-values type)) val)
             (setf (gethash val (cffi::value-keywords type)) key))))

(defmacro define-hresult (&body pairs)
  `(add-hresult ,@(loop for (key val) in pairs
                        collect key
                        collect val)))

(defmacro check-win32-error-code (value-form &rest expected)
  (let ((value (gensym "VALUE")))
    `(let ((,value ,value-form))
       (if (find ,value ',(or expected '(:success)))
           ,value
           (win32-error ,value :function-name ',(first value-form))))))

(defun add-win32-error-code (&rest pairs)
  (let ((type (cffi::parse-type 'com:win32-error-code)))
    (loop for (key val) on pairs by #'cddr
          do (when (gethash key (cffi::keyword-values type))
               (assert (= val (gethash key (cffi::keyword-values type)))))
             (setf (gethash key (cffi::keyword-values type)) val)
             (setf (gethash val (cffi::value-keywords type)) key))))

(defmacro define-win32-error-code (&body pairs)
  `(add-win32-error-code ,@(loop for (key val) in pairs
                                 collect key
                                 collect val)))
