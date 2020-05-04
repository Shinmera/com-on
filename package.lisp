#|
 This file is a part of com-on
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.com-on.cffi
  (:use #:cl)
  (:shadow #:byte)
  (:export
   #:ole32
   #:cp-utf8
   #:clsctx-all
   #:format-message-from-system
   #:format-message-ignore-inserts
   #:dword
   #:word
   #:long
   #:short
   #:byte
   #:wchar
   #:uint-ptr
   #:init
   #:hresult
   #:com
   #:vtbl
   #:guid
   #:guid-data1
   #:guid-data2
   #:guid-data3
   #:guid-data4
   #:initialize
   #:uninitialize
   #:create-instance
   #:wide-char-to-multi-byte
   #:multi-byte-to-wide-char
   #:get-last-error
   #:format-message))

(defpackage #:org.shirakumo.com-on
  (:use #:cl)
  (:local-nicknames
   (#:com #:org.shirakumo.com-on.cffi))
  ;; com.lisp
  (:export
   #:create
   #:release
   #:define-comfun
   #:define-comstruct
   #:init
   #:shutdown)
  ;; error.lisp
  (:export
   #:wstring->string
   #:string->wstring
   #:error-message
   #:win32-error
   #:function-name
   #:code
   #:message
   #:check-last-error
   #:check-hresult
   #:with-deref)
  ;; guid.lisp
  (:export
   #:guid
   #:bytes
   #:guid-string
   #:make-guid
   #:define-guid))
