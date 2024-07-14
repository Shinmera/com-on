(in-package #:org.shirakumo.com-on.cffi)

(cffi:define-foreign-library ole32
  (T (:default "Ole32")))

(defconstant CP-UTF8 65001)
(defconstant CLSCTX-ALL 23)
(defconstant FORMAT-MESSAGE-FROM-SYSTEM 4096)
(defconstant FORMAT-MESSAGE-IGNORE-INSERTS 512)
;; https://learn.microsoft.com/en-us/windows/win32/winprog/windows-data-types
;; https://learn.microsoft.com/en-us/windows/win32/winprog64/the-new-data-types
(cffi:defctype dword :uint32)
(cffi:defctype word :uint16)
(cffi:defctype long :int32)
(cffi:defctype short :int16)
(cffi:defctype byte :uint8)
(cffi:defctype char :uint8)
(cffi:defctype wchar :uint16)
(cffi:defctype uint-ptr #+64-bit :uint64 #-64-bit :uint32)
(cffi:defctype wparam :uintptr)
(cffi:defctype lparam :intptr)

(cffi:defctype lresult :intptr)

;; cffi :bool is c99 `_Bool` or c++ `bool`.
;; and is "usually 1 byte except on OSX where it is int"
;; cffi :boolean is `int`
(cffi:defctype bool (:boolean :int32)) ;; windows BOOL, 0 or 1
(cffi:defctype boolean (:boolean :uint8)) ;; windows BOOLEAN, 0 or 1

(cffi:defctype handle :intptr)
(cffi:defctype hdc :intptr)
(cffi:defctype hglrc :intptr)

(cffi:defcenum init
  (:multi-threaded #x0)
  (:apartment-threaded #x2)
  (:disable-ole1dde #x4)
  (:speed-over-memory #x8))

;; Ensure we don't redefine and thrash existing values.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (ignore-errors (cffi::parse-type 'hresult))
    (cffi:defcenum (hresult :ulong :allow-undeclared-values T)
      (:ok              #x00000000)
      (:false           #x00000001)
      (:abort           #x80004004)
      (:cancelled       #x800704C7)
      (:access-denied   #x80070005)
      (:fail            #x80004005)
      (:handle          #x80070006)
      (:invalid-arg     #x80070057)
      (:no-interface    #x80004002)
      (:not-implemented #x80004001)
      (:out-of-memory   #x8007000e)
      (:pointer         #x80004003)
      (:unexpected      #x8000ffff)
      (:changed-thread-mode #x80010106)
      (:already-initialized #x88890002)))

  ;; https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-erref/18d8fbe8-a967-4f1c-ae50-99ca8e491d2d
  (unless (ignore-errors (cffi::parse-type 'win32-error-code))
    (cffi:defcenum (win32-error-code :uint32 :allow-undeclared-values T)
      (:success             #x00000000)
      (:invalid-function    #x00000001)
      (:file-not-found      #x00000002)
      (:path-not-found      #x00000003)
      (:too-many-open-files #x00000004)
      (:access-denied       #x00000005)
      (:invalid-handle      #x00000006)
      (:gen-failure         #x0000001f)
      (:not-supported       #x00000032)
      (:invalid-parameter   #x00000057)
      (:insufficient-buffer #x0000007a))))

(cffi:defcstruct (com :conc-name || :class com)
  (vtbl :pointer))

(cffi:defcstruct (guid :conc-name guid- :class guid)
  (data1 dword)
  (data2 word)
  (data3 word)
  (data4 :uint8 :count 8))

(cffi:defcfun (initialize "CoInitializeEx") hresult
  (nullable :pointer)
  (init init))

(cffi:defcfun (uninitialize "CoUninitialize") :void)

(cffi:defcfun (create-instance "CoCreateInstance") hresult
  (rclsid :pointer)
  (punkouter :pointer)
  (dwclscontext dword)
  (riid :pointer)
  (ppv :pointer))

(cffi:defcfun (task-mem-free "CoTaskMemFree") :void
  (data :pointer))

(cffi:defcfun (wide-char-to-multi-byte "WideCharToMultiByte") :int
  (code-page :uint)
  (flags dword)
  (wide-char-str :pointer)
  (wide-char :int)
  (multi-byte-str :pointer)
  (multi-byte :int)
  (default-char :pointer)
  (used-default-char :pointer))

(cffi:defcfun (multi-byte-to-wide-char "MultiByteToWideChar") :int
  (code-page :uint)
  (flags dword)
  (multi-byte-str :pointer)
  (multi-byte :int)
  (wide-char-str :pointer)
  (wide-char :int))

(cffi:defcfun (get-last-error "GetLastError") dword)

(cffi:defcfun (format-message "FormatMessageW") dword
  (flags dword)
  (source :pointer)
  (message-id dword)
  (language-id dword)
  (buffer :pointer)
  (size dword)
  (arguments :pointer))
