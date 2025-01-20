(in-package #:org.shirakumo.com-on)

;;; com.lisp
(docs:define-docs
  (function create
    "Create an instance of a COM class.

CLASS should be the GUID of the COM class. Typically named something
like CLSID_...

INSTANCE should be the GUID of the COM instance to access. Typically
named something like IID_...

Returns the pointer to the COM instance if successful, or signals an
error otherwise. You must release this instance when you are done with
it by calling RELEASE.

Automatically calls INIT.

See GUID
See WIN32-ERROR
See RELEASE
See INIT
See WITH-COM")

  (function release
    "Release a COM instance.

After releasing a COM instance, you /must not/ access it again, as it
may have been deallocated or invalidated.

You may get a COM instance through CREATE or some other API function
that returns an instance.

See CREATE
See RELEASEF
See WITH-COM
See WITH-COM*")

  (function releasef
    "Release a COM instance in PLACE if not NIL, and set PLACE to NIL.

You may get a COM instance through CREATE or some other API function
that returns an instance.

See CREATE
See RELEASE
See WITH-COM
See WITH-COM*")

  (function query-interface
    "Query a COM object for a pointer to one of its interfaces.

OBJECT must be a valid com instance.

RIID should be a GUID or string representation of a GUID.

Returns the pointer to the COM interface if successful, or signals an
error otherwise. You must release this interface when you are done
with it by calling RELEASE.

See WITH-COM
See RELEASE")

  (function add-ref
    "Increments the reference count for an interface pointer to a COM object.

OBJECT must be a valid com instance.

Returns the new reference count. This value is intended to be used only for test purposes.

See CREATE
See RELEASE")

  (function with-com
    "Hold a COM instance for the duration of the body.

This will ensure RELEASE is called on the instance when the body is
exited by any means.

INIT may be any form that returns a pointer to a COM instance.

See CREATE
See RELEASE")

  (function with-com*
    "Hold one or more COM instance for the duration of the body.

When the body is exited by any means, RELEASEF will be called on
variables defined by BINDINGS.

BINDINGS is a list of VARIABLE or (VARIABLE INIT), where INIT is
expected to return a COM instance to initialize VARIABLE. If INIT is
not provided, VARIABLE will be initialized to NIL. Bindings are
created as with LET*, so earlier bindings can be used in INIT.

See CREATE
See RELEASE
See RELEASEF
See WITH-COM")

  (function define-comfun
    "Define a method on a COM interface.

ARGS should be an optional docstring followed by a list of argument
declarations, with each argument being composed of an argument name
and a CFFI type.

This will create a function with the name of STRUCT-METHOD with the
declared arguments, which will attempt to call the related COM method
on the supplied COM instance. This method must be accessible through a
function called %STRUCT-METHOD to which a pointer to a VTBL can be
passed.

You will typically not use this macro by itself, and instead use
DEFINE-COMSTRUCT to perform the definition of a COM interface.

See COM:VTBL
See DEFINE-COMSTRUCT")
  
  (function define-comstruct
    "Define a COM interface structure.

NAME should be of the following structure:

  NAME      ::= name | (name &key (include 'iunknown) conc-name)
  name      --- The name of the CFFI structure type.
  include   --- COM interface to include in the interface being defined.
                Defaults to IUnknown. If set to NIL, the structure will
                not be an IUnknown, and will thus not include the standard
                methods QueryInterface, AddRef, and Release.
  conc-name --- The prefix for the structure interface functions.
                If not set, the name is used as a prefix.

METHODS should be a body of the following kinds of entries after an
optional docstring:

  METHODS      ::= (method [return-value] ARGUMENT*)*
  ARGUMENT     ::= (name type)
  method       --- Name of the interface method. You may pick this to
                   be whatever you like, there is no strict binding to
                   any C function.
  return-value --- The return value of the method. If not passed
                   explicitly, HRESULT is assumed.
  name         --- The name of the argument. Again, the name may be
                   arbitrarily chosen.
  type         --- The CFFI type that the argument should be of.

Note that the order of the methods /must/ be the same as in the actual
C header you're mimicking. You also /must/ include all of the methods
defined in the C header and cannot skip any. The order is what
actually defines which method is used. The name is purely on the Lisp
side.

Each COM interface (almost) always includes IUnknown, so has the
following three methods at the beginning, which DEFINE-COMSTRUCT adds
for you automatically:

  (QUERY-INTERFACE HRESULT (UID :POINTER) (OUT :POINTER))
  (ADD-REF :ULONG)
  (RELEASE :ULONG)

Also note that the THIS argument is always assumed for every method
and should therefore be omitted from the declarations.

For each method defined in the body, A DEFINE-COMFUN is generated,
which in turn will generate a function of the name NAME-METHOD using
the declared arguments and return type.

Alongside the methods, a C structure is defined which constitutes the
VTBL layout of the COM interface. Note that it does /not/ define the
COM instance layout itself. Each COM instance is assumed to merely be
a pointer to a structure with a pointer to a VTBL. None of this should
concern you terribly much, however. All you need to know is that you
can just pass a COM instance pointer to the method functions defined
by DEFINE-COMSTRUCT.

See DEFINE-COMFUN")
  
  (function init
    "Initialises the COM system if it has not yet been initialised.

This will load OLE32 and initialise COM for a multi-threaded
application.

This function must be called before any COM operations are performed.

Calling this function multiple times is safe.

See SHUTDOWN")
  
  (function shutdown
    "Uninitialises the COM system if it has been initialised.

After this you may not perform any further COM operations.

Calling this function multiple times is safe.

See INIT"))

;;; error.lisp
(docs:define-docs
  (function wstring->string
    "Converts a Windows 'wchar' string to a Lisp string and returns it.

See STRING->WSTRING")
  
  (function string->wstring
    "Converts a Lisp string to a Windows 'wchar' string and returns the pointer to this freshly allocated string.

See WSTRING->STRING")
  
  (function error-message
    "Returns the error message string for the given error code.

Unless specifically supplied, the last caused error code is used.

If you need to get an error message from a library, pass the library
handle as the second argument to this function.

See COM:GET-LAST-ERROR")
  
  (type win32-error
    "Condition type for errors coming from the Windows API.

This condition type is signalled whenever a Windows API call returns
unsuccessfully.

See WIN32-ERROR (function)
See FUNCTION-NAME
See CODE
See MESSAGE
See CHECK-LAST-ERROR
See CHECK-HRESULT")
  
  (function function-name
    "Returns the function name that caused the error, if known.

See WIN32-ERROR")
  
  (function code
    "Returns the windows error code associated with the problem.

See WIN32-ERROR")
  
  (function message
    "Returns a descriptive message about the error.

See WIN32-ERROR")
  
  (function win32-error
    "Signals an error of type WIN32-ERROR

Requires the Windows error code.
If no explicit MESSAGE is passed, the message is determined by
ERROR-MESSAGE.

See WIN32-ERROR (type)
See ERROR-MESSAGE")
  
  (function check-last-error
    "Convenience function to check the last error on failure.

If PREDICATE returns NIL, CLEANUP forms are run. After this, the error
code is retrieved through COM:GET-LAST-ERROR, and a WIN32-ERROR is
signalled using this code.

See WIN32-ERROR (type)")
  
  (function check-hresult
    "Convenience function to check the returned HRESULT and error on failure.

If the return value of VALUE-FORM is not one of the supplied EXPECTED
values, an error of type WIN32-ERROR is returned. If it is one of the
EXPECTED values, the value is returned.

If EXPECTED is not passed, it is assumed to be just (:OK).

See COM:HRESULT
See WIN32-ERROR (type)")
  
  (function with-deref
    "Shorthand to initialise a value by dereferencing.

Binds VAR to a pointer to a memory region of size fitting for TYPE,
then evaluates INIT. INIT should return a COM:HRESULT. If this result
is not :OK, an error is signalled. Otherwise, the memory region bound
to VAR is dereferenced as a value of TYPE, which is then returned.

Seee CHECK-HRESULT")

  (function add-hresult
    "Add HRESULT values.

This allows you to dynamically add new HRESULT keys to the enum.
The pairs should be specified like a plist:

  (add-hresult :foo #xDEAD :bar #xBEEF)

Existing keys or values will be silently overwritten.

See DEFINE-HRESULT
See COM:HRESULT")

  (function define-hresult
    "Define HRESULT values.

This allows you to dynamically add new HRESULT enum keys as if by
CFFI:DEFCENUM.

  (define-hresult
    (:foo #xDEAD)
    (:bar #xBEEF))

See ADD-HRESULT
See COM:HRESULT"))

;;; guid.lisp
(docs:define-docs
  (type guid
    "Encapsulation for a Windows GUID.

GUIDs are 16 byte identifiers that are used for COM classes and COM
instances.

The :ID initarg determines the GUID's contents. See MAKE-GUID.

A GUID instance may be passed as an argument to a C function where the
argument expects a COM:GUID structure pointer.

A GUID instance is usable as a literal and may be dumped to a FASL.

See COM:GUID
See BYTES
See GUID-STRING
See GUID (function)
See DEFINE-GUID")
  
  (function bytes
    "Returns a 16-octet vector describing the GUID.

Note that the vector elements are in the order expected in the
memory representation of the GUID, which may not be entirely
intuitive.

See GUID")

  (function guid-string
    "Returns a standard string representation of the GUID.

The bytes of the GUID are represented in hex format as follows:

 3 2 1 0 - 5 4 - 7 6 - 8 9 - 10 11 12 13 14 15

The reordering is due to the little-endian internal representation of
the octets. The passed GUID may either be a GUID instance, or a valid
GUID identifier from which a GUID can be constructed.

See GUID")

  (function guid=
    "Compares two GUIDs for equality.

Returns T if the two GUIDs are the same, NIL otherwise.

See GUID")
  
  (function guid
    "Create a new GUID instance.

ID may either be multiple values, or a single value determining the
GUID's actual ID values. The following ID types are allowed:

  STRING                --- Parses the string representation of the
                            UUID into its appropriate octets. Such a
                            UUID is typically of the form
                              XXXX-XX-XX-XX-XXXXXX
  LIST (16)             --- Uses the 16 octets in the list to build
                            the internal octet vector.
  LIST (11)             --- Uses the 11 integers in the list to build
                            the octet vector. Specifically, the list
                            should contain integers of the following
                            bit sizes:
                              32 16 16 8 8 8 8 8 8 8 8
                            This representation is sometimes found in
                            C headers.
  CFFI:FOREIGN-POINTER  --- Copies the contents from the supplied C
                            pointer to a GUID into the internal byte
                            vector.
  VECTOR (16)           --- Uses the 16 octets in the vector to build
                            the internal octet vector.
  NULL                  --- Fills the internal octet vector with 0s.

Supplying any integer anywhere in these values outside of the
specified ranges is an error.

See GUID (type)")
  
  (function define-guid
    "Define a GUID instance.

This is a shorthand for DEFCONSTANT of a GUID instance created from
the given ID argument.

See GUID (type)
See GUID (function)"))
