(asdf:defsystem com-on
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Utilities for dealing with COM interfaces."
  :homepage "https://shinmera.github.io/com-on/"
  :bug-tracker "https://github.com/shinmera/com-on/issues"
  :source-control (:git "https://github.com/shinmera/com-on.git")
  :serial T
  :components ((:file "package")
               (:file "bindings")
               (:file "error")
               (:file "guid")
               (:file "com")
               (:file "documentation"))
  :depends-on (:cffi
               :documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :com-on-test))))
