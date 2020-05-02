#|
 This file is a part of com-on
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem com-on
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Utilities for dealing with COM interfaces."
  :homepage "https://shinmera.github.io/com-on/"
  :bug-tracker "https://github.com/shinmera/com-on/issues"
  :source-control (:git "https://github.com/shinmera/com-on.git")
  :serial T
  :components ((:file "package")
               (:file "weak-enum")
               (:file "bindings")
               (:file "wrapper")
               (:file "documentation"))
  :depends-on (:cffi
               :documentation-utils))
