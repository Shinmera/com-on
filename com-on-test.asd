(asdf:defsystem com-on-test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Test system for com-on"
  :homepage "https://shinmera.github.io/com-on/"
  :bug-tracker "https://github.com/shinmera/com-on/issues"
  :source-control (:git "https://github.com/shinmera/com-on.git")
  :serial T
  :components ((:file "tests"))
  :depends-on (:com-on :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :com-on-test)))
