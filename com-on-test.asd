(asdf:defsystem com-on-test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Test system for com-on"
  :homepage "https://shinmera.com/docs/com-on/"
  :bug-tracker "https://shinmera.com/project/com-on/issues"
  :source-control (:git "https://shinmera.com/project/com-on.git")
  :serial T
  :components ((:file "tests"))
  :depends-on (:com-on :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :com-on-test)))
