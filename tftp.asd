(defsystem tftp
  :name "tftp"
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An implementation of the TFTP standard for both client and server"
  :homepage "https://shinmera.github.io/tftp/"
  :bug-tracker "https://github.com/Shinmera/tftp/issues"
  :source-control (:git "https://github.com/Shinmera/tftp.git")
  :serial T
  :components ((:file "package")
               (:file "tftp")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :usocket
               :nibbles))
