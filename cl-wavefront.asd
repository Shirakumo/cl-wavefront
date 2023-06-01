#|
 This file is a part of cl-wavefront
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-wavefront 
 :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A library to parse the Wavefront OBJ file format."
  :homepage "https://shirakumo.github.io/cl-wavefront/"
  :bug-tracker "https://github.com/shirakumo/cl-wavefront/issues"
  :source-control (:git "https://github.com/shirakumo/cl-wavefront.git")
  :serial T
  :components ((:file "package")
               (:file "objects")
               (:file "parser")
               (:file "serializer")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :parse-float
               :cl-ppcre)
  :in-order-to ((asdf:test-op (asdf:test-op :cl-wavefront-test))))
