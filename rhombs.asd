;;;; rhombs.asd

(asdf:defsystem #:rhombs
  :description "P2 and P3 tile generator"
  :author "José Miguel Ángel Ronquillo Rivera <jose@rufina.link>"
  :license  "GPL Ver. 3"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop
               #:alexandria
               #:bordeaux-threads
               #:local-time
               #:mcclim)
  :components ((:file "package")
               (:file "objects")
               (:file "rhombs")))
