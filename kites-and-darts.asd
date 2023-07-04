;;;; kites-and-darts.asd

(asdf:defsystem #:kites-and-darts
  :description "P2 tile generator"
  :author "José Miguel Ángel Ronquillo Rivera <jose@rufina.link>"
  :license  "GPL Ver. 3"
  :version "0.0.1"
  :serial t
  :depends-on (#:bordeaux-threads
               ;;#:local-time
               #:mcclim)
  :components ((:file "package")
               (:file "polynomials")
               (:file "objects")
               (:file "kites-and-darts")))
