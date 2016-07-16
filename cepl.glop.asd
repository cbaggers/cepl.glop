;;;; cepl.glop.asd

(asdf:defsystem #:cepl.glop
  :description "glop host for cepl"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :depends-on (#:cepl #:glop)
  :serial t
  :components ((:file "package")
               (:file "cepl.glop")))
