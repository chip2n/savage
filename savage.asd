(asdf:defsystem #:savage
  :description "DSL for generating SVG images"
  :author "Andreas Arvidsson <andreas@arvidsson.io>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:arrow-macros #:chiputils)
  :components ((:file "package")
               (:file "core")))
