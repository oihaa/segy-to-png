;;;; segy-to-png.asd

(asdf:defsystem #:segy-to-png
  :description "Describe segy-to-png here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:segyio :csv-parser :zpng)
  :components ((:file "package")
	       (:file "zpng-extras")
               (:file "segy-to-png")))
