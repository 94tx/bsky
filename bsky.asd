(defsystem #:bsky
  :author "lobo torres"
  :license "ISC"
  :depends-on (:alexandria
               :serapeum
               :verbose
               :bit-smasher
               :trivial-utf-8)
  :components ((:file "package")
               (:file "cbor")))
