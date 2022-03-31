(asdf:defsystem #:clogpower
  :description "New CLOG System"
  :author "some@one.com"
  :license  "BSD"
  :version "0.0.0"
  :serial t
  :depends-on (#:clog #:lichat-tcp-client)
  :components ((:file "clogpower")
	       (:file "clog-lichat-talker")))

