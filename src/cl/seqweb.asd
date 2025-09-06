;;; seqweb.asd - ASDF system definition for SeqWeb Common Lisp implementation
;;; Created by Waldo 2025-08-27

(asdf:defsystem :seqweb
  :description "SeqWeb polyglot system - Common Lisp implementation"
  :version "0.1.0"
  :author "Waldo"
  :license "MIT"
  :depends-on (:cl-sqlite :cl-toml :alexandria :fiveam)
  :pathname "libs/core/"
  :components ((:file "seqvar")
              (:file "seqvar-toml"))
  :in-order-to ((test-op (test-op :seqweb/tests))))

;;; Test system
(asdf:defsystem :seqweb/tests
  :description "Tests for SeqWeb Common Lisp implementation"
  :version "0.1.0"
  :author "Waldo"
  :license "MIT"
  :depends-on (:seqweb :fiveam)
  :pathname "test/"
  :components ((:file "test-seqvar")
              (:file "show-seqvar"))
  :perform (test-op (o c)
                    (uiop:symbol-call :fiveam :run! :seqweb-tests)))
