;;; show-seqvar.lisp - Demonstration script for Common Lisp seqvar facility
;;; Created by Waldo 2025-08-27

(defpackage :seqweb-demo
  (:use :cl :seqweb.seqvar :seqweb.seqvar-toml)
  (:export :show-seqvar-demo))

(in-package :seqweb-demo)

(defun show-seqvar-demo ()
  "Demonstrate the Common Lisp seqvar facility"
  (format t "=== Common Lisp SeqVar Facility Demonstration ===~%~%")
  
  ;; Check if we can access the database
  (handler-case
      (seqvar-store-path)
    (seqvar-error (c)
      (format t "ERROR: Cannot access seqvar database.~%")
      (format t "Make sure SEQWEBDEV_HOME is set and the seqvar database is accessible.~%")
      (return-from show-seqvar-demo)))
  
  (format t "✓ Database connection successful~%~%")
  
  ;; Demonstrate basic operations
  (format t "1. Setting values...~%")
  (seqvar-set "demo.key1" "value1" "SeqVar" "show-seqvar")
  (seqvar-set "demo.key2" "value2" "SeqVar" "show-seqvar")
  (seqvar-set "demo.key3" "value3" "SeqVar" "show-seqvar")
  (format t "   Set 3 demo keys~%~%")
  
  (format t "2. Getting values...~%")
  (format t "   demo.key1 = ~A~%" (seqvar-get "demo.key1"))
  (format t "   demo.key2 = ~A~%" (seqvar-get "demo.key2"))
  (format t "   demo.key3 = ~A~%~%" (seqvar-get "demo.key3"))
  
  (format t "3. Dumping all values...~%")
  (format t "   Dump output:~%")
  (let ((dump-result (seqvar-dump "SeqVar")))
    (dolist (row dump-result)
      (when (search "demo.key" (first row))
        (format t "     KEY=~A VAL=~A SRC=~A TS=~A~%" 
                (first row) (second row) (third row) (fourth row)))))
  (format t "~%")
  
  (format t "4. Getting dictionary with pattern matching...~%")
  (format t "   All demo keys:~%")
  (let ((dict-result (seqvar-get-dict "demo.*" "SeqVar")))
    (dolist (item dict-result)
      (format t "     ~A=~A~%" (first item) (second item))))
  (format t "~%")
  
  (format t "5. Getting all values as dictionary...~%")
  (format t "   All values:~%")
  (let ((all-result (seqvar-get-dict nil "SeqVar")))
    (dolist (item all-result)
      (when (search "demo.key" (first item))
        (format t "     ~A=~A~%" (first item) (second item)))))
  (format t "~%")
  
  ;; Demonstrate TOML integration
  (format t "6. TOML integration demonstration...~%")
  (let* ((toml-content "[demo]
key4 = \"toml_value4\"
key5 = \"toml_value5\"
[demo.subsection]
key6 = \"toml_value6\"")
         (temp-file (uiop:with-temporary-file (:stream s :pathname p)
                      (write-string toml-content s)
                      p)))
    
    (unwind-protect
        (progn
          (format t "   Created temporary TOML file with 3 keys~%")
          
          ;; Load TOML and write to seqvar
          (let ((bindings (load-toml-to-seqvar temp-file "show-seqvar")))
            (format t "   Loaded ~A bindings from TOML~%" (hash-table-count bindings)))
          
          ;; Show the new values
          (format t "   New TOML-loaded values:~%")
          (format t "     demo.key4 = ~A~%" (seqvar-get "demo.key4"))
          (format t "     demo.key5 = ~A~%" (seqvar-get "demo.key5"))
          (format t "     demo.subsection.key6 = ~A~%" (seqvar-get "demo.subsection.key6")))
      (delete-file temp-file)))
  (format t "~%")
  
  ;; Cleanup demo data
  (format t "7. Cleaning up demo data...~%")
  (let ((db (db-connection)))
    (unwind-protect
        (sqlite:execute-non-query db "DELETE FROM seqvars WHERE src = 'show-seqvar'")
      (sqlite:disconnect db)))
  (format t "   Demo data cleaned up~%~%")
  
  (format t "=== Demonstration Complete ===~%~%")
  (format t "This shows the basic usage of the Common Lisp seqvar facility.~%")
  (format t "The library provides:~%")
  (format t "  - seqvar-get KEY [NS]     - Get a value~%")
  (format t "  - seqvar-set KEY VALUE [NS] [SRC] - Set a value~%")
  (format t "  - seqvar-dump [NS]        - Dump all values~%")
  (format t "  - seqvar-get-dict [PATTERN] [NS] - Get values as alist~%")
  (format t "  - TOML integration via load-toml and write-toml-to-seqvar~%~%")
  (format t "For testing, see test-seqvar.lisp~%"))
