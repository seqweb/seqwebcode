;;; test-seqvar.lisp - Test suite for Common Lisp seqvar implementation
;;; Created by Waldo 2025-08-27

(defpackage :seqweb-tests
  (:use :cl :fiveam :seqweb.seqvar :seqweb.seqvar-toml)
  (:export :seqweb-tests))

(in-package :seqweb-tests)

;;; Test suite definition
(def-suite seqweb-tests
  :description "Tests for SeqWeb Common Lisp implementation")

(in-suite seqweb-tests)

;;; Test utilities
(defun cleanup-test-data ()
  "Clean up test data from the database."
  (let ((db (db-connection)))
    (unwind-protect
        (sqlite:execute-non-query db "DELETE FROM seqvars WHERE src = 'test-seqvar'")
      (sqlite:disconnect db))))

;;; Test cases
(test basic-get-set
  "Test basic get/set functionality"
  (cleanup-test-data)
  
  ;; Set a value
  (seqvar-set "test_key" "test_value" "SeqVar" "test-seqvar")
  
  ;; Get the value back
  (is (string= "test_value" (seqvar-get "test_key" "SeqVar")))
  
  ;; Test empty value
  (seqvar-set "empty_key" "" "SeqVar" "test-seqvar")
  (is (string= "" (seqvar-get "empty_key" "SeqVar")))
  
  ;; Test non-existent key
  (is (string= "" (seqvar-get "nonexistent_key" "SeqVar"))))

(test default-namespace
  "Test default namespace handling"
  (cleanup-test-data)
  
  ;; Set without specifying namespace
  (seqvar-set "default_key" "default_value" nil "test-seqvar")
  
  ;; Get without specifying namespace
  (is (string= "default_value" (seqvar-get "default_key"))))

(test dump-functionality
  "Test dump functionality"
  (cleanup-test-data)
  
  ;; Set multiple values
  (seqvar-set "dump_key1" "dump_value1" "SeqVar" "test-seqvar")
  (seqvar-set "dump_key2" "dump_value2" "SeqVar" "test-seqvar")
  
  ;; Test dump
  (let ((dump-result (seqvar-dump "SeqVar")))
    (is (>= (length dump-result) 2))
    (is (find "dump_key1" dump-result :key #'first :test #'string=))
    (is (find "dump_key2" dump-result :key #'first :test #'string=))))

(test get-dict-functionality
  "Test get_dict functionality"
  (cleanup-test-data)
  
  ;; Set values for dict test
  (seqvar-set "dict_key1" "dict_value1" "SeqVar" "test-seqvar")
  (seqvar-set "dict_key2" "dict_value2" "SeqVar" "test-seqvar")
  
  ;; Test get_dict without pattern
  (let ((dict-result (seqvar-get-dict nil "SeqVar")))
    (is (>= (length dict-result) 2))
    (is (find "dict_key1" dict-result :key #'first :test #'string=))
    (is (find "dict_key2" dict-result :key #'first :test #'string=))))

(test pattern-matching
  "Test pattern matching functionality"
  (cleanup-test-data)
  
  ;; Set values with pattern
  (seqvar-set "pattern.test.key1" "value1" "SeqVar" "test-seqvar")
  (seqvar-set "pattern.test.key2" "value2" "SeqVar" "test-seqvar")
  (seqvar-set "other.key" "other_value" "SeqVar" "test-seqvar")
  
  ;; Test pattern matching
  (let ((pattern-result (seqvar-get-dict "pattern.test.*" "SeqVar")))
    (is (= 2 (length pattern-result)))
    (is (every (lambda (item) (string= "pattern.test." (subseq (first item) 0 13))) pattern-result))))

(test toml-integration
  "Test TOML integration functionality"
  (cleanup-test-data)
  
  ;; Create a temporary TOML file
  (let* ((toml-content "[section]
key1 = \"value1\"
key2 = \"value2\"
[section.subsection]
key3 = \"value3\"")
         (temp-file (uiop:with-temporary-file (:stream s :pathname p)
                      (write-string toml-content s)
                      p)))
    
    (unwind-protect
        (progn
          ;; Load TOML and write to seqvar
          (let ((bindings (load-toml-to-seqvar temp-file "test-seqvar")))
            (is (= 3 (hash-table-count bindings)))
            
            ;; Check that values were written
            (is (string= "value1" (seqvar-get "section.key1")))
            (is (string= "value2" (seqvar-get "section.key2")))
            (is (string= "value3" (seqvar-get "section.subsection.key3")))))
      (delete-file temp-file))))

(test error-handling
  "Test error handling"
  (cleanup-test-data)
  
  ;; Test with missing SEQWEBDEV_HOME
  (let ((old-home (uiop:getenv "SEQWEBDEV_HOME")))
    (unwind-protect
        (progn
          (setf (uiop:getenv "SEQWEBDEV_HOME") nil)
          (signals seqvar-error (seqvar-store-path)))
      (when old-home
        (setf (uiop:getenv "SEQWEBDEV_HOME") old-home)))))

;;; Test runner
(defun run-tests ()
  "Run all seqweb tests"
  (run! 'seqweb-tests))
