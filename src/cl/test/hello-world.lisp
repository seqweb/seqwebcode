;;; hello-world.lisp - Simple Common Lisp hello world program
;;; Created by Waldo 2025-08-27
;;;
;;; This program verifies basic Common Lisp functionality in the SeqWeb environment.

(defpackage :hello-world
  (:use :cl)
  (:export :main))

(in-package :hello-world)

(defun main ()
  "Main function that prints hello world."
  (format t "Hello, World!~%")
  (format t "Common Lisp is working in the SeqWeb environment!~%")
  
  ;; Verify some basic Common Lisp functionality
  (format t "Lisp implementation: ~A~%" (lisp-implementation-type))
  (format t "Lisp version: ~A~%" (lisp-implementation-version))
  
  ;; Test basic string operations
  (let ((test "Hello SeqWeb"))
    (format t "✓ String operations working: ~A~%" (string-upcase test)))
  
  ;; Test basic math
  (let ((result (+ 2 2)))
    (format t "✓ Basic math working: 2 + 2 = ~A~%" result))
  
  ;; Test list operations
  (let ((test-list '(1 2 3 4 5)))
    (format t "✓ List operations working: ~A~%" (reverse test-list)))
  
  (format t "Common Lisp environment verification complete!~%"))

;;; Allow running directly
(when (member (pathname-name *load-pathname*) '("hello-world" "hello-world.lisp") :test #'string=)
  (main))
