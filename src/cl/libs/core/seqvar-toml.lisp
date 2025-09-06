;;; seqvar-toml.lisp - TOML integration for seqvar facility
;;; Created by Waldo 2025-08-27

(defpackage :seqweb.seqvar-toml
  (:use :cl :cl-toml :alexandria :uiop)
  (:import-from :seqweb.seqvar :seqvar-set)
  (:export :load-toml :write-toml-to-seqvar))

(in-package :seqweb.seqvar-toml)

;;; TOML flattening utilities
(defun flatten-toml (data &optional (prefix ""))
  "Recursively flatten TOML data structure into dotted key-value pairs.
   All values are converted to strings to maintain seqvar DB uniformity."
  (let ((result (make-hash-table :test 'equal)))
    (labels ((flatten (obj current-prefix)
               (etypecase obj
                 (hash-table
                  (maphash (lambda (k v)
                             (let ((new-prefix (if (string= current-prefix "")
                                                  k
                                                  (format nil "~A.~A" current-prefix k))))
                               (flatten v new-prefix)))
                           obj))
                 (list
                  (setf (gethash current-prefix result) (format nil "~A" obj)))
                 (t
                  (setf (gethash current-prefix result) (format nil "~A" obj))))))
      (flatten data prefix))
    result))

;;; Main TOML loading function
(defun load-toml (paths)
  "Load TOML file(s) and return flattened key-value hash table.
   
   Args:
     paths - Single path or list of paths to TOML files
   
   Returns:
     Hash table with flattened dotted keys and string values
   
   Signals:
     file-error: If any TOML file doesn't exist
     cl-toml:toml-parse-error: If TOML parsing fails"
  (let ((path-list (if (listp paths) paths (list paths)))
        (all-bindings (make-hash-table :test 'equal)))
    
    (dolist (path path-list)
      (let ((path-obj (pathname path)))
        (unless (probe-file path-obj)
          (error 'file-error :pathname path-obj :format-control "TOML file not found: ~A"))
        
        (let* ((content (uiop:read-file-string path-obj))
               (data (cl-toml:parse content))
               (flattened (flatten-toml data)))
          (maphash (lambda (k v) (setf (gethash k all-bindings) v)) flattened))))
    
    all-bindings))

;;; Write bindings to seqvar
(defun write-toml-to-seqvar (bindings &optional (src nil))
  "Write flattened key-value bindings to the seqvar database.
   
   Args:
     bindings - Hash table of key-value pairs to write
     src - Source identifier (default: nil)"
  (maphash (lambda (key value)
             (seqvar-set key value "SeqVar" (or src "seqweb")))
           bindings))

;;; Convenience function for loading TOML and writing to seqvar
(defun load-toml-to-seqvar (paths &optional (src nil))
  "Load TOML file(s) and write directly to seqvar database.
   
   Args:
     paths - Single path or list of paths to TOML files
     src - Source identifier (default: nil)"
  (let ((bindings (load-toml paths)))
    (write-toml-to-seqvar bindings src)
    bindings))
