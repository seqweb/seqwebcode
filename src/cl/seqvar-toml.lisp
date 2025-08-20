;; Created by Waldo 2025-08-20

;; TOML sidecar for SeqVar Facility - loads TOML files and writes to seqvar database.
;; Implements the common contract: flattens tables to dotted keys, stringifies values.

(defpackage :seqvar-toml
  (:use :cl)
  (:import-from :cl-toml :parse-toml)
  (:import-from :seqvar :seqvar-set)
  (:export #:load-toml #:write-toml-to-seqvar))
(in-package :seqvar-toml)

(defun flatten-toml (data &optional (prefix ""))
  "Recursively flatten TOML data structure into dotted key-value pairs.
   All values are converted to strings to maintain seqvar DB uniformity."
  (let ((result (make-hash-table :test 'equal)))
    (cond
      ((hash-table-p data)
       (maphash (lambda (key value)
                  (let ((new-prefix (if (string= prefix "")
                                       key
                                       (concatenate 'string prefix "." key))))
                    (maphash (lambda (k v) (setf (gethash k result) v))
                             (flatten-toml value new-prefix))))
                data))
      ((listp data)
       ;; Convert lists to JSON-like string representation
       (setf (gethash prefix result) (format nil "~A" data)))
      (t
       ;; Convert any other type to string
       (setf (gethash prefix result) (format nil "~A" data))))
    result))

(defun load-toml (paths)
  "Load TOML file(s) and return flattened key-value hash-table.
   
   PATHS can be a single pathname/string or a list of pathnames/strings.
   Returns a hash-table with flattened dotted keys and string values.
   
   Signals an error if any TOML file doesn't exist or if TOML parsing fails."
  (let* ((path-list (if (listp paths) paths (list paths)))
         (all-bindings (make-hash-table :test 'equal)))
    
    (dolist (path path-list)
      (let* ((path-obj (pathname path))
             (content (alexandria:read-file-into-string path-obj)))
        (unless (probe-file path-obj)
          (error "TOML file not found: ~A" path))
        
        ;; Parse TOML and flatten the structure
        (let* ((data (parse-toml content))
               (flattened (flatten-toml data)))
          (maphash (lambda (k v) (setf (gethash k all-bindings) v))
                   flattened))))
    
    all-bindings))

(defun write-toml-to-seqvar (bindings &key (ns "SeqVar") (src "seqweb"))
  "Write flattened key-value bindings to the seqvar database.
   
   BINDINGS is a hash-table of key-value pairs to write.
   NS is the namespace for the bindings (default: \"SeqVar\").
   SRC is the source identifier (default: \"seqweb\")."
  (maphash (lambda (key value)
             (seqvar-set key value :ns ns :src src))
           bindings))
