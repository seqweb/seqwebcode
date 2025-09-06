;;; seqvar.lisp - Core seqvar functionality for Common Lisp
;;; Created by Waldo 2025-08-27

(defpackage :seqweb.seqvar
  (:use :cl :cl-sqlite :alexandria :uiop)
  (:export :seqvar-get :seqvar-set :seqvar-dump :seqvar-get-dict
           :seqvar-store-path :seqvar-error))

(in-package :seqweb.seqvar)

;;; Error condition
(define-condition seqvar-error (error)
  ((message :initarg :message :reader seqvar-error-message))
  (:report (lambda (condition stream)
             (format stream "SeqVar error: ~a" (seqvar-error-message condition)))))

;;; Database path
(defun seqvar-store-path ()
  "Get the path to the seqvar store database file."
  (let ((home (uiop:getenv "SEQWEBDEV_HOME")))
    (unless home
      (error 'seqvar-error :message "SEQWEBDEV_HOME environment variable not set"))
    (merge-pathnames ".state/env.sqlite" (pathname home))))

;;; Database connection
(defun db-connection ()
  "Get a database connection, creating the database if it doesn't exist."
  (let ((db-path (seqvar-store-path)))
    (ensure-directories-exist (directory-namestring db-path))
    (sqlite:connect db-path)))

;;; Initialize database schema
(defun ensure-schema (db)
  "Ensure the seqvars table exists."
  (sqlite:execute-non-query db
    "CREATE TABLE IF NOT EXISTS seqvars(
       ns TEXT NOT NULL DEFAULT 'SeqVar',
       key TEXT NOT NULL,
       val TEXT NOT NULL DEFAULT '',
       src TEXT,
       ts INTEGER NOT NULL,
       PRIMARY KEY(ns, key)
     )"))

;;; Core functions
(defun seqvar-get (key &optional (ns "SeqVar"))
  "Get a value from the seqvar store."
  (let ((db (db-connection)))
    (unwind-protect
        (progn
          (ensure-schema db)
          (let ((result (sqlite:execute-single db
                           "SELECT val FROM seqvars WHERE ns = ? AND key = ?"
                           ns key)))
            (or result "")))
      (sqlite:disconnect db))))

(defun seqvar-set (key val &optional (ns "SeqVar") (src nil))
  "Set a value in the seqvar store."
  (let ((db (db-connection))
        (now (get-universal-time)))
    (unwind-protect
        (progn
          (ensure-schema db)
          (sqlite:execute-non-query db
            "INSERT INTO seqvars(ns, key, val, src, ts) VALUES(?, ?, ?, ?, ?)
             ON CONFLICT(ns, key) DO UPDATE SET val = excluded.val, src = excluded.src, ts = excluded.ts"
            ns key val (or src "seqweb") now))
      (sqlite:disconnect db))))

(defun seqvar-dump (&optional (ns "SeqVar"))
  "Return all rows from the seqvar database."
  (let ((db (db-connection)))
    (unwind-protect
        (progn
          (ensure-schema db)
          (sqlite:execute-to-list db
            "SELECT key, val, src, ts FROM seqvars WHERE ns = ? ORDER BY key"
            ns))
      (sqlite:disconnect db))))

(defun seqvar-get-dict (&optional (pattern nil) (ns "SeqVar"))
  "Return key-value pairs as an alist."
  (let ((db (db-connection)))
    (unwind-protect
        (progn
          (ensure-schema db)
          (if pattern
              (let ((like-pattern (substitute #\% #\* pattern)))
                (sqlite:execute-to-list db
                  "SELECT key, val FROM seqvars WHERE ns = ? AND key LIKE ? ORDER BY key"
                  ns like-pattern))
              (sqlite:execute-to-list db
                "SELECT key, val FROM seqvars WHERE ns = ? ORDER BY key"
                ns)))
      (sqlite:disconnect db))))
