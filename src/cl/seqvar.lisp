;; Created by Waldo 2025-08-20

(defpackage :seqvar
  (:use :cl)
  (:export #:seqvar-get #:seqvar-set #:seqvar))
(in-package :seqvar)

(define-condition seqvar-error (simple-error) ())

(defun getenv (name)
  #+allegro (sys:getenv name)
  #-allegro (uiop:getenv name))

(defun %db-path ()
  (let ((home (getenv "SEQWEBDEV_HOME")))
    (unless home (error 'seqvar-error :format-control "SEQWEBDEV_HOME is not set"))
    (let* ((base (truename (pathname home)))
           (db (merge-pathnames ".state/env.sqlite" base)))
      (unless (probe-file db)
        (error 'seqvar-error :format-control "seqvar store not initialized: ~A missing. Run SeqWeb bootstrap." :format-arguments (list db)))
      db)))

(defun %run-sql (sql &key readonly)
  (let* ((db (namestring (%db-path)))
         (cmd (append (list "sqlite3") (when readonly '("-readonly")) (list db sql))))
    (multiple-value-bind (out code)
        (if (find-package :uiop)
            (let* ((pi (uiop:run-program cmd :output :string :ignore-error-status t :search t)))
              (values (uiop:process-info-output pi) (uiop:process-info-exit-code pi)))
          #+allegro
          (let* ((tmp (make-pathname :name (format nil "seqvar-~D" (get-internal-real-time))
                                     :type "tmp" :defaults (truename ".")))
                 (shell (format nil "~{~A~^ ~}" cmd))
                 (ec (excl:run-shell-command (concatenate 'string shell " > " (namestring tmp))
                                             :wait t :ignore-error-status t))
                 (out (with-open-file (s tmp :direction :input :external-format :utf-8)
                        (let ((txt (make-string (file-length s)))) (read-sequence txt s) txt))))
            (values out (if (numberp ec) ec 1)))
          #-allegro
          (error 'seqvar-error :format-control "No process runner (UIOP/Allegro) available")))
      (when (not (zerop code))
        (error 'seqvar-error :format-control "sqlite3 failed (~A)" :format-arguments (list code)))
      (string-right-trim '(#\Newline #\Return) out))))

(defun unix-now () (- (get-universal-time) 2208988800))

(defun escape-sql (s)
  (with-output-to-string (out)
    (loop for ch across s do
      (write-char ch out)
      (when (char= ch #\') (write-char #\' out)))))

(defun seqvar-get (key &key (ns "SeqVar"))
  (declare (type string key ns))
  (let* ((sql (format nil
                      "SELECT IFNULL((SELECT val FROM seqvars WHERE ns='~A' AND key='~A'),'');"
                      (escape-sql ns) (escape-sql key))))
    (%run-sql sql :readonly t)))

(defun seqvar-set (key val &key (ns "SeqVar") (src "seqweb"))
  (declare (type string key val ns src))
  (let* ((ts (unix-now))
         (sql (format nil
               "INSERT INTO seqvars(ns,key,val,src,ts) VALUES('~A','~A','~A','~A',~D) ~
                ON CONFLICT(ns,key) DO UPDATE SET val=excluded.val, src=excluded.src, ts=excluded.ts;"
               (escape-sql ns) (escape-sql key) (escape-sql val) (escape-sql src) ts)))
    (%run-sql sql)
    val))

;; Convenience accessor
(defsetf seqvar (key &key (ns "SeqVar")) (val)
  `(seqvar-set ,key ,val :ns ,ns))
(defun seqvar (key &key (ns "SeqVar")) (seqvar-get key :ns ns))
