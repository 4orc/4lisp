(defmacro aif (test-form then-form &optional else-form) 
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhile (expr &body body) 
  `(do ((it ,expr ,expr))
     ((not it)) ,@body))

(defparameter *csv-readtable* (copy-readtable))
(setf (readtable-case *csv-readtable*) :preserve)
(set-syntax-from-char #\, #\Space *csv-readtable*)

(defun read-csv-line (string)
  (let ((*readtable* *csv-readtable*))
    (with-input-from-string (stream string)
      (loop for object = (read stream nil nil)
            while object
            collect object))))

(defun csv (file fun)
  (with-open-file (stream file)
    (awhile (read-line stream nil nil)
            (funcall fun (read-csv-line it)))))

(let ()
  (print 1)
  (csv "../data/SSM.csv" (lambda (row) ;(print row)
      t)))
