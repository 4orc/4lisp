(defun trim (x) (string-trim '(#\Space #\Tab #\Newline) x))

(defun thing (x &aux (y (trim x)))
  (cond ((string= y "t") t)
        ((string= y "nil") nil)
        (t (let ((z (read-from-string y nil nil))) 
             (if (numberp z) z y)))))

(defun splits (str &key (char #\,) (filter #'identity))
  (loop for start = 0 then (1+ finish)
    for        finish = (position char str :start start)
    collecting (funcall filter (trim (subseq str start finish)))
    until      (null finish)))

(defun cells (string) (splits string :char #\, :filter #'thing))

(defun with-lines (file fun)
  (with-open-file (s file) (loop (funcall fun (cells (or (read-line s nil) (return)))))))

(defun show-slots(self)
  (labels ((slots (it) #+clisp (class-slots (class-of it))
                       #+sbcl (sb-mop:class-slots (class-of it)))
           (name (x)   #+clisp (slot-definition-name x)
                       #+sbcl (sb-mop:slot-definition-name x)))
    (remove-if (lambda (x) (eq #\_ (char (symbol-name x) 0))) 
               (mapcar #'name (slots self)))))

(defun show (self &optional (str t))
  (labels ((show1       (y) (format nil ":~(~a~) ~a" y (slot-value self y))))
    (format str "~a" (cons (type-of self)  (mapcar #'show1 (show-slots self))))))

(defmacro defstruct+ (x &body body) 
  `(progn (defstruct (,x (:constructor ,(intern (format nil "%MAKE-~a" x)))) ,@body)
          (defmethod print-object ((self ,x) str) (show self str))))

(defstruct+ fred (k 23) (a 1) (_b 2))

 (print (%make-fred))
