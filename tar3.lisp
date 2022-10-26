(defun thing (x)
  (let ((y (string-trim '(#\Space #\Tab #\Newline) x)))
    (cond ((string= y "t") t)
          ((string= y "nil") nil)
          (t (let ((z (read-from-string y nil nil))) 
               (if (numberp z) z y))))))

(defun splits (str &key (char #\,) (filter #'identity))
  (loop 
    for start = 0 then (1+ finish)
    for        finish = (position char str :start start)
    collecting (funcall filter (subseq str start finish))
    until      (null finish)))

(defun cells (string) (splits string :char #\, :filter #'thing))

(defun with-lines (file fn)
  (with-open-file (s file) (loop (funcall fn (cells (or (read-line s nil) (return)))))))

(defstruct struct)
(defmethod print-object ((self struct) str)
  (format str "(~a ~{~a~^ ~})" (type-of self)
     (mapcar (lambda (x) (format nil ":~(~a~) ~a" x (slot-value self x)))
             (public-slots self))))

(defmacro defstruct+ (x &body body) 
  `(progn 
     (defstruct (,x (:include struct)
                    (:constructor ,(intern (format nil "%MAKE-~a" x)))) ,@body)
     (defmethod public-slots ((self ,x)) 
       ',(remove-if (lambda (x) (eq #\_ (char (symbol-name x) 0))) 
                    (mapcar (lambda (x) (if (consp x) (car x) x)) body)))))

(defstruct+ fred (k 23) (a 1) (_b 2))

(format t "~&~a" (%make-fred))
