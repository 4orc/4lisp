(defmacro ? (s x &rest xs)
 (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))
  
  (defun charn (s) (elt s (1- (length s))))

(defun thing (x)
  (let ((y (string-trim '(#\Space #\Tab #\Newline) x)))
    (cond ((equal y "t") t)
          ((equal y "nil") nil)
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
  (with-open-file (s file) 
    (loop (funcall fn (cells (or (read-line s nil) (return)))))))

(defstruct struct)
(defmethod print-object ((self struct) str)
  (format str "(~a ~{~a~^ ~})" (type-of self)
     (mapcar (lambda (x) (format nil ":~(~a~) ~a" x (slot-value self x)))
             (public-slots self))))

(defmacro defstruct+ (x &body body) 
  (let ((slots (remove-if (lambda (x) (eq #\_ (char (symbol-name x) 0))) 
                          (mapcar (lambda (x) (if (consp x) (car x) x)) body))))
    `(progn 
       (defstruct (,x (:include struct)
                      (:constructor ,(intern (format nil "%MAKE-~a" x)))) ,@body)
       (defmethod public-slots ((self ,x)) ',slots))))

(defstruct+ options
  (some 512))

(defvar *the** (make-options))
 
(defstruct+ some (n 0) (max 512) _all)
(defun make-some (&key max)
(defstruct+ num (n 0) (mu 0) (sd 0) (m2 0) (some (make-some)))
(defstruct+ cols all x y klass)

(defun make-cols (lst &aux (self (%make-cols)) (at -1))
  (with-slots (all x y klass) self
    (labels ((nump   (s) (upper-case-p (char s 0)))
             (klassp (s) (eql #\! (charn s)))
             (goalp  (s) (member (chran s) '(#\! #\+ #\-)))
             (skipp  (s) (equal #\X charn s)))
      (dolist (txt lst self)
        (let ((col (if (nump txt) 
                        (make-num :txt txt :at (incf at))
                        (make-sym :txt txt :at (incf at)))))
             (push all col)
             (unless (skipp txt)
               (if (klassp txt) (setf klass col))
               (if (goalp txt) (push y col) (push x col))))))))




 
(print (make-cols nil));(with-lines "../data/auto93.csv" #'print)
