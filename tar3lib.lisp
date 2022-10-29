(defmacro ?  (s x &rest xs) (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))
(defmacro !! (x) `(second (slot-value *the* ',x)))

(defun args  ()                   #+sbcl sb-ext:*posix-argv*        #+clisp ext:*args*)
(defun stop  (&optional (code 0)) #+sbcl (sb-ext:exit :code code) #+:clisp (ext:exit code))

(defun slots(x)     #+clisp(clos:class-slots (class-of x)) #+sbcl(sb-mop:class-slots (class-of x)))
(defun slot-name(x) #+clisp(clos:slot-definition-name x)   #+sbcl(sb-mop:slot-definition-name x))

(defun public-slots (self)
  (remove-if (lambda (x) (eq #\_ (char (symbol-name x) 0))) (mapcar 'slot-name (slots self))))

(defun charn (s) (elt s (1- (length s))))
(defun trim(x) (string-trim '(#\Space #\Tab #\Newline) x))

(defun 1atom (x)
  (let ((y (trim x)))
    (cond ((equal y "t") t)
          ((equal y "nil") nil)
          (t (let ((z (read-from-string y nil nil))) 
               (if (numberp z) z y))))))

(defun splits (str &key (char #\,) (filter #'identity))
  (loop for start = 0 then (1+ finish)
        for        finish = (position char str :start start)
        collecting (funcall filter (subseq str start finish))
        until      (null finish)))

(defun lines (string) (splits string :char #\Newline :filter #'trim))
(defun cells (string) (splits string :char #\, :filter #'1atom))

(defun with-lines (file fn)
  (with-open-file (s file) (loop (funcall fn (cells (or (read-line s nil) (return)))))))

(defstruct thing)
(defmethod print-object ((self thing) str)
  (format str "(~a ~{~a~^ ~})" (type-of self)
          (mapcar (lambda (x) (format nil ":~(~a~) ~a" x (slot-value self x))) 
                  (public-slots self))))

(defvar *oid* 0)

(defmacro defstruct+ (x &body body) 
  `(defstruct (,x (:include thing)
                  (:constructor ,(intern (format nil "%MAKE-~a" x)))) 
     (oid (incf *oid*)) ,@body))

(defstruct+ option flag name default current help)

(let (all)
  (defun opt (flag name val help)
    (push (%make-option :name name :flag flag :help help :default val :current val) all))

  i;(loop for i from 0 and x across items
    ;  do (format t "~a = ~a~%" i x))

  (defun options (&aux (args (args)))
    (dolist (one all)
      (dolist (arg while (setf arg (pop args))
        (if (equal arg (? one flag))
          (if (member (? one default) (list t nil))
            (setf (? one current) (not (? one current)))
            (setf (? one current) (1atom (pop args))))
            (arg (args))
        (if 
)

