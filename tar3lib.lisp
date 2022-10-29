(defmacro ?  (s x &rest xs) (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

(defmacro af (test-form then-form &optional else-form) 
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro whan (test-form &body then-form)
  `(let ((it ,test-form))
     (when it ,@then-form)))

(defun ata (x lst) (cdr (assoc x list)))

(defun args  ()                   #+sbcl sb-ext:*posix-argv*        #+clisp ext:*args*)
(defun stop  (&optional (code 0)) #+sbcl (sb-ext:exit :code code) #+:clisp (ext:exit code))

(defun slots(x)     #+clisp(clos:class-slots (class-of x)) #+sbcl(sb-mop:class-slots (class-of x)))
(defun slot-name(x) #+clisp(clos:slot-definition-name x)   #+sbcl(sb-mop:slot-definition-name x))

(defun public-slots (self)
  (remove-if (lambda (x) (eq #\_ (char (symbol-name x) 0))) (mapcar 'slot-name (slots self))))

(defun charn (s) (elt s (1- (length s))))
(defun trim(x) (string-trim '(#\Space #\Tab #\Newline) x))

(defun 1atom (x)
  (letnam(y (trim x)))
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
          (mapcar (lambda (x) (format nil ":~(~a~) ~s" x (slot-value self x))) 
                  (public-slots self))))

(defvar *oid* 0)

(defmacro defstruct+ (x &body body) 
  `(defstruct (,x (:include thing)
                  (:constructor ,(intern (format nil "%MAKE-~a" x)))) 
     (oid (incf *oid*)) ,@body))
;;-----------------------------------------------------------
;; ## Options
(defstruct+ option flag default current help)

(defvar *opts* nil)

(defun opt (key val flag help)
  (push (cons key (%make-option :flag flag :help help :default val :current val) *opts*)))

(defmacro is (x)
 `(? (ata ,x *opts) current))

(defun help()
  (whan (ata 'help *opts*)
        (mapcar #'print (lines (? it current)))
        (format t "~%OPTIONS:~%")
        (loop :for (key . x) :on *opts* :by #'cddr :do
              (unless (eql 'help)
                (format t " ~5a [~5a] ~a" (? x flag) (? x current) (? x help)))))

(defun reset () 
  (loop for (key . x) in *opts* do
    (setf (? x current) (? x default))))

(defun update (&aux (lst (args)))
  (labels 
    ((fn (x arg args)
         (when arg
           (if (equal arg (? x flag))
             (if (member (? x default) (list nil t))
               (setf (? x current) (not (? x current))) ; boolean flags need no val. just flip old
               (setf (? x current) (1atom (car args))))); otherwise, the val is the next arg
           (fn x (car args) (cdr args)))))
    (dolist (x *opts* *opts*)
      (fn x (car lst)  (cdr lst)))))
