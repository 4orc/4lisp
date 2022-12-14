;-----------------------------------------------------------------------
;; ## Macros
(defmacro ? (s x &rest xs)
  (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

;-----------------------------------------------------------------------
;; ## Lists
(defun geta (x lst) 
  (cdr (assoc x lst)))

(defun lt (x)
  (lambda (a b) (< (slot-value a x) (slot-value b x))))

(defun gt (x)
  (lambda (a b) (> (slot-value a x) (slot-value b x))))

;-----------------------------------------------------------------------
;; ## System Stuff
(defun args  ()                   
  #+sbcl  sb-ext:*posix-argv* 
  #+clisp ext:*args*)

(defun stop (&optional (code 0)) 
  #+sbcl   (sb-ext:exit :code code) 
  #+:clisp (ext:exit code))

(defun slots(x)     
  #+clisp (clos:class-slots (class-of x)) 
  #+sbcl  (sb-mop:class-slots (class-of x)))

(defun slot-name(x) 
  #+clisp (clos:slot-definition-name x)   
  #+sbcl  (sb-mop:slot-definition-name x))

(defun public-slots (i)
  (remove-if (lambda (x) (eq #\_ (char (symbol-name x) 0))) 
             (mapcar 'slot-name (slots i))))

;-----------------------------------------------------------------------
;; ## Symbols
(defun charn (s) (elt s (1- (length s))))

;-----------------------------------------------------------------------
;; ## Strings
(defun trim(x) 
  (string-trim '(#\Space #\Tab #\Newline) x))

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

(defun lines (string) 
  (splits string :char #\Newline :filter #'trim))

(defun cells (string) 
  (splits string :char #\, :filter #'1atom))

(defun with-lines (file fn)
  (with-open-file (s file) (loop (funcall fn (cells (or (read-line s nil) (return)))))))

;-----------------------------------------------------------------------
;; ## Maths
;; ### Random
(defvar *seed* 10013)

(defun randf (&optional (n 1.0))
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun randi (&optional (n 1)) ;--> 0..n-1
  (floor (* n (/ (randf 1000000000.0) 1000000000))))

(defmethod any ((i cons))   
  (any (coerce i 'vector)))

(defmethod any ((i vector)) 
  (elt i (randi (length i))))

(defmethod many ((i cons)   &optional (n 10)) 
  (many (coerce i 'vector) n))

(defmethod many ((i vector) &optional (n 10)) 
  (loop repeat n collect (any i)))

(defun rnd (number &optional (digits 3))
  (let* ((div (expt 10 digits))
         (tmp (/ (round (* number div)) div)))
    (if (zerop digits) (floor tmp) (float tmp))))

;-----------------------------------------------------------------------
;; ## Structs
(defvar *oid* -1)
(defstruct thing (oid (incf *oid*)))

(defmethod print-object ((i thing) str)
  (format str "(~a ~{~a~^ ~})" (type-of i)
          (mapcar (lambda (x) (format nil ":~(~a~) ~s" x (slot-value i x))) 
                  (public-slots i))))

(defmacro defstruct+ (x &body body) 
  `(defstruct (,x (:include thing)
                  (:constructor ,(intern (format nil "%MAKE-~a" x)))) ,@body))
;-----------------------------------------------------------
;; ## Options
(defstruct+ opt flag default current help)

(defvar *opts* nil)

(defun opt (flag help key val)
  (push (cons key (%make-opt :flag flag :help help :default val :current val)) 
        *opts*))

(defmacro is (x)
 `(? (geta ',x *opts*) current))

(defmethod default ((i opt))
  (setf (? i current) (? i default)))

(defmethod update ((i opt) v)
  (with-slots (default current) i
    (if (member default (list nil t))
      (setf current (not current)) ; boolean flags don't use val. just flip old
      (setf current v))))          ; otherwise, the val is the next arg

(defun help (opts)
  (when (is help)
    (format t "~%~{~a~%~}~%OPTIONS:~%" (lines (? (geta 'help opts) help)))
    (loop for (key . x) in opts do 
          (unless (eql 'help key) 
            (format t " ~5a ~a = ~a~%" (? x flag) (? x help) (? x current))))
    (stop)))

(defun defaults (opts) 
  (dolist (x opts) (default (cdr x)))
  (setf *seed* (is seed)))

(defun cli (&optional (opts *opts*))
  (labels ((fun (x arg args) (when arg
                               (if (equal arg (? x flag)) 
                                 (update x (1atom (car args))))
                               (fun x (car args) (cdr args)))))
    (let ((lst (args)))
      (dolist (x opts opts) (fun (cdr x) (car lst) (cdr lst))))
    (help opts)))
;-----------------------------------------------------------
;; ## Tests

(defvar *tests* nil)

(defmacro eg (name &body body)
  `(progn (pushnew ',name *tests*) (defun ,name ,@body)))

(defun run (&optional (tests *tests*) (opts *opts*))
  (let ((fails 0))
    (dolist (test tests)
      (when (or (equal (is go) "all") 
                (equal (is go) (format nil "~(~a~)" test)))  
        (defaults opts)
        (cond ((funcall test) (format t "??? pass ~a~%" test))
              (t              (format t "??? fail ~a~%" test)
                              (incf fails)))))
    (stop fails)))
