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

(defun public-slots (self)
  (remove-if (lambda (x) (eq #\_ (char (symbol-name x) 0))) 
             (mapcar 'slot-name (slots self))))

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

(defun randi (&optional (n 1))
  (floor (* n (/ (randf 1000000000.0) 1000000000))))

(defmethod any ((i cons))   
  (any (coerce i 'vector)))

(defmethod any ((i vector)) 
  (elt i (randi (length i))))

(defmethod many ((i cons)   &optional (n 10)) 
  (many (coerce i 'vector) n))

(defmethod many ((i vector) &optional (n 10)) 
  (loop repeat n collect (any i)))

;-----------------------------------------------------------------------
;; ## Structs
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
;-----------------------------------------------------------
;; ## Options
(defstruct+ opt flag default current help)

(defvar *opts* nil)

(defun opt (flag help key val)
  (push (cons key (%make-opt :flag flag :help help :default val :current val)) 
        *opts*))

(defmacro is (x)
 `(? (geta ',x *opts*) current))

(defmethod reset1 ((self opt))
  (setf (? self current) (? self default)))

(defmethod update ((self opt) v)
  (with-slots (default current) self
    (if (member default (list nil t))
      (setf current (not current)) ; boolean flags don't use val. just flip old
      (setf current v))))          ; otherwise, the val is the next arg

(defun help (opts)
  (when (is help)
    (format t "~%~{~a~%~}~%OPTIONS:~%" (lines (? (geta 'help opts) help)))
    (loop for (key . x) in opts do 
          (unless (eql 'help key) 
            (format t " ~5a ~a = ~a~%" (? x flag) (? x help) (? x current)))))
  opts)

(defun resets (opts) 
  (dolist (x opts) (reset1 (cdr x)))
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

(defmacro deftest (name params &body body)
  `(progn (pushnew ',name *tests*) (defun ,name ,params  ,@body)))

(deftest hi() 
  (format t "~&Welcome to keys~%") t)

(defun sym (the)
  (let ((n (add (make-sym) '("a" "b" "b" "c" "c" "c" "c"))))
    (want (< 1.378 (var n) 1.379) "bad ent")))

(defun num (the)
  (let ((n (add (init(make-num  :txt "asd-"))
                '(2 3 4 4 4 4  5  5  6  7 
                  7 8 9 9 9 9 10 11 12 12))))
    (want (= 3.125 (var n)) "bad sd")
    (want (= 7     (mid n)) "bad mean")))

(defun run (&optional (tests *tests*) (opts *opts*))
  (let ((fails 0))
    (dolist (test tests)
      (when (or (equal (is go) "all") 
                (equal (is go) (format nil "~(~a~)" test)))  
        (resets opts)
        (cond ((funcall test) (format t "✅ pass ~a~%" test))
              (t              (format t "❌ fail ~a~%" test)
                              (incf fails)))))
    (stop fails)))
