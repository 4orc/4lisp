(defmacro ?  (s x &rest xs) (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))
(defmacro !! (x) (second (slot-value *the* ',x)))

(defun args  ()                   #+sbcl sb-ext:*posix-argv*        #+clisp ext:*args*)
(defun stop  (&optional (code 0)) #+sbcl (sb-ext:exit :code status) #+:clisp (ext:exit code))

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

(defstruct struct)
(defmethod print-object ((self struct) str)
  (format str "(~a ~{~a~^ ~})" (type-of self)
          (mapcar (lambda (x) (format nil ":~(~a~) ~a" x (slot-value self x))) 
                  (public-slots self))))

(defmacro defstruct+ (x &body body) 
  `(defstruct (,x (:include struct)
                  (:constructor ,(intern (format nil "%MAKE-~a" x)))) ,@body))

; (defun settings (str)
;   (dolist (line (lines (help)))
;     (when (> (length line) 0)
;     (let ((words (splits line :char #\Space :filter #\1atom)))
;       (equal "-" (char (first words) 0)
;     (mapcar #'print (lines (car (slot-value x 'help))))
;     (format t "~&~%OPTIONS:~%")
;     (dolist (slot (public-slots x) (bye 0)) 
;       (format t " -~10a  ~20s ~s~%" (char (symbol-name slot) 0) 
;               (first (slot-value x slot)) (second (slot-value x slot))))))
;
;---------------------------------------------------------------------------------------------------
(defstruct+ options
  (help '("ewrew asda das as asd asdas asda", nil))
  (some '("how many samples to explore" 512)))

(defvar *the** (%make-options))
(defun help (x)
  (when (!! help)
    (mapcar #'print (lines (car (options-help x))))
    (format t "~&~%OPTIONS:~%")
    (dolist (slot (public-slots x) (bye 0)) 
      (format t " -~10a  ~20s ~s~%" (char (symbol-name slot) 0) 
              (first (slot-value x slot)) (second (slot-value x slot))))))

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
