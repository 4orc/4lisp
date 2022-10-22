(defstruct options (keep 256))

(defvar *my* (make-options))

(defun end (&optional (fails 0)) 
  #+clisp (ext:exit fails)
  #+sbcl  (sb-ext:exit :code fails))

(defmacro ? (s x &rest xs)
  (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

(defmacro !! (x) `(slot-value *my* ',x))

(defmacro geta (x l &optional (init 0))
  `(cdr (or (geta ,l ,x)
            (car (setf ,l (cons (cons ,x ,init) ,l))))))

(defmacro defstruct+ (x &body body) 
  (let* ((slots (mapcar    (lambda (x) (if (consp x) (car x) x))          body))
         (show  (remove-if (lambda (x) (eq #\_ (char (symbol-name x) 0))) slots)))
    `(progn
       (defstruct (,x (:constructor ,(intern (format nil "%MAKE-~a" x)))) ,@body)
       (defmethod print-object ((self ,x) str)
         (labels ((fun (y) (format nil ":~(~a~) ~a" y (slot-value self y))))
           (format str "~a" (cons ',x (mapcar #'fun ',show))))))))

;;-------------------------------------------------------------------------
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

(defun lines (string) (splits string :char   #\Newline))
(defun cells (string &key (char #\,)) (splits string :char char :filter #'thing))

(defun with-lines (file fun)
  (with-open-file (s file) (loop (funcall fun (cells (or (read-line s nil) (return)))))))

(defun charn (x) (and (stringp x) (> (length x) 0) (char x (1- (length x)))))
;;---------------------------------------------------------------------------
(defstruct+ sample (_kept (make-array 2 :fill-pointer 0 :adjustable t)) (n 0) max ok)   
(defun make-sample (&optional (max (!! keep))) (%make-sample :max max))

(defstruct+ num 
  (txt "") (at 0) (n 0) (w 1)  
  (lo most-positive-fixnum) (hi most-negative-fixnum) (_has (make-sample)))     
(defun make-num (&optional (s "") (n 0)) ;;; create
  (%make-num :txt s :at n :w (if (eq #\- (charn s)) -1 1)))

(defstruct+ sym (txt "") (at 0) (n 0) has)   
(defun make-sym (&optional s n) (%make-sym :txt s :at n))

(defstruct+ row cells)
(defun make-row (lst) (%make-row :cells lst))

(defmethod add ((self row) (x number))
  (incf (? self n))
  (let ((size (length (? self _kept))))
    (cond ((< size  (? self max))
           (setf (? self ok) nil)
           (vector-push-extend x (? self _kept)))
          ((< (randf) (/ (? self n) (? self max)))
           (setf (? self ok) nil)
           (setf (elt (? self _kept) (randi size)) x)))))

(defmethod per ((self row) p)
  (let* ((all (sorted self))
         (n  (1- (length all))))
    (elt all (max 0 (min n (floor (* p n)))))))

(defmethod mid ((self row)) (per self .5))
(defmethod div ((self row)) (/ (- (per self .9) (per self .1)) 2.58))

(defmethod sorted ((self row))
  (unless (? self ok) (sort (? self _kept) #'<))
  (setf (? self ok) t)
  (? self _kept))

(defmethod better ((row1 row) (row2 row) data1)
  (let* ((s1 0) (s2 0)
         (cols (? data1 cols y))
         (n (length cols)))
    (dolist (col cols (< (/ s1 n) (/ s2 n)))
      (with-slots (at w) col
        (let ((x (norm col (elt (? row1 cells) at)))
              (y (norm col (elt (? row2 cells) at))))
          (decf s1 (exp (* w (/ (- x y) n))))
          (decf s2 (exp (* w (/ (- y x) n)))))))))

(defmethod around ((self row) rows data)
  (labels ((two (row2) (cons (dists self row2 data) row2)))
    (sort (mapcar #'two rows) 'car<)))

(defmethod far ((self row) rows data)
  (cdr (elt (around self rows data) (floor (* (length rows) (!! far))))))

(defmethod dists ((row1 row) (row2 row) data)
  (let ((d 0) (n 0))
    (dolist (col (? row1 _parent cols x))
      (incf n)
      (let ((d1 (dist col (elt (? row1 cells) (? col at)) 
                      (elt (? row2 cells) (? col at)) data))) 
        (incf d (expt d1 (!! p)))))
    (expt (/ d n) (/ 1 (!! p)))))
;;---------------------------------------------------------------------------
(defstruct+ cols names all x y klass)
(defun make-cols (lst)
  (let (all x y kl (at -1))
    (dolist (str lst (%make-cols :names lst :x x :y y :klass kl :all (reverse all)))
      (let* ((what (if (upper-case-p (char str 0)) #'make-num #'make-sym))
             (col  (funcall what str (incf at))))
        (push col all)
        (unless (eq #\X (charn str))
          (if (member (charn str) '(#\! #\- #\+)) (push col y) (push col x))
          (if (eq #\! (charn str)) (setf kl col)))))))

(defstruct+ data _has cols)  
(defun make-data (&optional src (self (%make-data)))
  (labels ((fun (x) (if (? self cols) 
                      (push (add self x) (? self _has))
                      (setf (? self cols) (make-cols x)))))
    (if (stringp src) (with-lines src #'fun) (mapcar #'fun src)) 
    self))
;;-------------------------------------------------------------------------
;;-------------------------------------------------------------------------
(defmethod add ((self num) x) ;;; Add one thing, updating 'lo,hi'
  (unless (eq x "?")
    (with-slots (lo hi) self
      (incf (? self n))
      (add (? self _has) x)
      (setf lo (min x (? self lo))
            hi (max x (? self hi))))))

(defmethod mid ((i num)) (mid (? i _has)))
(defmethod div ((i num)) (div (? i _has)))
;;------------------------------------
(defmethod add ((self sym) x)
  (unless (equalp "?" x)
    (incf (? self n) 1)
    (incf (? self has) 1)))

(defmethod mid ((i sym)) (loop for (key . n) in (? i has) maximizing n return key))

(defmethod div ((i sym))
  (labels ((fun (p) (* -1 (* p (log p 2)))))
    (loop for (_ . n) in (? i has) sum (fun (/ n (? i n))))))
;;------------------------------------------
(defmethod clone ((self data) &optional src) 
  (make-data (cons (? self cols names) src)))

(defmethod add ((self data) (row1 row))
  (dolist (cols `(,(? self cols x) ,(? self cols y)) row1)
    (dolist (col cols)
      (add col (elt (? row1 cells) (? col at))))))

(defmethod half ((self data) &optional all above)
  (let* ((all   (or    all (? self _has)))
         (some  (many  all (!! some)))
         (left  (or    above (far (any some) some self)))
         (right (far   left some self))
         (c     (dists left right self))
         (n 0)  lefts rights)
    (labels ((project (row) 
                      (let ((a (dists row left self))
                            (b (dists row right self)))
                        (cons (/ (+ (* a a) (* c c) (- (* b b))) (* 2 c)) row))))
      (dolist (one (sort (mapcar #'project all) #'car<))
        (if (<= (incf n) (/ (length all) 2))
          (push (cdr one) lefts)
          (push (cdr one) rights)))
      (values left right lefts rights c))))
;;-----------------------------------------
(print 10)
(make-data "../data/auto93.csv")
