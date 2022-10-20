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
  (with-open-file (s file) (loop (funcall fun (or (read-line s nil) (return))))))

(defun charn (x) (and (stringp x) (> (length x) 0) (char x (1- (length x)))))
;;---------------------------------------------------------------------------
(defstruct+ sample (_kept (make-array 2 :fill-pointer 0 :adjustable t)) (n 0) max ok)   
(defun make-sample (&optional (max (!! keep)) (%make-sample :max max)))

(defstruct+ num 
  (txt "") (at 0) (n 0) (w 1)  
  (lo most-positive-fixnum) (hi most-negative-fixnum) (_has (make-sample)))     
(defun make-num (&optional (s "") (n 0)) ;;; create
  (%make-num :txt s :at n :w (if (eq #\- (charn s)) -1 1)))

(defstruct+ sym (txt "") (at 0) (n 0) has)   
(defun make-sym (&optional s n) (%make-sym :txt s :at n))

(defstruct+ row cells)
(defun make-row (rows lst) (%make-row :_parent rows :cells lst :cooked (copy-list lst)))

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
(defun make-data (&optional src (i (%make-rows)))
  (labels ((top.row.is.special  (x) (if (? i cols)
                                     (push (add i x) (? i _has))
                                     (setf (? i cols) (make-cols x)))))
    (if (stringp src)
      (with-lines src (lambda (line) (top.row.is.special (cells line))))
      (mapcar #'top.row.is.special src))
    i))
;;-------------------------------------------------------------------------
(defmethod add ((i row) (x number))
  (incf (? i n))
  (let ((size (length (? i _kept))))
    (cond ((< size  (? i max))
           (setf (? i ok) nil)
           (vector-push-extend x (? i _kept)))
          ((< (randf) (/ (? i n) (? i max)))
           (setf (? i ok) nil)
           (setf (elt (? i _kept) (randi size)) x)))))

(defmethod per ((i row) p)
  (let* ((all (sorted i))
         (n  (1- (length all))))
    (elt all (max 0 (min n (floor (* p n)))))))

(defmethod mid ((i row)) (per i .5))
(defmethod div ((i row)) (/ (- (per i .9) (per i .1)) 2.58))

(defmethod sorted ((i row))
  (unless (? i ok) (sort (? i _kept) #'<))
  (setf (? i ok) t)
  (? i _kept))
;;-------------------------------------------------------------------------
(defmethod better ((row1 row) (row2 row) (data1 data))
  (let* ((s1 0) (s2 0)
         (cols (? data1 cols y))
         (n (length cols)))
    (dolist (col cols (< (/ s1 n) (/ s2 n)))
      (with-slots (at w) col
        (let ((x (norm col (elt (? row1 cells) at)))
              (y (norm col (elt (? row2 cells) at))))
          (decf s1 (exp (* w (/ (- x y) n))))
          (decf s2 (exp (* w (/ (- y x) n)))))))))

(defmethod around ((row1 row) allrows data)
  (labels ((two (row2) (cons (dists row1 row2 data) row2)))
    (sort (mapcar #'two allrows) 'car<)))

(defmethod far ((i row) allrows data)
  (cdr (elt (around i allrows data) (floor (* (length allrows) (!! far))))))

(defmethod dists ((row1 row) (row2 row) data)
  (let ((d 0) (n 0))
    (dolist (col (? row1 _parent cols x))
      (incf n)
      (let ((d1 (dist col (elt (? row1 cells) (? col at)) 
                      (elt (? row2 cells) (? col at)) data))) 
        (incf d (expt d1 (!! p)))))
    (expt (/ d n) (/ 1 (!! p)))))
;;------------------------------------------
(defmethod clone ((data1 data) &optional src) 
  (make-data (cons (? data1 cols names) src)))

(defmethod add ((data1 data) (lst cons)) 
  (add data1 (make-row data1 lst)))

(defmethod add ((data1 data) (row1 row))
  (dolist (cols `(,(? data1 cols x) ,(? data1 cols y)) row1)
    (dolist (col cols)
      (add col (elt (? row1 cells) (? col at))))))

(defmethod half ((data1 data) &optional all above)
  (let* ((all   (or    all (? data1 _has)))
         (some  (many  all (!! some)))
         (left  (or    above (far (any some) some data1)))
         (right (far   left some data1))
         (c     (dists left right data1))
         (n 0)  lefts rights)
    (labels ((project (row)
                (let ((a (dists row left data1))
                      (b (dists row right data1)))
                  (cons (/ (+ (* a a) (* c c) (- (* b b))) (* 2 c)) row))))
      (dolist (one (sort (mapcar #'project all) #'car<))
        (if (<= (incf n) (/ (length all) 2))
          (push (cdr one) lefts)
          (push (cdr one) rights)))
      (values left right lefts rights c))))
;;-----------------------------------------
(print (make-cols '("aa" "Naa" "Ccc-")))

;(with-lines "../data/auto93.csv" (lambda (row) (print (cells row))))
