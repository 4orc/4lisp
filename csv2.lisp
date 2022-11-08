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

(defun csv (file fn)
  (with-open-file (s file) (loop (funcall fn (cells (or (read-line s nil) (return)))))))

(let ()
  (print 1)
  (csv "../data/SSM.csv" (lambda (row) ;(print row)
      t)))

