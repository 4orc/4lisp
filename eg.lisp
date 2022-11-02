(load "eye")

(eg hi () 
  (format t "~&Welcome to keys~%") t)

(eg rndi()
    (let (lst) 
      (print (dotimes (i 100 (sort lst #'<)) 
               (push (randi 10) lst)))))

(eg rndf()
    (let (lst) 
      (print (dotimes (i 100 (sort lst #'<)) 
               (push (rnd (randf 10)) lst)))))

(eg pts() 
    (print *opts*))

(eg sml()
    (let ((s (make-sample 32)))
      (dotimes (i 10000)  (add s i))
      (print (sorted s))))

; (defun sym (the)
;   (let ((n (add (make-sym) '("a" "b" "b" "c" "c" "c" "c"))))
;     (want (< 1.378 (var n) 1.379) "bad ent")))
;
; (defun num (the)
;   (let ((n (add (init(make-num  :txt "asd-"))
;                 '(2 3 4 4 4 4  5  5  6  7 
;                   7 8 9 9 9 9 10 11 12 12))))
;   (want (= 3.125 (var n)) "bad sd")
;  (want (= 7     (mid n)) "bad mean")))

(cli)
(run)
