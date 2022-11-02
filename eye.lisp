(load "etc")

(opt "-h" "TAR3: recursive sampler
           (c) 2022 Tim Menzies <timm@ieee.org> BSD-2" 'help  nil)
(opt "-g" "start-up action; all= run all" 'go   "none")
(opt "-p" "whatever floats your boat    " 'p    2)
(opt "-s" "random number seed           " 'seed 10013)
(opt "-S" "keep at most 'S' nums        " 'samples 256)

(defstruct+ sample 
  (n 0) (max (is samples)) ok
  (_all  (make-array 2 :fill-pointer 0 :adjustable t)))

(defun make-sample (&optional (max (is samples)))
  (%make-sample :max max))

(defmethod add ((i  sample) x )
  (with-slots (n _all max ok) i
    (incf n)
    (let ((size (length  _all)))
      (cond ((< size  max)
             (setf ok nil)
             (vector-push-extend x  _all))
            ((< (randf) (/ max  n))
             (setf  ok nil)
             (setf (elt _all (randi size)) x))))))

(defmethod per ((i sample) p)
  (let* ((all (sorted i))
         (n  (1- (length all))))
    (elt all (max 0 (min n (floor (* p n)))))))

(defmethod mid ((i sample))
  (per i .5))

(defmethod div ((i sample))
  (/ (- (per i .9) (per i .1)) 2.58))

(defmethod sorted ((i sample))
  (unless (? i ok)
    (sort (? i _all) #'<)
    (setf (? i ok) t))
  (? i _all))

