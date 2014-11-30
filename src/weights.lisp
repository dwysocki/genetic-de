(load "util.lisp")

(defun discrete-squared-weight ((y list) (min real))
  (let* ((N (length y))
         (indices (range :min 1 :max (1+ N)))
         (c (/ (- 1 (* N min))
               (apply #'+
                        (mapcar (lambda (i) (expt (- i N) 2))
                                indices))))
         (weight-fn (lambda (i) (+ min (* c (expt (- i N) 2)))))
         (weights (mapcar weight-fn indices)))
    weights))

(defun unit-step-weight ((N integer))
  (let* ((w_0 0.5)
         (w_i (/ 0.5
                 (1- N))))
    (cons w_0
          (repeat (1- N)
                  w_i))))

(defun linear-decrease-weight ((N integer))
  (mapcar (lambda (i) (- (1+ N) i))
          (range :min 1 :max (1+ N))))

(defun uniform-weight ((N integer))
  (repeat N 1))
