;; short-hand macros
(defmacro if-not (pred then else)
  `(if ,pred
     ,else
     ,then))

(defmacro when-not (pred &rest then)
  `(when (not ,pred)
     ,@then))

(defmacro probabilistic-if (prob then else)
  `(if (prob ,prob)
     ,then
     ,else))

(defmacro prob (prob)
  `(< (random 1.0) ,prob))

;; list operations
(defmethod take ((n integer) (x list))
  (when (and x (> n 0))
    (cons (car x)
          (take (1- n)
                (cdr x)))))

(defmacro drop (n x)
  `(nthcdr ,n ,x))

(defmethod partition ((coll list)
                      &optional
                      (size nil) 
                      (step nil))
  (let* ((size (or size
                   (length coll)))
         (step (or step
                   size)))
    (partition-iter coll size step)))

(defmethod partition-iter ((coll list)
                           (size integer)
                           (step integer))
  (when (and coll
             (<= size
                 (length coll)))
    (cons (take size coll)
          (partition-iter (drop step coll)
                          size
                          step))))

(defmethod partition-all ((coll list)
                          &optional
                          (size nil) 
                          (step nil))
  (let* ((size (or size
                   (length coll)))
         (step (or step
                   size)))
    (partition-all-iter coll size step)))

(defmethod partition-all-iter ((coll list)
                               (size integer)
                               (step integer))
  (when coll
    (cons (take size coll)
          (partition-all-iter (drop step coll)
                              size
                              step))))

(defmethod split ((index integer)
                  (coll  list))
  (list (take index coll)
        (drop index coll)))

(defmethod range (&key (min 0) (max 0) (step 1))
  (loop for n from min below max by step
     collect n))

(defmethod repeat ((n integer) x)
  (when (> n 0)
    (cons x
          (repeat (1- n)
                  x))))

(defmethod repeatedly ((n integer)
                       (f function))
  (when (> n 0)
    (cons (funcall f)
          (repeatedly (1- n)
                      f))))

;; randomness functions
(defun random-normal (&key (mean 0.0) (std 1.0))
  (+ mean
     (* (sqrt (* -2 (log (random 1.0))))
        (cos (* 2 pi (random 1.0)))
        std)))

(defmethod random-permutation ((x number)
                               &optional
                               (range 2.0))
  (+ x
     (- (random range)
        (/ range 2))))

(defmethod random-permutations ((x list)
                                &optional
                                (range 2.0)
                                (prob  0.20))
  (mapcar (lambda (i)
            (probabilistic-if prob
              (random-permutation i range)
              i))
          x))

(defmethod n-random-permutations ((n integer)
                                  (x number)
                                  &optional
                                  (range 2.0))

  (repeatedly n (lambda () (random-permutation x range))))

(defmethod select-randomly ((n    integer)
                            (coll list))
  (select-randomly-iter n
                        (length coll)
                        coll))

(defmethod select-randomly-iter ((n    integer)
                                 (len  integer)
                                 (coll list))
  (when coll
    ;; has n/len probability of selecting (car coll)
    (probabilistic-if (/ n len)
      (cons (car coll)
            (select-randomly-iter (1- n)
                                  (1- len)
                                  (cdr coll)))
      (select-randomly-iter n
                            (1- len)
                            (cdr coll)))))

(defmacro eval-random (&rest body)
  (let* ((num (length body))
         (idx (random num))
         (choice (nth idx body)))
    choice))

;; math functions
(defmethod abs- ((n number))
  "Take the negation of the absolute value of n"
  (- (abs n)))

(defmethod mean ((x list))
  (/ (apply #'+ x)
     (length    x)))

(defun stdev (list)
  (let ((mean (mean list))
        (N    (length list)))
    (sqrt (* (/ N)
             (apply #'+
                    (mapcar #'square
                            (mapcar (lambda (x) (- x mean))
                                    list)))))))

(defmethod square ((n number))
  (expt n 2))

(defmethod sum-of-squares ((x list))
  (apply #'+
         (mapcar #'square
                 x)))

(defmethod L1-norm ((a list) (b list))
  (apply #'+ (mapcar (lambda (x y) (abs (- x y))) a b)))

(defmethod L2-norm ((a list) (b list))
  (sum-of-squares (mapcar #'- a b)))

(defmethod weighted-Lp-norm ((a list) (b list) (p real) (weights list))
  (expt (apply #'+
               (mapcar (lambda (x y w)
                         (expt (abs (- x y))
                               p))
                       a b weights))
        (/ p)))

(defmethod weighted-L1-norm ((a list) (b list) (weights list))
  (weighted-Lp-norm a b 1 weights))

(defmethod weighted-L2-norm ((a list) (b list) (weights list))
  (weighted-Lp-norm a b 2 weights))
