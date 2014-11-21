;; short-hand macros
(defmacro if-not (pred then else)
  `(if ,pred
     ,else
     ,then))

(defmacro when-not (pred &rest then)
  `(when (not ,pred)
     ,@then))

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

(defmethod repeatedly ((n integer)
                       (f function))
  (when (> n 0)
    (cons (funcall f)
          (repeatedly (1- n)
                      f))))

;; randomness functions
(defmethod random-permutation ((x number)
                               &optional
                               (range 2.0))
  (+ x
     (- (random range)
        (/ range 2))))

(defmethod n-random-permutations ((n integer)
                                  (x number)
                                  &optional
                                  (range 2.0))

  (repeatedly n (lambda () (random-permutation x range))))

(defmacro eval-random (&rest body)
  (let* ((num (length body))
         (idx (random num))
         (choice (nth idx body)))
    choice))

;; math functions
(defmethod square ((n number))
  (expt n 2))

(defmethod sum-of-squares ((x list))
  (apply #'+
         (mapcar #'square
                 x)))