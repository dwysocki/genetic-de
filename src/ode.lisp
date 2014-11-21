(load "util.lisp")

(defclass equation ()
;; f(x) = rhs
  ((rhs :reader equation-rhs :initarg :rhs :initform #'identity)
   (N   :reader equation-N   :initarg :N   :initform 100     )))

(defclass ode (equation)
  ((y0  :reader equation-y0 :initarg :y0 :initform 0)
   (x0  :reader equation-x0 :initarg :x0 :initform 0)
   (xN  :reader equation-xN :initarg :xN :initform 1)))
(defclass 1st-order-ode (ode) ())
(defclass 2nd-order-ode (ode) ())

(defclass solution () ())
(defclass           ode-solution (    solution)
  ((y   :reader solution-y   :initarg :y  )))
(defclass 1st-order-ode-solution (ode-solution)
  ((y-  :reader solution-y-  :initarg :y- )))
(defclass 2nd-order-ode-solution (ode-solution)
  ((y-- :reader solution-y-- :initarg :y--)))

(defmethod step-size ((e ode))
  (with-slots (x0 xN N) e
    (/ (- xN x0)
       N)))

(defmethod random-solution ((e 1st-order-ode))
  (with-slots (y0 N) e
    (let* ((y  (cons y0 (n-random-permutations (1- N) y0)))
           (y- (diff-all y (step-size e))))
      (make-instance '1st-order-ode-solution
         :y  y
         :y- y-))))

(defmethod display ((eqn ode)
                    &key (output t))
  (with-slots (x0 y0 xN) eqn
    (format output
            "f(y, x); y(~D) = ~D; ~D <= x <= ~D"
                        x0    y0  x0         xN)))

(defmethod display ((eqn 1st-order-ode)
                    &key (output t))
  (format output
          "dy/dx = ~A"
          (display (coerce eqn ode)
                   :output nil)))

(defmethod display ((eqn 2nd-order-ode)
                    &key (output t))
  (format output
          "d^2y/dx^2 = ~A"
          (display (coerce eqn ode)
                   :output nil)))

(defmethod display ((s 1st-order-ode-solution)
                    &key (output t))
  (format output
          "y(x)  = ~A~%y'(x) = ~A"
          (solution-y  s)
          (solution-y- s)))

(defmethod diff ((pair list)
                 (h    real))
  (let* ((y1 (first  pair))
         (y2 (second pair)))
    (/ (- y2 y1)
       h)))

(defmethod diff-all ((y list)
                     (h real))
  (mapcar (lambda (pair)
            (diff pair h))
          (partition y 2 1)))

(defmethod diff2-all ((y list)
                      (h real))
  (diff-all (diff-all y h)
            h))

(defmethod mutate ((s 1st-order-ode-solution)
                   (e ode))
  (let ((h (step-size e)))
    (permute-random s h)))

(defmethod permute-random ((s 1st-order-ode-solution)
                           (h real))
  (with-slots (y y-) s
    (let* ((i              (1+ (random (1- (length y)))))
           ;; split y at the element to be permuted
           (first-half-y   (take i y))
           (second-half-y  (drop i y))
           ;; permute the first element of the second half of y
           (mutated-y      (random-permutation (car second-half-y)))
           ;; 
           (new-y          (append first-half-y
                                   (list mutated-y)
                                   (cdr second-half-y)))
           (first-half-y-  (take (1- i) y-))
           (second-half-y- (drop  i     y-))
           (mutated-y-     (diff (list (car (last first-half-y))
                                       mutated-y)
                                 h))
           (new-y-         (append first-half-y-
                                   (list mutated-y-)
                                   second-half-y-)))
      (make-instance '1st-order-ode-solution
        :y  new-y
        :y- new-y-))))

(defmethod crossover ((m 1st-order-ode-solution)
                      (f 1st-order-ode-solution)
                      (e ode))
  (let* ((i      (1+ (random (- (length (solution-y m))
                                2))))
         (m-y    (take i      (solution-y  m)))
         (f-y    (drop i      (solution-y  f)))
         (m-y-   (take (1- i) (solution-y- m)))
         (f-y-   (drop i      (solution-y- f)))
         (mid-y- (diff (list (car (last m-y))
                             (car f-y))
                       (step-size e)))
         (new-y  (append m-y
                         f-y))
         (new-y- (append m-y-
                         (list mid-y-)
                         f-y-)))
    (make-instance '1st-order-ode-solution
       :y  new-y
       :y- new-y-)))
