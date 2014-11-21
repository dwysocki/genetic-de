(load "util.lisp")

(defconstant *population-size*      30)
(defconstant *sample-size*          5)
(defconstant *proportion-mutate*    0.05)
(defconstant *proportion-copy*      0.90)
(defconstant *proportion-crossover* 0.10)

(defclass equation ()
;; f(x) = rhs
  ((rhs :reader equation-rhs :initarg :rhs :initform #'identity)
   (N   :reader equation-N   :initarg :N   :initform 100     )))

(defclass ode (equation)
  ((y0  :reader equation-y0  :initarg :y0  :initform 0)
   (x0  :reader equation-x0  :initarg :x0  :initform 0)
   (xN  :reader equation-xN  :initarg :xN  :initform 1)
   (val :reader equation-val :initarg :val :initform (range :min  0
                                                            :max  1
                                                            :step 0.01))))
(defclass 1st-order-ode (ode) ())
(defclass 2nd-order-ode (ode) ())

(defclass solution ()
  ((fitness :reader solution-fitness :initarg :fitness)))
(defclass           ode-solution (    solution)
  ((y   :reader solution-y   :initarg :y  )))
(defclass 1st-order-ode-solution (ode-solution)
  ((y-  :reader solution-y-  :initarg :y- )))
(defclass 2nd-order-ode-solution (ode-solution)
  ((y-- :reader solution-y-- :initarg :y--)))

(defclass population ()
  ((generation :reader population-generation :initarg :generation)
   (equation   :reader population-equation   :initarg :equation)
   (solutions  :reader population-solutions  :initarg :solutions)))

;; DISPLAY METHODS ;;
(defmethod display ((eqn ode)
                    &key (output t))
  (with-slots (y0 x0 xN) eqn
    (format output
            "f(y, x); y(~D) = ~D; ~D <= x <= ~D"
            x0
            y0
            x0
            xN)))

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

(defmethod display ((p population)
                    &key (output t))
  (format output
          "Generation ~D population...~%~{~A~^~%~}"
          (population-generation p)
          (mapcar (lambda (x) (display x :output nil))
                  (population-solutions p))))

(defmethod make-1st-order-ode ((rhs function)
                               (y0  real)
                               (x0  real)
                               (xN  real)
                               (N   integer))
  (let* ((x (range :min x0
                   :max xN
                   :step (/ (- xN x0)
                            N)))
         (val (mapcar rhs x)))
    (make-instance '1st-order-ode
      :rhs rhs
      :y0  y0
      :x0  x0
      :xN  xN
      :N   N
      :val val)))

(defmethod step-size ((e ode))
  (with-slots (x0 xN N) e
    (/ (- xN x0)
       N)))

(defmethod fitness ((vals list)
                    (e    ode))
  (- (sum-of-squares-difference vals
                                (equation-val e))))

(defmethod fitness ((s 1st-order-ode-solution)
                    (e ode))
  (fitness (solution-y- s) e))

(defmethod random-solution ((e 1st-order-ode))
  (with-slots (y0 N) e
    (let* ((y  (cons y0 (n-random-permutations (1- N) y0)))
           (y- (diff-all y (step-size e)))
           (fitness (fitness y- e)))
      (make-instance '1st-order-ode-solution
         :y       y
         :y-      y-
         :fitness fitness))))

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

(defmethod possibly-mutate ((s solution)
                            (e equation)
                            (prob real))
  "Mutates solution with the given probability."
  (probabilistic-if prob
    (mutate s e)
    s))

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

(defmethod random-population ((e equation)
                              &optional
                              (size *population-size*))
  (let ((initial-solutions (repeatedly size
                                       (lambda ()
                                         (random-solution e)))))
    (make-instance 'population
      :generation 0
      :equation   e
      :solutions  initial-solutions)))

(defmethod most-fit ((solutions list))
  (reduce (lambda (leader competer)
            (if (> (solution-fitness leader)
                   (solution-fitness competer))
              leader
              competer))
          solutions))

(defmethod sample ((p population)
                   &optional
                   (sample-size *sample-size*))
  (select-randomly sample-size
                   (population-solutions p)))

(defmethod select-individual ((p population)
                              (sample-size integer))
  (let* ((sampled-solutions (sample p sample-size)))
    (most-fit sampled-solutions)))

(defmethod make-copy ((p                 population)
                      (sample-size       integer)
                      (proportion-mutate real))
  (possibly-mutate (select-individual p sample-size)
                   (population-equation p)
                   proportion-mutate))

(defmethod make-copies ((n                 integer)
                        (p                 population)
                        (sample-size       integer)
                        (proportion-mutate real))
  (when (> n 0)
    (cons (make-copy p sample-size proportion-mutate)
          (make-copies (1- n) p sample-size proportion-mutate))))

(defmethod make-crossover ((p population)
                           (sample-size integer))
  (let ((mother (select-individual p sample-size))
        (father (select-individual p sample-size)))
    (crossover mother father
               (population-equation p))))

(defmethod make-crossovers ((n integer)
                            (p population)
                            (sample-size integer))
  (when (> n 0)
    (cons (make-crossover p sample-size)
          (make-crossovers (1- n) p sample-size))))

(defmethod next-population ((current-population population)
                            &key
                            (sample-size          *sample-size*         )
                            (proportion-mutate    *proportion-mutate*   )
                            (proportion-copy      *proportion-copy*     )
                            (proportion-crossover *proportion-crossover*))
  (with-slots (generation equation solutions) current-population
    (let* ((size          (length solutions))
           (n-copies      (round (* size proportion-copy)))
           (n-crossovers  (- size n-copies))
           (copies        (make-copies n-copies current-population
                                       sample-size
                                       proportion-mutate))
           (crossovers    (make-crossovers n-crossovers current-population
                                           sample-size))
           (new-solutions (append copies crossovers)))
      (make-instance 'population
        :generation (1+ generation)
        :equation   equation
        :solutions  new-solutions))))
