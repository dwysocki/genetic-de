(load "weights.lisp")
(load "util.lisp")

(defconstant *population-size*      100)
(defconstant *sample-size*          8)
(defconstant *proportion-mutate*    0.50)
(defconstant *proportion-copy*      0.40)
(defconstant *proportion-crossover* 0.60)
(defconstant *gaussian-sigma*       0.1)

(setf *base-error-function* #'weighted-L1-norm)
(defvar *error-function*)

(defclass equation ()
;; f(x, y) = rhs
  ((rhs :reader equation-rhs :initarg :rhs :initform #'identity)
   (N   :reader equation-N   :initarg :N   :initform 100     )))

(defclass ode (equation)
  ((y0 :reader equation-y0 :initarg :y0 :initform 0)
   (x0 :reader equation-x0 :initarg :x0 :initform 0)
   (xN :reader equation-xN :initarg :xN :initform 1)
   (xs :reader equation-xs :initarg :xs :initform (range :min  0
                                                         :max  1
                                                         :step 0.01))))
(defclass 1st-order-ode (ode) ())

(defclass solution ()
  ((fitness :accessor solution-fitness :initarg :fitness)))
(defclass           ode-solution (    solution)
  ((y   :reader solution-y   :initarg :y  )))
(defclass 1st-order-ode-solution (ode-solution)
  ((y-  :reader solution-y-  :initarg :y- )))

(defclass population ()
  ((generation :reader population-generation :initarg :generation)
   (equation   :reader population-equation   :initarg :equation)
   (solutions  :reader population-solutions  :initarg :solutions)))

;; DISPLAY METHODS ;;
(defmethod display ((s 1st-order-ode-solution)
                    &key (output t))
  (format output
          "y(x)  = ~A~%y'(x) = ~A~%fitness = ~D"
          (solution-y       s)
          (solution-y-      s)
          (solution-fitness s)))

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
  (let ((xs (range :min x0
                   :max xN
                   :step (/ (- xN x0)
                            N))))
    (make-instance '1st-order-ode
      :rhs rhs
      :y0  y0
      :x0  x0
      :xN  xN
      :xs  xs
      :N   N)))

(defmethod step-size ((e ode))
  (with-slots (x0 xN N) e
    (float (/ (- xN x0)
              N))))

(defmethod 1st-order-ode-fitness ((x  list)
                                  (y  list)
                                  (y- list)
                                  (fn function))
  (- (funcall *error-function* y- (mapcar fn x y))))

(defmethod fitness ((s 1st-order-ode-solution)
                    (e ode))
  (with-slots (xs rhs) e
    (with-slots (y y-) s
      (1st-order-ode-fitness xs y y- rhs))))

(defmethod initial-y ((y_0 real) (xs list) (dx real) (f function))
  (cons y_0
        (initial-y-iter y_0
                        xs
                        dx
                        f)))

(defmethod initial-y-iter ((y_i real) (xs list) (dx real) (f function))
  (when (cdr xs) ;; list has next value
    (let* ((dy/dx (float (funcall f (car xs)
                                  y_i)))
           (dy (* dy/dx dx))
           (y_i+1 (cond
                    ((> dy/dx 0) (+ y_i (random    dy)))
                    ((< dy/dx 0) (- y_i (random (- dy))))
                    ((= dy/dx 0) y_i))))
      (cons y_i+1
            (initial-y-iter y_i+1
                            (cdr xs)
                            dx
                            f)))))

(defmethod random-solution ((e 1st-order-ode))
  (with-slots (rhs xs y0 N) e
    (let* ((dx (step-size e))
           (y (initial-y y0 xs dx rhs))
           ;; (y  (cons y0
           ;;           (n-random-permutations (1- N)
           ;;                                  y0)))
           (y- (diff-all y dx))
           (f  (1st-order-ode-fitness xs
                                      y
                                      y-
                                      rhs)))
      (make-instance '1st-order-ode-solution
        :y       y
        :y-      y-
        :fitness f))))

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

(defmethod possibly-mutate ((s solution)
                            (e equation)
                            (prob real))
  "Mutates solution with the given probability."
  (probabilistic-if prob
    (mutate s e)
    s))


(defmethod mutate ((s 1st-order-ode-solution)
                   (e 1st-order-ode))
  (with-slots (rhs xs N) e
    (with-slots (y) s
      (let* (;; select index to mutate
             ;; index cannot be first element
             (i (1+ (random (1- N))))
             (dy (random-normal :std *gaussian-sigma*))
             (front-y (take i y))
             (back-y  (mapcar (lambda (y) (+ dy y))
                              (drop i y)))
             (new-y (append front-y back-y))
             (new-y- (diff-all new-y (step-size e)))
             (new-fitness (1st-order-ode-fitness xs new-y new-y- rhs)))
        (make-instance '1st-order-ode-solution
          :y       new-y
          :y-      new-y-
          :fitness new-fitness)))))

(defmethod crossover ((m 1st-order-ode-solution)
                      (f 1st-order-ode-solution)
                      (e 1st-order-ode))
  (with-slots (rhs xs N) e
    (let* ((dx (step-size e))
           (i (+ (random (- N 3))
                 2))
           (front-y (take i (solution-y m)))
           ;; one of the ugliest lines of code I've written in a while
           ;; finds the difference between the last y value taken from the
           ;; mother, and the mother's y value which succeeds it
           (desired-division-dy (- (apply #'-
                                          (take 2
                                                (drop (1- i)
                                                      (solution-y m))))))
           (back-y (drop i (solution-y f)))
           (actual-division-dy (- (car back-y)
                                  (car (last front-y))))
           (delta-dy (- actual-division-dy desired-division-dy))
           (shifted-back-y (mapcar (lambda (y) (+ y delta-dy))
                                   back-y))
           (new-y (append front-y shifted-back-y))
           (new-y- (diff-all new-y dx))
           (new-fitness (1st-order-ode-fitness xs new-y new-y- rhs)))
      (make-instance '1st-order-ode-solution
        :y       new-y
        :y-      new-y-
        :fitness new-fitness))))


(defmethod init-error-function ((N integer))
  "Initializes the error function to use uniform weight."
  (let* ((weights (uniform-weight N))
         (normed-weights (normalize weights)))
    (setf *error-function*
          (lambda (a b)
            (funcall *base-error-function*
                     a b normed-weights)))))

(defmethod random-population ((e equation)
                              &optional
                              (size *population-size*))
  (init-error-function (1- (equation-N e)))
  (let ((initial-solutions (repeatedly size
                                       (lambda ()
                                         (random-solution e)))))
    (make-instance 'population
      :generation 0
      :equation   e
      :solutions  initial-solutions)))

(defmethod most-fit ((p population))
  "Select the most fit solution from a population"
  (most-fit (population-solutions p)))

(defmethod most-fit ((solutions list))
  "Select the most fit solution from a list of solutions"
  (reduce (lambda (leader competer)
            (if (> (solution-fitness leader)
                   (solution-fitness competer))
              leader
              competer))
          solutions))

(defmethod mean-fitness ((p population))
  (mean (mapcar #'solution-fitness (population-solutions p))))

(defmethod stdev-fitness ((p population))
  (stdev (mapcar #'solution-fitness (population-solutions p))))

(defmethod sample ((p population)
                   &optional
                   (sample-size *sample-size*))
  "Take a sample of the given size from the population"
  (select-randomly sample-size
                   (population-solutions p)))

(defmethod select-individual ((p population)
                              (sample-size integer))
  "Select the most fit individual from a sample of the population"
  (let* ((sampled-solutions (sample p sample-size)))
    (most-fit sampled-solutions)))

(defmethod make-copy ((p                 population)
                      (sample-size       integer)
                      (proportion-mutate real))
  "Make a single copy from the population"
  (possibly-mutate (select-individual p sample-size)
                   (population-equation p)
                   proportion-mutate))

(defmethod make-copies ((n                 integer)
                        (p                 population)
                        (sample-size       integer)
                        (proportion-mutate real))
  "Make n copies from the population"
  (when (> n 0)
    (cons (make-copy p sample-size proportion-mutate)
          (make-copies (1- n) p sample-size proportion-mutate))))

(defmethod make-crossover ((p population)
                           (sample-size integer))
  "Make a single crossover from the population"
  (let ((mother (select-individual p sample-size))
        (father (select-individual p sample-size)))
    (if (equal mother father)
      ;; asexual reproduction is not allowed. try again
      (make-crossover p sample-size)
      (crossover mother father
                 (population-equation p)))))

(defmethod make-crossovers ((n integer)
                            (p population)
                            (sample-size integer))
  "Make n crossovers from the population"
  (when (> n 0)
    (cons (make-crossover p sample-size)
          (make-crossovers (1- n) p sample-size))))

(defmethod next-population ((current-population population)
                            &key
                            (sample-size          *sample-size*         )
                            (proportion-mutate    *proportion-mutate*   )
                            (proportion-copy      *proportion-copy*     )
                            (proportion-crossover *proportion-crossover*))
  "Make the given number of copies and crossovers from the population and
  create a new population with them"
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
