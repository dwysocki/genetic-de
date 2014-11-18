
;; number of evenly spaced points to evaluate y(x) at
(defconstant *num-points* 100)

(defclass equation ()
  ((a        :reader equation-a    :initarg :a)
   (b        :reader equation-b    :initarg :b)
   (c        :reader equation-c    :initarg :c)
   (init-val :reader equation-init :initarg :init-val)
   (min      :reader equation-min  :initarg :min)
   (max      :reader equation-max  :initarg :max)))

(defclass solution ()
  ((c1 :reader solution-c1 :initarg :c1)
   (c2 :reader solution-c2 :initarg :c2)
   (r1 :reader solution-r1 :initarg :r1)
   (r2 :reader solution-r2 :initarg :r2)))

(defclass real-distinct-solution (solution))

(defclass real-repeated-solution (solution))

(defclass       complex-solution (solution))

(defmethod solution-y ((s real-distinct-solution))
  "y(x) = c_1 \\exp(r_1 x) + c_2 \\exp(r_2 x)"
  (let ((c1 (solution-c1 s))
        (c2 (solution-c2 s))
        (r1 (solution-r1 s))
        (r2 (solution-r2 s)))
    (lambda (x)
      (+ (* c1 (exp (* r1 x)))
         (* c2 (exp (* r2 x)))))))

(defmethod solution-y-prime ((s real-distinct-solution))
  "y'(x) = c_1 r_1 \\exp(r_1 x) + c_2 r_2 \\exp(r_2 x)"
  (let ((c1 (solution-c1 s))
        (c2 (solution-c2 s))
        (r1 (solution-r1 s))
        (r2 (solution-r2 s)))
    (lambda (x)
      (+ (* c1 r1 (exp (* r1 x)))
         (* c2 r2 (exp (* r2 x)))))))

(defmethod solution-y-prime-prime ((s real-distinct-solution))
  "y''(x) = c_1 r_1 \\exp(r_1 x) + c_2 r_2 \\exp(r_2 x)"
  (let ((c1 (solution-c1 s))
        (c2 (solution-c2 s))
        (r1 (solution-r1 s))
        (r2 (solution-r2 s)))
    (lambda (x)
      (+ (* c1 r1 r1 (exp (* r1 x)))
         (* c2 r2 r2 (exp (* r2 x)))))))

(defmethod solution-y ((s real-repeated-solution))
  "y(x) = c_1 \\exp(r x) + c_2 x \\exp(r x)"
  (let ((c1 (solution-c1 s))
        (c2 (solution-c2 s))
        (r  (solution-r1 s)))
    (lambda (x)
      (* (exp (* r x))
         (+ c1 (* c2 x))))))

(defmethod solution-y-prime ((s real-repeated-solution))
  "y'(x) = c_1 r \\exp(r x) + c_2 \\left( \\exp(r x) + x r \\exp(r x) \\right)"
  (let ((c1 (solution-c1 s))
        (c2 (solution-c2 s))
        (r  (solution-r1 s)))
    (lambda (x)
      (* (exp (* r x))
         (+ (* c1 r)
            (* c2 (1+ (* x r))))))))

(defmethod solution-y-prime-prime ((s real-repeated-solution))
  "y''(x) = r \\exp(r x) \\left( c_1 r + c_2 (2 + r x) \\right)"
  (let ((c1 (solution-c1 s))
        (c2 (solution-c2 s))
        (r  (solution-r1 s)))
    (lambda (x)
      (let ((rx (* r x)))
        (* r (exp rx)
           (+ (* c1 r)
              (* c2 (+ 2 rx))))))))

(defmethod solution-y ((s complex-solution))
  "y(x) = \\exp(\\alpha x) \\left(
              c_1 \\cos(\\beta x) + c_2 \\sin(\\beta x)
          \\right)"
  (let ((c1 (solution-c1 s))
        (c2 (solution-c2 s))
        (A  (solution-r1 s))
        (B  (solution-r2 s)))
    (lambda (x)
      (* (exp (* A x))
         (+ (* c1 (cos (* B x)))
            (* c2 (sin (* B x))))))))

(defmethod solution-y-prime ((s complex-solution))
  "y(x) = \\exp(\\alpha x) \\left(
              \\alpha \\left( c_1 \\cos(\\beta x) + c_2 \\sin(\\beta x) \\right)
            + \\beta  \\left( c_2 \\cos(\\beta x) - c_1 \\sin(\\beta x) \\right)
          \\right)"
  (let ((c1 (solution-c1 s))
        (c2 (solution-c2 s))
        (A  (solution-r1 s))
        (B  (solution-r2 s)))
    (lambda (x)
      (* (exp (* A x))
         (+ (* c1 (cos (* B x)))
            (* c2 (sin (* B x))))))))
