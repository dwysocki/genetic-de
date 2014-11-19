
;; number of evenly spaced points to evaluate y(x) at
(defconstant *num-points* 100)

(defclass equation ()
  ((a        :reader equation-a    :initarg :a        :initform 0)
   (b        :reader equation-b    :initarg :b        :initform 0)
   (c        :reader equation-c    :initarg :c        :initform 0)
   (init-val :reader equation-init :initarg :init-val :initform 0)
   (min      :reader equation-min  :initarg :min      :initform 0)
   (max      :reader equation-max  :initarg :max      :initform 1)))

(defmethod display ((e equation)
                    &optional (dest t))
  (format dest
          "~Dy'' + ~Dy' + ~Dy = 0; y(0) = ~D; ~D <= x <= ~D~%"
          (equation-a    e) (equation-b   e) (equation-c e)
          (equation-init e)
          (equation-min  e) (equation-max e)))

(defclass solution ()
  ((c1 :reader solution-c1 :initarg :c1)
   (c2 :reader solution-c2 :initarg :c2)
   (r1 :reader solution-r1 :initarg :r1)
   (r2 :reader solution-r2 :initarg :r2)))

(defclass real-distinct-solution (solution) ())

(defmethod display ((s real-distinct-solution)
                    &optional (dest t))
  (format dest
          "y(x) = ~D⋅exp(~D⋅x) + ~D⋅exp(~D⋅x)~%"
          (solution-c1 s) (solution-r1 s)
          (solution-c2 s) (solution-r2 s)))

(defclass real-repeated-solution (solution) ())

(defmethod solution-r ((s real-repeated-solution))
  (let ((s1 (solution-r1 s))
        (s2 (solution-r2 s)))
    (when (= s1 s2)
      s1)))

(defmethod display ((s real-repeated-solution)
                    &optional (dest t))
  (format dest
          "y(x) = ~D⋅exp(~D⋅x) + ~D⋅x⋅exp(~D⋅x)~%"
          (solution-c1 s) (solution-r s)
          (solution-c2 s) (solution-r s)))

(defclass       complex-solution (solution) ())

(defmethod solution-A ((s complex-solution))
  (solution-r1 s))

(defmethod solution-B ((s complex-solution))
  (solution-r2 s))

(defmethod display ((s complex-solution)
                    &optional (dest t))
  (format dest
          "y(x) = exp(~D⋅x) (~D⋅cos(~D⋅x) + ~D⋅sin(~D⋅x))~%"
          (solution-A  s)
          (solution-c1 s) (solution-B s)
          (solution-c2 s) (solution-B s)))

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
      (let ((Ax (* A x))
            (Bx (* B x)))
        (* (exp Ax)
           (+ (* A
                 (+ (* c1 (cos Bx))
                    (* c2 (sin Bx))))
              (* B
                 (- (* c2 (cos Bx))
                    (* c1 (sin Bx))))))))))

(defmethod solution-y-prime-prime ((s complex-solution))
  "y(x) = \\exp(\\alpha x) \\left(
              \\alpha \\left( c_1 \\cos(\\beta x) + c_2 \\sin(\\beta x) \\right)
            + \\beta  \\left( c_2 \\cos(\\beta x) - c_1 \\sin(\\beta x) \\right)
          \\right)"
  (let ((c1 (solution-c1 s))
        (c2 (solution-c2 s))
        (A  (solution-r1 s))
        (B  (solution-r2 s)))
    (lambda (x)
      (let ((Ax (* A x))
            (Bx (* B x)))
        (* (exp Ax)
           (- (* A A
                 (+ (* c1 (cos Bx))
                    (* c2 (sin Bx))))
              (* 2 A B
                 (- (* c1 (sin Bx))
                    (* c2 (cos Bx))))
              (* B B
                 (+ (* c1 (cos Bx))
                    (* c2 (sin Bx))))))))))
