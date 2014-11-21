(load "util.lisp")

(defconstant *tolerance* 1e-5)
(defconstant *step-size* 0.1)

(defmethod almost= ((x number) (y number))
  (< (abs (- x y))
     *tolerance*))

(defclass equation ()
  ((a        :reader equation-a        :initarg :a        :initform 0)
   (b        :reader equation-b        :initarg :b        :initform 0)
   (c        :reader equation-c        :initarg :c        :initform 0)
   (y0       :reader equation-y0       :initarg :y0       :initform 0)
   (y0-prime :reader equation-y0-prime :initarg :y0-prime :initform 0)
   (min      :reader equation-min      :initarg :min      :initform 0)
   (max      :reader equation-max      :initarg :max      :initform 1)))

(defmethod display ((e equation)
                    &optional (dest t))
  (format dest
          "~Dy'' + ~Dy' + ~Dy = 0; y(0) = ~D; y'(0) = ~D; ~D <= x <= ~D~%"
          (equation-a    e) (equation-b        e) (equation-c e)
          (equation-y0   e) (equation-y0-prime e)
          (equation-min  e) (equation-max      e)))

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

(defmethod make-duplicate ((s real-distinct-solution)
                           &key c1 c2 r1 r2)
  (make-instance 'real-distinct-solution
     :c1 (or c1 (solution-c1 s))
     :c2 (or c2 (solution-c2 s))
     :r1 (or r1 (solution-r1 s))
     :r2 (or r2 (solution-r2 s))))


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

(defmethod solution-equation ((s solution)
                              (e equation))
  (lambda (x)
    (+ (* (equation-c e)
          (funcall (solution-y s) x))
       (* (equation-b e)
          (funcall (solution-y-prime s) x))
       (* (equation-a e)
          (funcall (solution-y-prime-prime s) x)))))

(defmethod solution-error ((s solution)
                           (e equation)
                           &optional (step *step-size*))
  (sum-of-squares (mapcar (solution-equation s e)
                          (range :min  (equation-min e)
                                 :max  (equation-max e)
                                 :step *step-size*))))

(defmethod get-c1 ((c2 number)
                   (s  real-distinct-solution)
                   (e  equation))
  (let ((r1 (solution-r1 s))
        (r2 (solution-r2 s))
        (y0 (equation-y0 e))
        (y0-prime (equation-y0-prime e)))
   (/ (- y0-prime (* y0 r2))
      (- r1 r2))))

(defmethod get-c2 ((c1 number)
                   (s  real-distinct-solution)
                   (e  equation))
  (let ((r1 (solution-r1 s))
        (r2 (solution-r2 s))
        (y0 (equation-y0 e))
        (y0-prime (equation-y0-prime e)))
   (/ (- (* y0 r1) y0-prime)
      (- r1 r2))))

(defmethod mutate-coefficient ((c number))
  (let ((factor (+ 0.5 (random 1.0))))
    (* c factor)))

(defmethod mutate ((s real-distinct-solution)
                   (e equation))
  (eval-random
    (mutate-c1 s e)
    (mutate-c2 s e)))

(defmethod mutate-c1 ((s real-distinct-solution)
                      (e equation))
  (let* ((c1-old (solution-c1 s))
         (c1-new (mutate-coefficient c1-old))
         (c2-new (get-c2 c1-new s e)))
    (make-duplicate s :c1 c1-new :c2 c2-new)))

(defmethod mutate-c2 ((s real-distinct-solution)
                      (e equation))
  (let* ((c2-old (solution-c1 s))
         (c2-new (mutate-coefficient c2-old))
         (c1-new (get-c1 c2-new s e)))
    (make-duplicate s :c1 c1-new :c2 c2-new)))


(defmethod valid-solution? ((s real-distinct-solution)
                            (e equation))
  (let ((c1       (solution-c1 s))
        (c2       (solution-c2 s))
        (r1       (solution-r1 s))
        (r2       (solution-r2 s))
        (y0       (equation-y0 e))
        (y0-prime (equation-y0-prime e)))
    (and (almost= y0
                  (+ c1 c2))
         (almost= y0-prime
                  (+ (* c1 r1)
                     (* c2 r2))))))

(defmethod valid-solution? ((s real-repeated-solution)
                            (e equation))
  (let ((c1       (solution-c1 s))
        (c2       (solution-c2 s))
        (r        (solution-r s))
        (y0       (equation-y0 e))
        (y0-prime (equation-y0-prime e)))
    (and (almost= y0 c1)
         (almost= y0-prime
                  (+ (* c1 r)
                     c2)))))

(defmethod valid-solution? ((s complex-solution)
                            (e equation))
  (let ((c1       (solution-c1 s))
        (c2       (solution-c2 s))
        (A        (solution-A  s))
        (B        (solution-B  s))
        (y0       (equation-y0 e))
        (y0-prime (equation-y0-prime e)))
    (and (almost= y0 c1)
         (almost= y0-prime
                  (+ (* A c1)
                     (* B c2))))))
