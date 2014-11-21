(load "gde.lisp")

(defconstant demo-equation
  (make-instance 'equation
     :a 1 :b 3 :c 1
     :y0 0 :y0-prime 1
     :min 0 :max 10))

(defconstant demo-exact-solution
  (make-instance 'real-distinct-solution
     :c1    (/ (sqrt 5))
     :c2 (- (/ (sqrt 5)))
     :r1 (*  1/2 (- (sqrt 5) 3))
     :r2 (* -1/2 (+ (sqrt 5) 3))))

(defconstant demo-other-solution
  (make-instance 'real-distinct-solution
     :c1  2
     :c2 (/ (- 0 1 (* 2 (1- (* 1/2 (- (sqrt 5) 3)))))
            (1- (* -1/2 (+ (sqrt 5) 3))))
     :r1 (*  1/2 (- (sqrt 5) 3))
     :r2 (* -1/2 (+ (sqrt 5) 3))))

(defmethod solution-demo ()
  (format t
          "Solutions for: ~A~%~%"
          (display demo-equation nil))
  (format t
          "Exact solution: ~AError: ~D~%~%"
          (display demo-exact-solution nil)
          (solution-error demo-exact-solution demo-equation))
  (format t
          "Other solution: ~AError: ~D~%~%"
          (display demo-other-solution nil)
          (solution-error demo-other-solution demo-equation)))
