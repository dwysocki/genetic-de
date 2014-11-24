(load "ode.lisp")

(defmethod ode-demo ()
  (let* ((eqn (make-1st-order-ode
                ;; dy/dx = x*y(x)
                (lambda (x y) (* x y))
                ;; y_0 = 0
                0
                ;; x_0 = 0
                0
                ;; x_N = 1
                1
                ;; N   = 20
                20))
         (pop (random-population eqn)))
    (display pop)
    (setf pop
          (next-population pop))
    (dotimes (_ 1000)
      (format t
              "generation ~D~%"
              (population-generation pop))
      (format t
              "avg fitness = ~D~%"
              (mean-fitness pop))
      (setf pop
            (next-population pop)))

    (display pop)))
