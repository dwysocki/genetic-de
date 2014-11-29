(load "ode.lisp")
(load "example-equations.lisp")

(defmethod ode-demo ()
  (let* ((eqn example-1st-order-ode-xy)
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
