(load "ode.lisp")
(load "example-equations.lisp")

(defun ode-stats (&optional (eqn example-1st-order-ode-xy))
  (let ((pop (random-population eqn)))
    (dotimes (i 100)
      (let* ((fitnesses (mapcar (lambda (x) (fitness x eqn))
                                (population-solutions pop)))
             (mean (mean fitnesses))
             (stdev (stdev fitnesses))
             (min (apply #'min fitnesses))
             (max (apply #'max fitnesses))
             (range (- max min)))
        (format t
                "Generation ~D:~%  mean = ~D~%  std = ~D~%  range = ~D~%"
                i mean stdev range))
      (format t "----------~%")
      (setf pop (next-population pop)))))
