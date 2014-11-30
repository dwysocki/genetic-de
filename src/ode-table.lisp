(load "ode.lisp")
(load "example-equations.lisp")

(defmethod print-row ((x list)
                      &optional
                      (output t))
  (format output "~{~A~^  ~}~%" x))

(defmethod print-best ((p population)
                       &optional
                       (output t))
  (let ((mfi (most-fit p)))
    (print-row (solution-y mfi)
               output)))

(defmethod ode-table ((e ode)
                      (iterations integer)
                      &optional
                      (output t))
  "Generates a table demonstrating the evolution of the most fit solution"
  (let ((pop (random-population e)))
    (print-row (mapcar #'float (equation-xs e))
               output)
    (print-best pop
                output)
    (dotimes (_ iterations)
      (setf pop
            (next-population pop))
      (print-best pop
                  output))))
