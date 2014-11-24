(load "ode.lisp")

(defconstant example-1st-order-ode-xy
  (make-1st-order-ode (lambda (x y) (* x y))
                      1
                      0
                      1
                      10))
