(load "ode.lisp")

(defconstant example-1st-order-ode-xy
  (make-1st-order-ode (lambda (x y) (* x y))
                      1
                      0
                      1
                      50))

(defconstant example-1st-order-ode-xsiny
  (make-1st-order-ode (lambda (x y) (* x (sin y)))
                      1
                      0
                      1
                      10))
