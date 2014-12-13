(load "ode.lisp")

(defconstant example-1st-order-ode-xy
  ;; equation
  ;; dy/dx = x*y; y(0) = 1; 0 <= x <= 1; N = 50
  ;;
  ;; exact solution
  ;; y(x) = e^(x^2 / 2)
  (make-1st-order-ode (lambda (x y) (* x y))
                      1
                      0
                      1
                      50))

(defconstant example-1st-order-ode-xsiny
  ;; equation
  ;; dy/dx = x*sin(y); y(0) = 1; 0 <= x <= 1; N = 50
  ;;
  ;; exact solution
  ;; y(x) = 2 cot^-1( e^(-x^2 / 2) cot(1/2) )
  (make-1st-order-ode (lambda (x y) (* x (sin y)))
                      1
                      0
                      1
                      50))
