;;; 05-returning-functions.el --- Scratch for onlisp
;;; Commentary:
;;; Code:

(cl-defun joiner (obj)
  (typecase obj
    (cons #'append)
    (number #'+)))

(cl-defun join (&rest args)
  (apply (joiner (car args)) args))

(ert-deftest test-join ()
  (should (equal (join 1 2 3 4 5)
		 15)))

(setq lexical-binding t)

(cl-defun make-adder (n)
  #'(lambda (x) (+ x n)))

(ert-deftest test-make-adder ()
  (let ((add3 (make-adder 3)))
    (should (equal (funcall add3 2)
		   5))))

(cl-defun complement (fn)
  #'(lambda (&rest args) (not (apply fn args))))

(ert-deftest test-complement ()
  (should (equal (remove-if (complement #'oddp) '(1 2 3 4 5 6))
		 (remove-if-not #'oddp '(1 2 3 4 5 6)))))


;;; 05-returning-functions.el ends here
