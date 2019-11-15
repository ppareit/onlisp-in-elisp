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

(cl-defun memoize (fn)
  (lexical-let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
	(let ((val (gethash args cache)))
	  (if val
	      val
	    (setf (gethash args cache)
		  (apply fn args)))))))

(cl-defun compose (&rest fns)
  (if fns
      (lexical-let ((fn1 (car (last fns)))
		    (fns (butlast fns)))
	#'(lambda (&rest args)
	    (reduce #'funcall fns
		    :from-end t
		    :initial-value (apply fn1 args))))
    #'identity))

(ert-deftest test-compose ()
  (should (equal (funcall (compose #'1+ #'find-if)
			  #'oddp
			  '(2 3 4))
		 4)))

;;; Recursion on Cdrs

(setq lexical-binding t)
(cl-defun lrec (rec &optional base)
  (labels ((self (lst)
		 (if (null lst)
		     (if (functionp base)
			 (funcall base)
		       base)
		   (funcall rec (car lst)
			    #'(lambda ()
				(self (cdr lst)))))))
    #'self))

(ert-deftest test-lrec-list-length ()
  (funcall (lrec #'(lambda (x f) (1+ (funcall f))) 0) '(a b c))
  3)

(ert-deftest test-lrec-list-odd ()
  (funcall (lrec #'(lambda (x f) (and (oddp x) (funcall f))) t)
	   '(1 3 5))
  t)

;;; 5.6 Recursion on Subtrees

(setq x '(a b)
      listx (list x 1))

(eq x (car (copy-list listx)))
(equal x (car (copy-list listx)))

(eq x (car (copy-tree listx)))
(equal x (car (copy-tree listx)))

(cl-defun our-copy-tree (tree)
  (if (atom tree)
      tree
    (cons (our-copy-tree (car tree))
	  (if (cdr tree) (our-copy-tree (cdr tree))))))

(ert-deftest test-our-copy-tree ()
    (let* ((x '(a b))
	   (listx (list x 1)))
      (equal x (car (our-copy-tree listx)))))

;;; 05-returning-functions.el ends here
