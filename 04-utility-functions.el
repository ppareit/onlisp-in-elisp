;;; 04-utility-functions.el --- Scratch for onlisp
;;; Commentary:
;;; Code:

(cl-defun find2 (fn lst)
  (if (null lst)
      nil
    (let ((val (funcall fn (car lst))))
      (if val
	  (cl-values (car lst) val)
	(find2 fn (cdr lst))))))

(find2 '(lambda (x) (if (eq (car x) 'b) (cdr x))) '((a b c) (b c a) (c a b)))

(cl-defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
	 (cond ((funcall test y first) nil)
	       ((funcall test x first) lst)
	       (t (before x y (cdr lst) :test test))))))

(ert-deftest test-before ()
  (should (before 'b 'd '(a b c d)))
  (should (not (before 'd 'b '(a b c d))))
  (should (not (before 'e 'g '(a b c d)))))

(cl-defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
	((or (null src)
	     (funcall fn (car src)))
	 (cl-values (nreverse acc) src))
      (push (car src) acc))))

(ert-deftest test-split-if ()
  (should (equal (split-if (lambda (x) (> x 3)) '(1 2 3 4 5))
		 '((1 2 3) (4 5)))))


(cl-defun most (fn lst)
  (if (null lst)
      (values nil nil)
    (let* ((wins (car lst))
	   (max (funcall fn wins)))
      (dolist (obj (cdr lst))
	(let ((score (funcall fn obj)))
	  (when (> score max)
	    (setq wins obj
		  max score))))
      (values wins max))))

(ert-deftest test-most ()
  (should (equal (most #'length '((a b) (a b c) (a) (e f g)))
		 '((a b c) 3))))


(cl-defun best (fn lst)
  (if (null lst)
      nil
    (let ((wins (car lst)))
      (dolist (obj (cdr lst))
	(if (funcall fn obj wins)
	    (setq wins obj)))
      wins)))

(ert-deftest test-best ()
  (should (equal (best #'> '(1 3 4 2))
		 4)))

(cl-defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
    (let ((result (list (car lst)))
	  (max (funcall fn (car lst))))
      (dolist (obj (cdr lst))
	(let ((score (funcall fn obj)))
	  (cond ((> score max)
		 (setq max score
		       result (list obj)))
		((= score max)
		 (push obj result)))))
      (values (nreverse result) max))))

(ert-deftest test-mostn ()
  (should (equal (mostn #'length '((a b) (a b c) (a) (e f g)))
		 '(((a b c) (e f g)) 3))))

(cl-defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i)
       (nreverse result))
    (push (funcall fn i) result)))

(ert-deftest test-map-> ()
  (should (equal (map-> '(lambda (number) (% number 2))
			1
			'(lambda (number) (< 4 number))
			'1+)
		 '(1 0 1 0))))



;;; 04-utility-functions.el ends here
