;;; 04-utility-functions.el --- Scratch for onlisp
;;; Commentary:
;;; Code:

(defun find2 (fn lst)
  "Find the element in LST that satisfies condition FN."
  (if (null lst)
      nil
    (let ((val (funcall fn (car lst))))
      (if val
	  (cl-values (car lst) val)
	(find2 fn (cdr lst))))))

(find2 '(lambda (x) (if (eq (car x) 'b) (cdr x))) '((a b c) (b c a) (c a b)))

;;; 04-utility-functions.el ends here
