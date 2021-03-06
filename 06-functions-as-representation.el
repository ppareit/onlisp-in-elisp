;;; 06-functions-as-representation.el --- Scratch for onlisp
;;; Commentary:
;;; Code:

;;; 06-functions-as-representation.el ends here


(defstruct node contents yes no)

(defvar *nodes* (make-hash-table))

(defun defnode (name conts &optional yes no)
  "Define a node with name NAME and contens CONTS, optionally with a YES and NO part."
  (setf (gethash name *nodes*)
	(make-node :contents conts
		   :yes yes
		   :no no)))

(defnode 'people "Is the person a man?" 'male 'female)
(defnode 'male "Is he alive?" 'liveman 'deadman)
(defnode 'deadman "Was he American?" 'us 'them)
(defnode 'us "Is he on a coin?" 'coin 'cidence)
(defnode 'coin "Is the coin a penny?" 'penny 'coins)
(defnode 'penny 'lincoln)

(defun run-node (name)
  "Run the node with NAME."
  (let* ((n (gethash name *nodes*))
	 (prompt (format "%s " (node-contents n))))
    (cond ((node-yes n)
	   (if (y-or-n-p prompt)
	       (run-node (node-yes n))
	     (run-node (node-no n))))
	  (t (node-contents n)))))

(run-node 'people)

;;;-----------------------

(setq lexical-binding t)

(defvar *nodes* (make-hash-table))

(defun defnode (name conts &optional yes no)
  "Define a node with name NAME and contens CONTS, optionally with a YES and NO part."
  (setf (gethash name *nodes*)
	(if yes
	    #'(lambda ()
		(if (y-or-n-p conts)
		    (funcall (gethash yes *nodes*))
		  (funcall (gethash no *nodes*))))
	  #'(lambda () conts))))

(defnode 'people "Is the person a man? " 'male 'female)
(defnode 'male "Is he alive? " 'liveman 'deadman)
(defnode 'deadman "Was he American? " 'us 'them)
(defnode 'us "Is he on a coin? " 'coin 'cidence)
(defnode 'coin "Is the coin a penny? " 'penny 'coins)
(defnode 'penny 'lincoln)

(funcall (gethash 'people *nodes*))

(funcall (gethash 'penny *nodes*))

;;;---------------------------------

(setq lexical-binding t)

(defvar *nodes* nil)

(defun defnode (&rest args)
  "Define a node with the arguments ARGS."
  (push args *nodes*)
  args)

(defnode 'people "Is the person a man? " 'male 'female)
(defnode 'male "Is he alive? " 'liveman 'deadman)
(defnode 'deadman "Was he American? " 'us 'them)
(defnode 'us "Is he on a coin? " 'coin 'cidence)
(defnode 'coin "Is the coin a penny? " 'penny 'coins)
(defnode 'penny 'lincoln)

(defun compile-net (root)
  "Compile the network with root node ROOT."
  (let ((node (assoc root *nodes*)))
    (if (null node)
	nil
      (let ((conts (second node))
	    (yes (third node))
	    (no (fourth node)))
	(if yes
	    (let ((yes-fn (compile-net yes))
		  (no-fn (compile-net no)))
	      #'(lambda ()
		  (funcall (if (y-or-n-p conts)
			       yes-fn
			     no-fn))))
	  #'(lambda () conts))))))

(setq network (compile-net 'people))

(funcall network)

;;; 06-functions-as-representation.el ends here
