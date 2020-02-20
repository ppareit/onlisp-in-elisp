;;; 07-macros.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Pieter Pareit

;;; Commentary:

;;; Code:

(defmacro nil! (var)
  "Set argument VAR to nil."
  (list 'setq var nil))

(setq a "foo")
(nil! a)


(defmacro nil! (var)
  "Set argument VAR to nil."
  `(setq ,var nil))

(setq b "bar")
(nil! b)

(defmacro nif (expr pos zero neg)
  "Three-way numeric if, evaluate EXPR and depending on it's value, execute POS, ZERO or NEG."
  `(cond ((> ,expr 0) ,pos)
	 ((= ,expr 0) ,zero)
	 (t ,neg)))

(nif 0 "positive" "zero" "negative")


(defmacro our-when (test &rest body)
  "Execute BODY when TEST is true."
  `(if ,test
       (progn ,@body)))

(defun eligible-p (ARG) "...ARG..." t)
(defun do-this () "..." (message "Doing this.") (sleep-for 1))
(defun do-that () "..." (message "Doing that.") (sleep-for 1))
(defvar obj "obj")

(our-when (eligible-p obj)
	  (do-this)
	  (do-that)
	  obj)

;; Defining Simple Macros

(defmacro pp-memq (obj lst)
  "...OBJ.LST..."
  `(cl-member ,obj ,lst :test #'eq))

(memq 5 '(1 2 3 4 5 6 7))

(pp-memq 5 '(1 2 3 4 5 6 7))

(defvar *hungry* 5)

(defun hungry ()
  "Return t after a couple of times."
  (if (<= *hungry* 0)
      nil
    (setq *hungry* (- *hungry* 1))
    t))

(defun stare-intently ()
  "..."
  (message "Staring intently")
  (sleep-for 1))

(defun meow ()
  "..."
  (message "Meow, meow, meow, ...")
  (sleep-for 1))

(defun rub-against-legs ()
  "..."
  (message "Rubbing against the legs")
  (sleep-for 1))

(defmacro pp-while (test &rest body)
  ".TEST.BODY."
  `(do ()
       ((not ,test))
     ,@body))

(pp-while (hungry)
  (stare-intently)
  (meow)
  (rub-against-legs))


(provide '07-macros)
;;; 07-macros.el ends here
