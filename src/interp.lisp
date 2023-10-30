(defpackage :interp-4rules
  (:use :common-lisp)
  (:export :id
           :num
           :plus
           :minus
           :times
           :div
           :stmts
           :assign
           :print-
           :example-prog
           :interp))

(in-package :interp-4rules)

;; Expression
(defstruct expr ast val)
(defun id (val) (declare (string val)) (make-expr :ast 'Id :val val))
(defun num (val) (make-expr :ast 'Num :val val))
(defun plus (v1 v2) (make-expr :ast 'Plus :val `(:x ,v1 :y ,v2)))
(defun minus (v1 v2) (make-expr :ast 'Minus :val `(:x ,v1 :y ,v2)))
(defun times (v1 v2) (make-expr :ast 'Times :val `(:x ,v1 :y ,v2)))
(defun div (v1 v2) (make-expr :ast 'Div :val `(:x ,v1 :y ,v2)))


;; Statements
(defstruct stm ast val)
(defun stmts (v1 v2) (make-stm :ast 'Stmts :val `(:x ,v1 :y ,v2)))
(defun assign (i val) (make-stm :ast 'Assign :val `(:x ,i :y ,val)))
(defun print- (e) (make-stm :ast 'Print :val e))


;; Predicates
(defun id-p (ast) (and (expr-p ast) (eql (expr-ast ast) 'Id)))
(defun num-p (ast) (and (expr-p ast) (eql (expr-ast ast) 'Num)))
(defun plus-p (ast) (and (expr-p ast) (eql (expr-ast ast) 'Plus)))
(defun minus-p (ast) (and (expr-p ast) (eql (expr-ast ast) 'Minus)))
(defun times-p (ast) (and (expr-p ast) (eql (expr-ast ast) 'Times)))
(defun div-p (ast) (and (expr-p ast) (eql (expr-ast ast) 'Div)))
(defun stmts-p (ast) (and (stm-p ast) (eql (stm-ast ast) 'Stmts)))
(defun assign-p (ast) (and (stm-p ast) (eql (stm-ast ast) 'Assign)))
(defun print-p (ast) (and (stm-p ast) (eql (stm-ast ast) 'Print)))


(defun e0 (x)
  (declare (ignore x))
  (warn "No such symbol"))

(defparameter env #'e0)

(defun update (var vl env)
  (lambda (v)
    (if (string= v var)
        vl
        (funcall env v))))

(defun trans-exp (ast env)
  (cond
    ((id-p ast) (funcall env (expr-val ast)))
    ((num-p ast) (expr-val ast))
    ((plus-p ast) (+ (trans-exp (getf (expr-val ast) :x) env)
                     (trans-exp (getf (expr-val ast) :y) env)))
    ((minus-p ast) (- (trans-exp (getf (expr-val ast) :x) env)
                      (trans-exp (getf (expr-val ast) :y) env)))
    ((times-p ast) (* (trans-exp (getf (expr-val ast) :x) env)
                      (trans-exp (getf (expr-val ast) :y) env)))
    ((div-p ast) (/ (trans-exp (getf (expr-val ast) :x) env)
                    (trans-exp (getf (expr-val ast) :y) env)))))

(defun trans-stmt (ast env)
  (cond
    ((stmts-p ast) (trans-stmt (getf (stm-val ast) :y)
                               (trans-stmt (getf (stm-val ast) :x) env)))
    ((assign-p ast) (setf env (update (getf (stm-val ast) :x)
                                      (trans-exp (getf (stm-val ast) :y) env)
                                      env)))
    ((print-p ast) (progn env (format t "~A~%" (trans-exp (stm-val ast) env))))))

(defparameter example-prog (stmts
                             (assign "x" (plus (num 2) (times (num 2) (num 3))))
                             (stmts (assign "y" (div (id "x") (num 4)))
                                    (print- (id "y")))))

(defun interp (ast) (trans-stmt ast env))
