(defun double-recursive (y)
	(cond 
		((not(listp y)) nil)
		((null y) nil)
		(t (append (list (car y) (car y)) (double-recursive (cdr y))))
	)
)

(defun double-mapcar (y)
	(cond
		((not(listp y)) nil)
		(t
			(apply #'append (mapcar #'list y y))
		)
	)
)

(defun double-iterative (y)
	(cond
		((not(listp y)) nil)
		((null y) nil)
		(t
			(do ((temp (list (car y) (car y))
				(append temp (list (car y) (car y)))))
			((null (cdr y)) temp) (setq y (cdr y)))
		)
	)
)

(defun rdouble (y)
	(cond
		((not(listp y)) nil)
		(t
			(cond
				((null y) nil)
				((null (car y)) (append (list 'nil) (rdouble (cdr y))))
				((atom (car y)) (append (list (car y) (car y)) (rdouble (cdr y))))
				(t (append (list (rdouble (car y))) (rdouble (cdr y))))
			)
		)
	)
)

(defun my-mapcar (fun x y)
	(cond
		((null x) nil)
		((null y) nil)
		(t (cons (funcall fun (car x) (car y)) (my-mapcar fun (cdr x) (cdr y))))
	)
)

(defun rewrite (x)
	(cond
		((null x) nil)
		((atom x) x)
		((equal (car x) 'if)
			(cond
				((= 3 (length x)) (list 'cond (list (rewrite (cadr x)) (rewrite(caddr x)))))
				(t (list 'cond (list (rewrite(cadr x)) (rewrite(caddr x))) (list 't (rewrite(cadddr x)))))
			)
		)
		((listp (car x)) (cons (rewrite (car x)) (rewrite(cdr x))))
		(t (cons (car x) (rewrite (cdr x))))
	)
) 	

(defun check (x)
	(list (equal (eval x) (eval (funcall 'rewrite x))) (eval x) (eval (funcall 'rewrite x)))
)









	
