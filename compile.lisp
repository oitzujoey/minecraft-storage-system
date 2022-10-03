
(defmacro collect (bind &rest body)
  `(loop for ,(first bind) in ,(second bind) collect (progn ,@body)))

(defmacro collect-string (bind &rest body)
  `(apply #'concatenate 'string (collect ,bind ,@body)))


(defvar symbols (make-hash-table))
(defvar symbols-size 0)

(setf (gethash 'true symbols) "true")
(incf symbols-size)
(setf (gethash 'false symbols) "false")
(incf symbols-size)

(defun emit-symbol (sym)
  (multiple-value-bind (value status) (gethash sym symbols)
	(if status
		value
		(progn
		  (setf (gethash sym symbols) symbols-size)
		  (incf symbols-size)
		  (1- symbols-size)))))

(defun emit-quote (expr)
  (cond ((null expr) "nil")
		((numberp expr) expr)
		((stringp expr) expr)
		((symbolp expr) (emit-symbol expr))
		((listp expr) (emit-funcall `(table ,@(let ((index 0))
												(collect (subexpr expr)
												  (prog1
													  (cons index
															(emit-quote subexpr))
													(incf index)))))))
		(t (error (concatenate 'string "Bad quoted expression: " (write-to-string expr))))))

(defun emit-funcall (form)
  (let ((fn (first form))
		(args (rest form)))
	(case fn
	  (quote (if (/= (length args) 1)
				 (error (concatenate 'string "QUOTE accepts one argument. " (write-to-string (length args)) " given."))
				 (emit-expression (emit-quote (car args)))))
	  (progn (let ((args-left (length args)))
			   (collect-string (expr args)
							   (concatenate 'string (emit-expression expr)
											(if (/= args-left 1)
												(prog1
													" "
												  (decf args-left))
												"")))))
	  (table (let ((temp (concatenate 'string
									  "t"
									  (write-to-string (emit-symbol (gensym))))))
			   (concatenate 'string
							"(function() local "
							temp
							"={} "
							(collect-string (bind args)
											(let ((key (car bind))
												  (value (cdr bind)))
											  (concatenate 'string
														   temp
														   "["
														   (emit-expression key)
														   "]="
														   (emit-expression value)
														   " ")))
							"return "
							temp
							" end)()")))
	  (local (let ((args-left (length args)))
			   (collect-string (arg args)
							   (concatenate 'string
											"local "
											(emit-expression arg)
											(if (/= args-left 1)
												(prog1
													" "
												  (decf args-left))
												"")))))
	  (set (if (/= (length args) 2)
			   (error (concatenate 'string "SET accepts two arguments. " (write-to-string (length args)) " given."))
			   (concatenate 'string
							(if (listp (first args))
								(let ((args-left (length (first args))))
								  (collect-string (arg (first args))
												  (concatenate 'string
															   (emit-expression arg)
															   (if (/= args-left 1)
																   (prog1
																	   ","
																	 (decf args-left))
																   ""))))
								(emit-expression (first args)))
							"="
							(emit-expression (second args)))))
	  (while (concatenate 'string
						  "while "
						  (emit-expression (first args))
						  " do "
						  (let ((args-left (length (rest args))))
							(collect-string (arg (rest args))
											(concatenate 'string
														 (emit-expression arg)
														 (if (/= args-left 1)
															 (prog1
																 " "
															   (decf args-left))
															 ""))))
						  " end"))
	  (when (concatenate 'string
						 "if "
						 (emit-expression (first args))
						 " then "
						 (let ((args-left (length (rest args))))
						   (collect-string (arg (rest args))
										   (concatenate 'string
														(emit-expression arg)
														(if (/= args-left 1)
															(prog1
																" "
															  (decf args-left))
															""))))
						 " end"))
	  (unless (concatenate 'string
						   "if not "
						   (emit-expression (first args))
						   " then "
						   (let ((args-left (length (rest args))))
							 (collect-string (arg (rest args))
											 (concatenate 'string
														  (emit-expression arg)
														  (if (/= args-left 1)
															  (prog1
																  " "
																(decf args-left))
															  ""))))
						   " end"))
	  (if (if (/= (length args) 3)
			  (error (concatenate 'string "IF accepts three arguments. " (write-to-string (length args)) " given."))
			  (concatenate 'string
						   "if "
						   (emit-expression (first args))
						   " then "
						   (emit-expression (second args))
						   " else "
						   (emit-expression (third args))
						   " end")))
	  (> (if (/= (length args) 2)
			 (error (concatenate 'string "> accepts two arguments. " (write-to-string (length args)) " given."))
			 (concatenate 'string
						  "("
						  (emit-expression (first args))
						  ">"
						  (emit-expression (second args))
						  ")")))
	  (< (if (/= (length args) 2)
			 (error (concatenate 'string "< accepts two arguments. " (write-to-string (length args)) " given."))
			 (concatenate 'string
						  "("
						  (emit-expression (first args))
						  ">"
						  (emit-expression (second args))
						  ")")))
	  (= (if (/= (length args) 2)
			 (error (concatenate 'string "= accepts two arguments. " (write-to-string (length args)) " given."))
			 (concatenate 'string
						  "("
						  (emit-expression (first args))
						  "=="
						  (emit-expression (second args))
						  ")")))
	  (and (if (/= (length args) 2)
			   (error (concatenate 'string "AND accepts two arguments. " (write-to-string (length args)) " given."))
			   (concatenate 'string
							"("
							(emit-expression (first args))
							" and "
							(emit-expression (second args))
							")")))
	  (or (if (/= (length args) 2)
			  (error (concatenate 'string "OR accepts two arguments. " (write-to-string (length args)) " given."))
			  (concatenate 'string
						   "("
						   (emit-expression (first args))
						   " or "
						   (emit-expression (second args))
						   ")")))
	  (cl (emit-expression (eval `(progn ,@args))))
	  (otherwise (concatenate 'string
							  (map 'string
								   (lambda (char) (if (eq char #\@) #\: char))
								   (string-downcase (write-to-string fn)))
							  "("
							  (let ((args-left (length args)))
								(collect-string (arg args)
												(concatenate 'string
															 (emit-expression arg)
															 (if (/= args-left 1)
																 (prog1
																	 ","
																   (decf args-left))
																 ""))))
							  ")")))))

(defun emit-expression (expr)
  (cond ((null expr) "nil")
		((numberp expr) (write-to-string expr))
		((stringp expr) expr)
		((symbolp expr) (string-downcase (write-to-string expr)))
		((listp expr) (emit-funcall expr))
		(t (error (concatenate 'string "Bad expression: " (write-to-string expr))))))

(defun compile-project ()
  (let ((source (with-open-file (infile "main.lisp")
				  (do ((result nil (cons next result))
					   (next (read infile nil 'eof) (read infile nil 'eof)))
					  ((equal next 'eof) (reverse result))))))
	(with-open-file (outfile "main.lua" :direction :output
										:if-exists :supersede)
	  (format outfile (emit-expression `(progn ,@source))))))
