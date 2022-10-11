
(defun move (direction)
  (local error)
  (if (= direction 'DOWN)
	  (set error (not (turtle.down)))
	  (if (= direction 'UP)
		  (set error (not (turtle.up)))
		  (if (= direction 'LEFT)
			  (set error (not (turtle.turn-left)))
			  (if (= direction 'RIGHT)
				  (set error (not (turtle.turn-right)))
				  (if (= direction 'FORWARD)
					  (set error (not (turtle.forward)))
					  (if (= direction 'BACKWARD)
						  (set error (not (turtle.back)))
						  (set error true)))))))
  (return error))

(defmacro save-excursion (direction &rest body)
  (labels ((flatten (list)
			 (labels ((flatten2 (list last)
						(if (listp list)
							(if (null list)
								last
								(flatten2 (car list) (flatten2 (cdr list) last)))
							(cons list (if (null last)
										   nil
										   (flatten2 (car last) (cdr last)))))))
			   (flatten2 (car list) (cdr list))))
		   (inverse-direction (direction)
			 (case direction
			   (UP `'DOWN)
			   (DOWN `'UP)
			   (LEFT `'RIGHT)
			   (RIGHT `'LEFT)
			   (FORWARD `'BACKWARD)
			   (BACKWARD `'FORWARD)
			   (otherwise `'ERROR))))
	(if (listp direction)
		(let ((directions (flatten (mapcar (lambda (direction)
											 (if (listp direction)
												 (labels ((rec (n)
															(when (> n 0)
															  (cons (first direction) (rec (1- n))))))
												   (rec (second direction)))
												 direction))
										   direction))))
		  `(progn
			 ,@(mapcar (lambda (direction) `(move ',direction)) directions)
			 ,@body
			 ,@(mapcar (lambda (direction) `(move ,(inverse-direction direction))) (reverse directions))))
		`(progn
		   (move ',direction)
		   ,@body
		   (move ,(inverse-direction direction))))))

(local select-stack select-stack-top)
(set select-stack (array))
(set select-stack-top 0)
(defun push-select (index)
  (set select-stack-top (+ select-stack-top 1))
  (set select-stack[select-stack-top] (turtle.get-selected-slot))
  (turtle.select index))
(defun pop-select ()
  (turtle.select select-stack[select-stack-top])
  (set select-stack-top (- select-stack-top 1)))

(save-excursion ((DOWN 2))
				(turtle.suck-down)
				(save-excursion ((FORWARD 4)
								 RIGHT
								 FORWARD)
								(turtle.drop)
								(save-excursion (LEFT
												 FORWARD
												 RIGHT
												 (FORWARD 7)
												 RIGHT
												 FORWARD)
												(turtle.suck-down))))
