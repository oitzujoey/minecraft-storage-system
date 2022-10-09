
(defun move (direction)
  (local error)
  (if (= direction 'DOWN)
	  (set error (not (turtle.down)))
	  (if (= direction 'UP)
		  (set error (not (turtle.up)))
		  (if (= direction 'LEFT)
			  (set error (not (turtle.left)))
			  (if (= direction 'RIGHT)
				  (set error (not (turtle.right)))
				  (if (= direction 'FORWARD)
					  (set error (not (turtle.forward)))
					  (if (= direction 'BACKWARD)
						  (set error (not (turtle.back)))
						  (set error true)))))))
  (return error))

(defmacro save-excursion (direction &rest body)
  `(progn
	 (move ',direction)
	 ,@body
	 (move ,(case direction
			  (UP `'DOWN)
			  (DOWN `'UP)
			  (LEFT `'RIGHT)
			  (RIGHT `'LEFT)
			  (FORWARD `'BACKWARD)
			  (BACKWARD `'FORWARD)
			  (otherwise `'ERROR)))))

(save-excursion DOWN
				(save-excursion DOWN
								(turtle.suckDown)))
