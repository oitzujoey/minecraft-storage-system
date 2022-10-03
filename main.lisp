
(local map)
(set map (cl `(table ,@(mapcar (lambda (entry)
								 `(,(emit-symbol (first entry)) . (array ,@(rest entry))))
							   (with-open-file (infile "map.lisp")
								 (do ((result nil (cons next result))
									  (next (read infile nil 'eof)
											(read infile nil 'eof)))
									 ((equal next 'eof) (reverse result))))))))
(local home fuel)
(set home (elt map 'home))
(set fuel (elt map 'fuel))

(local orientation desiredOrientation)
(set orientation 'NONE)

(local error)
(set error false)

(local ox oy oz x y z dx dy dz)
(set (ox oy oz) (gps.locate))
(set error (not (turtle.forward)))
(set (x y z) (gps.locate))
(set dx (- x ox))
(set dz (- z oz))

(unless (= dx 0)
  (if (< dx 0)
	  (set orientation 'WEST)
	  (set orientation 'EAST)))
(unless (= dz 0)
  (if (< dz 0)
	  (set orientation 'NORTH)
	  (set orientation 'SOUTH)))

;; Move to right height first.
(defun move-to-coordinates (coord)
  (local run)
  (set run true)
  (while (and run (not error))
	(set (x y z) (gps.locate))
	(if (> y (elt coord 1))
		(set error (not (turtle.down)))
		(if (< y (elt coord 1))
			(set error (not (turtle.up)))
			(set run false))))

  (set run true)
  (set (x y z) (gps.locate))
  (while (and run (not error))
	(if (= x (elt coord 0))
		(if (= z (elt coord 2))
			(set run false)
			(if (< z (elt coord 2))
				(set desiredOrientation 'SOUTH)
				(set desiredOrientation 'NORTH)))
		(if (< x (elt coord 0))
			(set desiredOrientation 'EAST)
			(set desiredOrientation 'WEST)))
	(while (and run
				(and (not (= desiredOrientation orientation))
					 (not error)))
	  (set error (not (turtle.turn-left)))
	  (if (= orientation 'EAST)
		  (set orientation 'NORTH)
		  (if (= orientation 'NORTH)
			  (set orientation 'WEST)
			  (if (= orientation 'WEST)
				  (set orientation 'SOUTH)
				  (when (= orientation 'SOUTH)
					(set orientation 'EAST))))))
	(set error (not (turtle.forward)))
	(set (x y z) (gps.locate))
	(when (and (= x (elt coord 0))
			   (= z (elt coord 2)))
	  (set run false))))

(while true
	   (move-to-coordinates home)
	   (move-to-coordinates fuel))
