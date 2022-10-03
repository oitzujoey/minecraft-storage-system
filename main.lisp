(local map)
(setf map (cl (list 'quote
					(with-open-file (infile "map.lisp")
					  (do ((result nil (cons next result))
						   (next (read infile nil 'eof)
								 (read infile nil 'eof)))
						  ((equal next 'eof) (reverse result)))))))

(while true
	   (turtle.forward))
