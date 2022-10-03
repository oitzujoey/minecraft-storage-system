
(local map)
(set map (cl (list 'quote
					(with-open-file (infile "map.lisp")
					  (do ((result nil (cons next result))
						   (next (read infile nil 'eof)
								 (read infile nil 'eof)))
						  ((equal next 'eof) (reverse result)))))))
(local home)
(set home map)

(local error)
(set error false)
(while (not error)
	   (set error (not (turtle.forward)))
	   (setm (x y z) (gps.locate)))
