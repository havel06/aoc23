(defun my-numbers (line)
	(read-from-string
		(concatenate 'string
			(concatenate 'string
				"("
				(subseq line (+ 1 (position #\| line)))
			)
			")" )))

(defun winning-numbers (line)
	(read-from-string
		(concatenate 'string
			(concatenate 'string
				"("
				(subseq line
					(+ 1 (position #\: line))
					(position #\| line))
			)
			")" )))

(defun matching-numbers (line)
	(count-if
		(lambda (number) (member number (winning-numbers line)))
		(my-numbers line)))

(defun line-points (line)
	(let ((nums (matching-numbers line)))
		(if (eq nums 0)
			0
			(expt 2 (- (matching-numbers line) 1)))))

(defun solve1 (lines)
	(apply '+ 
		(mapcar 'line-points lines)))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))

	(print (solve1 lines)))
	;(print (solve2 lines)))
