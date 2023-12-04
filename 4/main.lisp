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

(defun solve2 (lines)
	(let (
		(cards (loop for i from 0 below (list-length lines) collect i))
		(sum 0)
		(len (list-length lines))
		(processed-lines (mapcar 'matching-numbers lines)))
			(loop while cards do
				(let ((card (first cards)))
					(setf cards (cdr cards))
					(when (< card len)
						(setf sum (+ 1 sum))
						(let ((new-cards
							(loop for i from 1 to (nth card processed-lines) collect (+ card i))))
								(when new-cards
									(setf cards (append new-cards cards)))))))
		sum))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))

	;(print (solve1 lines)))
	(print (solve2 lines)))
