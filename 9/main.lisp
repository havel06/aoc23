(defun parse-line (line)
	(read-from-string
		(concatenate 'string
			"("
			line
			")")))

(defun reduce-line (line)
	(let ((res (list)))
		(loop for i from 0 below (- (list-length line) 1) do
			(setf res (nconc res (list (- (nth (+ 1 i) line) (nth i line))))))
		res))

(defun predict-line (line)
	(if
		(every
			(lambda (l)
				(eq 0 l))
			line)
		0
		(+ (car (last line)) (predict-line (reduce-line line)))))

(defun predict-line-backwards (line)
	(if
		(every
			(lambda (l)
				(eq 0 l))
			line)
		0
		(- (first line) (predict-line-backwards (reduce-line line)))))


(defun solve1 (lines)
	(apply '+
		(mapcar 'predict-line
			(mapcar 'parse-line lines))))

(defun solve2 (lines)
	(apply '+
		(mapcar 'predict-line-backwards
			(mapcar 'parse-line lines))))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))

	;(print (solve1 lines)))
	(print (solve2 lines)))
