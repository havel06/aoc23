(defun is-row-empty (lines row)
	(loop for c across (nth row lines) do
		(when (char= c #\#)
			(return-from is-row-empty nil)))
	t)

(defun is-column-empty (lines column)
	(loop for line in lines do
		(when (char= #\# (char line column))
			(return-from is-column-empty nil)))
	t)

(defun expand-rows (lines)
	(let ((res (list)))
		(loop for i from 0 below (list-length lines) do
			(if (is-row-empty lines i)
				(setf res (nconc res (make-list 2 :initial-element (make-string (length (first lines)) :initial-element #\.))))
				(setf res (nconc res (list (nth i lines))))))
		res))

(defun insert-column (line column)
	(concatenate 'string
		(subseq line 0 column)
		"."
		(subseq line column)))

(defun expand-columns (lines)
	(let ((i 0))
		(loop
			(when (eq i (length (first lines)))
				(return))
			(when (is-column-empty lines i)
				(setf lines
					(mapcar
						(lambda (line) (insert-column line i))
						lines))
				(incf i))
			(incf i)))
	lines)

(defun expand-universe (lines)
	(expand-columns (expand-rows lines)))

(defun parse-galaxies (lines)
	(let ((res (list)))
		(loop for y from 0 below (list-length lines) do
			(loop for x from 0 below (length (first lines)) do
				(when (char= #\# (char (nth y lines) x))
					(setf res (nconc res (list (list x y)))))))
		res))

(defun galaxy-distance (g1 g2)
	(+
		(abs (-
			(first g1)
			(first g2)))
		(abs (-
			(second g1)
			(second g2)))))

(defun sum-paths (galaxies)
	(let ((res 0))
		(loop for g1 in galaxies do
			(loop for g2 in galaxies do
				;(print (list g1 g2 (galaxy-distance g1 g2)))
				(incf res (galaxy-distance g1 g2))))
		(/ res 2)))

(defun solve1 (lines)
	(loop for line in lines do
		(write-line line))
	(setf lines (expand-universe lines))
	(loop for line in lines do
		(write-line line))
	(sum-paths (parse-galaxies lines)))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))

	(print (solve1 lines)))
	;(print (solve2 lines)))
