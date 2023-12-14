(defun gets (lines x y)
	(if (< y 0)
		#\#	
		(char (nth y lines) x)))

(defun sets (lines x y value)
	(setf (char (nth y lines) x) value))

(defun move-rock (lines x y)
	(when (char= #\. (gets lines x (- y 1)))
		(sets lines x (- y 1) #\O)
		(sets lines x y #\.)
		(move-rock lines x (- y 1))))

(defun row-weight (lines row)
	(*
		(count #\O (nth row lines))
		(-
			(list-length lines)
			row)))

(defun solve1 (lines)
	(loop for y from 0 below (list-length lines) do
		(loop for x from 0 below (length (first lines)) do
			(when (char= (gets lines x y) #\O)
				(move-rock lines x y))))

	(loop for y from 0 below (list-length lines) sum
		(row-weight lines y)))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))
	(print (solve1 lines)))
	;(print (solve2 lines)))
