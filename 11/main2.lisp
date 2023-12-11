(declaim (optimize (speed 3) (safety 0)))

(defun is-row-empty (lines row)
	(declare (type integer row))
	(loop for c across (nth row lines) do
		(when (char= c #\#)
			(return-from is-row-empty nil)))
	t)

(defun is-column-empty (lines column)
	(declare (type integer column))
	(loop for line in lines do
		(when (char= #\# (char line column))
			(return-from is-column-empty nil)))
	t)

(defun parse-galaxies (lines)
	(let ((res (list)))
		(loop for y from 0 below (list-length lines) do
			(loop for x from 0 below (length (first lines)) do
				(when (char= #\# (char (nth y lines) x))
					(setf res (nconc res (list (list x y)))))))
		res))

(defun empty-columns-between (lines c1 c2)
	(let ((sum 0))
		(loop for i from (min c1 c2) to (max c1 c2) do
			(when (is-column-empty lines i)
				(incf sum 1)))
		sum))

(defun empty-rows-between (lines r1 r2)
	(declare (type integer r1 r2))
	(let ((sum 0))
		(loop for i from (min r1 r2) to (max r1 r2) do
			(when (is-row-empty lines i)
				(incf sum 1)))
		sum))

(defun galaxy-distance (lines g1 g2)
	(+
		(abs (-
			(first g1)
			(first g2)))
		(abs (-
			(second g1)
			(second g2)))
		(*
			999999
			(empty-rows-between lines
				(second g1)
				(second g2)))
		(*
			999999
			(empty-columns-between lines
				(first g1)
				(first g2)))))


(defun sum-paths (galaxies lines)
	(let ((res 0))
		(loop for g1 in galaxies do
			(print g1)
			(loop for g2 in galaxies do
				(incf res (galaxy-distance lines g1 g2))))
		(/ res 2)))

(defun solve2 (lines)
	(sum-paths (parse-galaxies lines) lines))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))

	(print (solve2 lines)))
