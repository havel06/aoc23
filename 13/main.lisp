(defun parse-lines (lines)
	(let ((res (list)))
		(loop for line in lines for i from 1 do
			(if (string= line "")
				(return-from parse-lines
					(append
						(list res)
						(parse-lines (nthcdr i lines))))
				(setf res (nconc res (list line)))))
		(list res)))

(defun partial-equal (list1 list2)
	(loop for i from 0 below (min (list-length list1) (list-length list2)) do
		(when (not (string= (nth i list1) (nth i list2)))
			(return-from partial-equal nil)))
	t)

(defun is-row-mirror (pattern row)
	(partial-equal
		(reverse (subseq pattern 0 row))
		(subseq pattern row)))

(defun extract-column (pattern column)
	(let ((res ""))
		(loop for line in pattern do
			(setf res (concatenate 'string res (string (char line column)))))
		res))

(defun transpose-pattern (pattern)
	(loop for i from 0 below (length (first pattern)) collect
		(extract-column pattern i)))

(defun solve-rows (pattern)
	(loop for i from 1 below (list-length pattern) sum
		(if (is-row-mirror pattern i)
			i
			0)))

(defun solve-columns (pattern)
	(solve-rows (transpose-pattern pattern)))

(defun solve-pattern (pattern)
	(+
		(* 100 (solve-rows pattern))
		(solve-columns pattern)))

(defun solve1 (lines)
	(loop for pattern in (parse-lines lines) sum
		(solve-pattern pattern)))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))
	(print (solve1 lines)))
	;(print (solve2 lines)))
