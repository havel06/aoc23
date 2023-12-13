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

(defun row-reflections (pattern)
	(loop for i from 1 below (list-length pattern)
		when (is-row-mirror pattern i) collect i))

(defun solve-rows (pattern)
	(apply '+ (row-reflections pattern)))

(defun solve-columns (pattern)
	(solve-rows (transpose-pattern pattern)))

(defun solve-pattern (pattern)
	(+
		(* 100 (solve-rows pattern))
		(solve-columns pattern)))

(defun solve1 (lines)
	(loop for pattern in (parse-lines lines) sum
		(solve-pattern pattern)))

(defun pattern-get (pattern row column)
	(char (nth row pattern) column))

(defun pattern-set (pattern row column value)
	(setf (char (nth row pattern) column) value))

(defun deep-copy (pattern)
	(mapcar 'copy-seq pattern))

(defun alter-pattern (pattern row column)
	(let ((copy (deep-copy pattern)))
		(if (char= #\# (pattern-get copy row column))
			(pattern-set copy row column #\.)
			(pattern-set copy row column #\#))
		copy))

(defun new-element (altered old)
	(loop for element in altered do
		(when (not (find element old))
			(return-from new-element element)))
	nil)

(defun solve-fixed-pattern (pattern)
	(loop for row from 0 below (list-length pattern) do
		(loop for column from 0 below (length (first pattern)) do
			(let ((altered (alter-pattern pattern row column)))
				(cond
					((and
						(> (solve-rows altered) 0)
						(not (equal (row-reflections altered) (row-reflections pattern))))
							(return-from solve-fixed-pattern (* 100 (new-element (row-reflections altered) (row-reflections pattern)))))
					((and
						(> (solve-rows (transpose-pattern altered)) 0)
						(not (equal (row-reflections (transpose-pattern altered)) (row-reflections (transpose-pattern pattern)))))
							(return-from solve-fixed-pattern (new-element (row-reflections (transpose-pattern altered)) (row-reflections (transpose-pattern pattern)))))))))
	(print (list "not found" pattern))
	nil)

(defun solve2 (lines)
	(loop for pattern in (parse-lines lines) sum
		(solve-fixed-pattern pattern)))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))
	;(print (solve1 lines)))
	(print (solve2 lines)))
