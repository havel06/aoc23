(defun parse-numbers (numbers-str)
	(read-from-string
		(concatenate 'string
			"("
			(substitute #\space #\, numbers-str)
			")")))

(defun parse-line (line)
	(list
		(subseq line 0 (position #\space line))
		(parse-numbers (subseq line (position #\space line)))))

(defun parse-lines (lines)
	(mapcar 'parse-line lines))

(defun valid-placement (str placement len)
	(when (> len (length str))
		(return-from valid-placement nil))

	(loop for i from placement below (+ placement len) do
		(when (char= (char str i) #\.)
			(return-from valid-placement nil)))

	(when
		(and
			(< (+ placement len) (length str))
			(char= #\# (char str (+ placement len))))
		(return-from valid-placement nil))

	(when
		(and
			(> placement 0)
			(char= #\# (char str (- placement 1))))
		(return-from valid-placement nil))
	t)

(defun safe-subseq (str start)
	(if (>= start (length str))
		""
		(subseq str start)))

(defun first-spring-position (str)
	(let ((res (position #\# str)))
		(if res
			res	
			(length str))))

(defun solve-line (line)
	(when (not (second line))
		(return-from solve-line
			(if (eq 0 (count #\# (first line)))
				1
				0)))

	(loop for placement from 0 to (min (- (length (first line)) (first (second line))) (first-spring-position (first line)))
		sum
			(if (valid-placement (first line) placement (first (second line)))
				(solve-line (list
					(safe-subseq (first line) (+ 1 placement (first (second line))))
					(rest (second line))))
				0)))

(defun solve-line-non-rec (line)
	(let ((res (solve-line line)))
		(print (list line res))
		res))
	
(defun solve2 (lines)
	(apply '+
		(mapcar 'solve-line-non-rec (parse-lines lines))))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))
	(print (solve2 lines)))
