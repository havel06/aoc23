(declaim (optimize (speed 3) (safety 0)))

(defun parse-line (line)
	(read-from-string
		(concatenate 'string
			"("
			(subseq line (+ 1 (position #\: line)))
			")")))

(defun read-line-num (line)
	(parse-integer (remove #\space (subseq line (+ 1 (position #\: line))))))

(defun calc-distance (max-time hold-time)
	(declare (type integer max-time hold-time))
	(* hold-time (- max-time hold-time)))

(defun combinations (max-time dist)
	(let ((res 0))
		(loop for time from 1 below max-time do
			(when (> (calc-distance max-time time) dist)
				(setf res (+ 1 res))))
		res))

(defun solve1 (lines)
	(let (
		(times (parse-line (first lines)))
		(distances (parse-line (second lines)))
		(res 1))
			(loop for i from 0 below (list-length times) do
				(setf res (* res (combinations (nth i times) (nth i distances)))))
		res))

(defun solve2 (lines)
	(combinations (read-line-num (first lines)) (read-line-num (second lines))))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))

	;(print (solve1 lines)))
	(print (solve2 lines)))
