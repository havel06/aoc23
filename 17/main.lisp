(defun invalid (lines x y)
	(cond
		((< x 0) t)
		((< y 0) t)
		((>= x (length (first lines))) t)
		((>= y (list-length lines)) t)
		(t nil)))

(defun opposite (dir new-dir)
	(or
		(eq (first dir) (* -1 (first new-dir)))
		(eq (second dir) (* -1 (second new-dir)))))

(defun is-finish (lines x y)
	(and
		(eq y (- (list-length lines) 1))
		(eq x (- (length (first lines)) 1))))

(defun path (lines x y dir came-from)
	(when (invalid lines x y)
		(return-from path 99999999))

	(when (is-finish lines x y)
		(print "finish!")
		(return-from path 0))
	
	(+
		(digit-char-p (char (nth y lines) x))
		(apply 'min
			(loop for new-dir in (list (list 1 0) (list 0 1) (list -1 0) (list 0 -1)) collect
				(cond
					((opposite dir new-dir) 99999999)
					(t (fast-path lines (+ x (first new-dir)) (+ y (second new-dir)) new-dir (append came-from (list(list x y (first dir) (second dir)))))))))))

(defvar *hash-table* (make-hash-table :test 'equal))

(defun fast-path (lines x y dir came-from)
	(print (list x y))
	(when (invalid lines x y)
		(print "invalid")
		(return-from fast-path 99999999))

	(when (find (list x y (first dir) (second dir)) came-from :test 'equal)
		(print "cycle")
		(return-from fast-path 99999999))

	(let ((found (gethash (list x y (first dir) (second dir)) *hash-table*)))
		(if found
			(progn
				(print (list "found!" x y dir))
				found)
			(let ((value (path lines x y dir came-from)))
				(print (list "solved!" x y dir))
				(setf (gethash (list x y (first dir) (second dir)) *hash-table*) value)
				value))))

(defun solve1 (lines)
	(min
		(path lines 0 0 (list 1 0) nil)
		(path lines 0 0 (list 0 1) nil)))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))
	(print (solve1 lines)))
	;(print (solve2 lines)))
