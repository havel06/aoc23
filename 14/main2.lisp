(defun gets (lines x y)
	(cond
		((< y 0) #\#)
		((>= y (list-length lines)) #\#)
		((< x 0) #\#)
		((>= x (length (first lines))) #\#)
		(t (char (nth y lines) x))))

(defun sets (lines x y value)
	(cond
		((< y 0) nil)
		((>= y (list-length lines)) nil)
		((< x 0) nil)
		((>= x (length (first lines))) nil)
		(t (setf (char (nth y lines) x) value))))

(defun move-rock-north (lines x y)
	(when (char= #\. (gets lines x (- y 1)))
		(sets lines x (- y 1) #\O)
		(sets lines x y #\.)
		(move-rock-north lines x (- y 1))))

(defun move-rock-south (lines x y)
	(when (char= #\. (gets lines x (+ y 1)))
		(sets lines x (+ y 1) #\O)
		(sets lines x y #\.)
		(move-rock-south lines x (+ y 1))))

(defun move-rock-east (lines x y)
	(when (char= #\. (gets lines (+ x 1) y))
		(sets lines (+ x 1) y #\O)
		(sets lines x y #\.)
		(move-rock-east lines (+ x 1) y)))

(defun move-rock-west (lines x y)
	(when (char= #\. (gets lines (- x 1) y))
		(sets lines (- x 1) y #\O)
		(sets lines x y #\.)
		(move-rock-west lines (- x 1) y)))

(defun do-cycle-north (lines)
	(loop for y from 0 below (list-length lines) do
		(loop for x from 0 below (length (first lines)) do
			(when (char= (gets lines x y) #\O)
				(move-rock-north lines x y)))))

(defun do-cycle-south (lines)
	(loop for y from (- (list-length lines) 1) downto 0 do
		(loop for x from 0 below (length (first lines)) do
			(when (char= (gets lines x y) #\O)
				(move-rock-south lines x y)))))

(defun do-cycle-west (lines)
	(loop for x from 0 below (length (first lines)) do
		(loop for y from 0 below (list-length lines) do
			(when (char= (gets lines x y) #\O)
				(move-rock-west lines x y)))))

(defun do-cycle-east (lines)
	(loop for x from (- (length (first lines)) 1) downto 0 do
		(loop for y from 0 below (list-length lines) do
			(when (char= (gets lines x y) #\O)
				(move-rock-east lines x y)))))

(defun do-cycle (lines)
	(do-cycle-north lines)
	(do-cycle-west lines)
	(do-cycle-south lines)
	(do-cycle-east lines))

(defun row-weight (lines row)
	(*
		(count #\O (nth row lines))
		(-
			(list-length lines)
			row)))

(defun deep-copy (lines)
	(mapcar 'copy-seq lines))

(defvar *table* (make-hash-table :test 'equal))

(defun cycles-until-repeat (lines)
	(let (
		(i 0))
			(loop
				(print i)
				(when (gethash (deep-copy lines) *table*)
					(return-from cycles-until-repeat
						(values
							(gethash (deep-copy lines) *table*)
							(- i (gethash (deep-copy lines) *table*)))))
				(setf (gethash (deep-copy lines) *table*) i)

				(incf i)
				(do-cycle lines))))

(defun solve2 (lines)
	(multiple-value-bind (until-repeat repeat-len) (cycles-until-repeat lines)
		(dotimes (i
			(+
				0 ;there should be until-repeat, but this works, idk why
				(mod (- 1000000000 until-repeat) repeat-len)))
			(do-cycle lines)))

	(loop for y from 0 below (list-length lines) sum
		(row-weight lines y)))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))
	(print (solve2 lines)))
