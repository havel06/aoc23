(declaim (optimize (speed 3) (safety 0)))

(defun parse-line (line)
	(list
		(char line 0)
		(parse-integer (subseq line 2 (position #\( line)))))

(defun parse-lines (lines)
	(mapcar 'parse-line lines))

(defun do-step (pos step)
	(cond
		((eq #\U (first step))
			(list (first pos) (- (second pos) (second step))))
		((eq #\D (first step))
			(list (first pos) (+ (second pos) (second step))))
		((eq #\L (first step))
			(list (- (first pos) (second step)) (second pos)))
		((eq #\R (first step))
			(list (+ (first pos) (second step)) (second pos)))))


(defun transform-steps (steps)
	(let ((res nil))
		(loop for step in steps do
			(dotimes (i (second step)) do
				(setf res (nconc res (list (list (first step) 1))))))
		res))

(defun map-size (steps)
	(let (
		(pos (list 0 0))
		(x-min 0)
		(x-max 0)
		(y-min 0)
		(y-max 0))
			(loop for step in steps do
				(setf pos (do-step pos step))
				(setf x-min (min x-min (first pos)))
				(setf x-max (max x-max (first pos)))
				(setf y-min (min y-min (second pos)))
				(setf y-max (max y-max (second pos))))
			(list (+ 1 (- x-max x-min)) (+ 1 (- y-max y-min)))))

(defun start-pos (steps)
	(let (
		(pos (list 0 0))
		(x-min 0)
		(y-min 0))
			(loop for step in steps do
				(setf pos (do-step pos step))
				(setf x-min (min x-min (first pos)))
				(setf y-min (min y-min (second pos))))
			(list (* -1 x-min) (* -1 y-min))))

(defun make-map (size)
	(loop for i from 0 below (second size) collect
		(make-string (first size) :initial-element #\.)))

(defun safe-char (str i)
	(cond
		((< i 0) #\.)
		((>= i (length str)) #\.)
		(t (char str i))))

(defun safe-get (map x y len-x len-y)
	(declare (type (first map) string))
	(cond
		((< x 0) #\#)
		((< y 0) #\#)
		((>= x (length (first map))) #\#)
		((>= y (list-length map)) #\#)
		(t (char (nth y map) x))))

(defun grow-outer (map len-x len-y)
	(loop for y from (- (list-length map) 1) downto 0 do
		(loop for x from 0 below (length (first map)) do
			(when (char= (safe-get map x y len-x len-y) #\.)
				(cond
					((char= #\X (safe-get map (+ x 1) y len-x len-y))
						(setf (char (nth y map) x) #\X)
						(print (list "grow" x y))
						(return-from grow-outer t))
					((char= #\X (safe-get map (- x 1) y len-x len-y))
						(setf (char (nth y map) x) #\X)
						(print (list "grow" x y))
						(return-from grow-outer t))
					((char= #\X (safe-get map x (+ y 1) len-x len-y))
						(setf (char (nth y map) x) #\X)
						(print (list "grow" x y))
						(return-from grow-outer t))
					((char= #\X (safe-get map x (- y 1) len-x len-y))
						(setf (char (nth y map) x) #\X)
						(print (list "grow" x y))
						(return-from grow-outer t))))))
	nil)

(defun fill-map (map)
	(setf (first map) (substitute #\X #\. (first map)))
	(setf (nth (- (list-length map) 1) map) (substitute #\X #\. (nth (- (list-length map) 1) map)))

	(loop for column from 0 below (length (first map)) do
		(loop for i from 0 below (list-length map) do
			(when (char= (safe-get map column i (length (first map)) (list-length map)) #\#)
				(return))
				(setf (char (nth i map) column) #\X))
		(loop for i from (- (list-length map) 1) downto 0 do
			(when (char= (safe-get map column i (length (first map)) (list-length map)) #\#)
				(return))
				(setf (char (nth i map) column) #\X)))

	(loop for line in map do
		(when (find #\# line)
			(loop for i from 0 below (position #\# line) do
				(setf (char line i) #\X))
			(loop for i from (+ 1 (position #\# line :from-end t)) below (length line) do
				(setf (char line i) #\X))))

	(print-map map)
	(let ((len-x (length (first map))) (len-y (list-length map)))
		(loop
			(when (not (grow-outer map len-x len-y))
				(return-from fill-map map)))))

(defun count-map (map)
	(loop for line in map sum
		(+
			(count #\# line)
			(count #\. line))))

(defun print-map (map)
	(write-line "")
	(write-line "")
	(write-line "")
	(loop for line in map do
		(write-line line)))

(defun solve1 (lines)
	(let ((steps (parse-lines lines)))
		(let ((map (make-map (map-size steps))) (pos (start-pos steps)))
			(loop for step in (transform-steps steps) do
				(setf (char (nth (second pos) map) (first pos)) #\#)
				(setf pos (do-step pos step)))
			(print-map map)
			(setf map (fill-map map))
			(print-map map)
			(count-map map))))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))
	(print (solve1 lines)))
	;(print (solve2 lines)))
