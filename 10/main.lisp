(defun find-start (lines)
	(loop for y from 0 below (list-length lines) do
		(loop for x from 0 below (length (first lines)) do
			(when (char= #\S (char (nth y lines) x))
				(return-from find-start (list x y))))))

(defun offset-eq (offset1 offset2)
	(and
		(eq (first offset1) (first offset2))
		(eq (second offset1) (second offset2))))

(defun transform-offset-I (offset)
	(cond
		((offset-eq offset (list 0 -1))
			(list 0 1))
		((offset-eq offset (list 0 1))
			(list 0 -1))
		(t nil)))

(defun transform-offset-- (offset)
	(cond
		((offset-eq offset (list 1 0))
			(list -1 0))
		((offset-eq offset (list -1 0))
			(list 1 0))
		(t nil)))

(defun transform-offset-L (offset)
	(cond
		((offset-eq offset (list 0 -1))
			(list 1 0))
		((offset-eq offset (list 1 0))
			(list 0 -1))
		(t nil)))

(defun transform-offset-J (offset)
	(cond
		((offset-eq offset (list 0 -1))
			(list -1 0))
		((offset-eq offset (list -1 0))
			(list 0 -1))
		(t nil)))

(defun transform-offset-7 (offset)
	(cond
		((offset-eq offset (list 0 1))
			(list -1 0))
		((offset-eq offset (list -1 0))
			(list 0 1))
		(t nil)))

(defun transform-offset-F (offset)
	(cond
		((offset-eq offset (list 0 1))
			(list 1 0))
		((offset-eq offset (list 1 0))
			(list 0 1))
		(t nil)))

(defun transform-offset (offset pipe)
	(cond
		((char= pipe #\|)
			(transform-offset-I offset))
		((char= pipe #\-)
			(transform-offset-- offset))
		((char= pipe #\L)
			(transform-offset-L offset))
		((char= pipe #\J)
			(transform-offset-J offset))
		((char= pipe #\7)
			(transform-offset-7 offset))
		((char= pipe #\F)
			(transform-offset-F offset))
		(t nil)))

(defparameter offsets
	(list
		(list 0 1)
		(list 1 0)
		(list 0 -1)
		(list -1 0)))

(defun negate-offset (offset)
	(mapcar (lambda (n) (* -1 n)) offset))

(defun get-pipe (lines pos)
	(let ((x (first pos)) (y (second pos)))
		(cond
			((< x 0) #\.)
			((>= x (length (first lines))) #\.)
			((< y 0) #\.)
			((>= y (length lines)) #\.)
			(t (char (nth y lines) x)))))

(defun add-offset (pos offset)
	(list
		(+ (first pos) (first offset))
		(+ (second pos) (second offset))))

(defun sub-offset (pos offset)
	(list
		(- (first pos) (first offset))
		(- (second pos) (second offset))))

(defun find-starting-pipe (lines)
	(let ((start-pos (find-start lines)))
		(loop for offset in offsets do
			(when (transform-offset (negate-offset offset) (get-pipe lines (add-offset start-pos offset)))
				(return-from find-starting-pipe (add-offset start-pos offset))))))

(defun get-new-pos (pos came-from lines)
	;(print (list pos (get-pipe lines pos)))
	(add-offset (transform-offset (sub-offset came-from pos) (get-pipe lines pos)) pos))

(defun solve1 (lines)
	(let (
		(came-from (find-start lines))
		(pos (find-starting-pipe lines))
		(steps 1))
			(print came-from)
			(print pos)
			(print (get-pipe lines pos))
			(loop
				(let ((new-pos (get-new-pos pos came-from lines)))
					(incf steps)
					(when (char= #\S (get-pipe lines new-pos))
						(return-from solve1 (/ steps 2)))
					(setf came-from pos)
					(setf pos new-pos)))))

(defun create-map-row (lines)
	(make-string (length (first lines)) :initial-element #\.))

(defun create-map (lines)
	(let ((res (list)))
		(loop for line in lines do
			(setf res (nconc res (list (create-map-row lines)))))
		res))

(defun offset-left (pos came-from map)
	(cond
		((offset-eq (sub-offset pos came-from) (list 0 1))
			(list 1 0))
		((offset-eq (sub-offset pos came-from) (list 0 -1))
			(list -1 0))
		((offset-eq (sub-offset pos came-from) (list 1 0))
			(list 0 -1))
		((offset-eq (sub-offset pos came-from) (list -1 0))
			(list 0 1))))

(defun offset-right (pos came-from map)
	(cond
		((offset-eq (sub-offset pos came-from) (list 0 1))
			(list -1 0))
		((offset-eq (sub-offset pos came-from) (list 0 -1))
			(list 1 0))
		((offset-eq (sub-offset pos came-from) (list 1 0))
			(list 0 1))
		((offset-eq (sub-offset pos came-from) (list -1 0))
			(list 0 -1))))

(defun map-get (map pos)
	(let ((x (first pos)) (y (second pos)))
		(cond
			((< x 0) #\space)
			((>= x (length (first map))) #\space)
			((< y 0) #\space)
			((>= y (length map)) #\space)
			(t (char (nth y map) x)))))

(defun mark-left-right (pos came-from map)
	(let (
		(left (add-offset pos (offset-left pos came-from map)))
		(right (add-offset pos (offset-right pos came-from map)))
		(right2 (add-offset came-from (offset-right pos came-from map))))
			(when (char= #\. (map-get map left))
				(setf (char (nth (second left) map) (first left)) #\L))
			(when (char= #\. (map-get map right))
				(setf (char (nth (second right) map) (first right)) #\R))
			(when (char= #\. (map-get map right2))
				(setf (char (nth (second right2) map) (first right2)) #\R))))

(defun mark-loop (lines map)
	(let (
		(came-from (find-start lines))
		(pos (find-starting-pipe lines)))
			(print came-from)
			(print pos)
			(print (get-pipe lines pos))
			(mark-left-right pos came-from map)
			(setf (char (nth (second pos) map) (first pos)) #\#)
			(loop
				(let ((new-pos (get-new-pos pos came-from lines)))
					(setf (char (nth (second new-pos) map) (first new-pos)) #\#)
					(mark-left-right pos came-from map)
					(when (char= #\S (get-pipe lines new-pos))
						(return-from mark-loop))
					(setf came-from pos)
					(setf pos new-pos)))))

(defun grow-R (map)
	(loop for y from 0 below (list-length map) do
		(loop for x from 0 below (length (first map)) do
			(when (char= (map-get map (list x y)) #\R)
				(loop for offset in offsets do
					(when (char= (map-get map (add-offset offset (list x y))) #\.)
						(setf (char (nth (+ y (second offset)) map) (+ x (first offset))) #\R)
						(return-from grow-R t))))))
	nil)

(defun count-R (map)
	(apply '+
		(mapcar
			(lambda (line)
				(count #\R line))
			map)))

(defun solve2 (lines)
	(let ((map (create-map lines)))
		(mark-loop lines map)
		(loop
			(when (not (grow-R map))
				(return))
			(loop for line in map do
				(print line)))
		(count-R map)))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))

	;(print (solve1 lines)))
	(print (solve2 lines)))
