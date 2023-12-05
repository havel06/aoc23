(declaim (optimize (speed 3) (safety 0)))

(defun parse-seeds (line)
	(read-from-string (concatenate 'string "(" (subseq line (+ 1 (position #\: line))) ")")))

(defun parse-map (lines line-num)
	(let ((vmap (list)))
		(loop while (not (= 0 (length (nth line-num lines)))) do
			(setf vmap (append vmap
				(list (read-from-string (concatenate 'string "(" (nth line-num lines) ")")))))
			(setf line-num (+ 1 line-num)))
		vmap))

(defun parse-maps (lines)
	(let ((maps (list)))
		(loop for i from 0 below (list-length lines) do
			(when (find #\: (nth i lines))
				(setf maps (append maps (list (parse-map lines (+ 1 i)))))))
		maps))

(defun apply-map (seed vmap)
	(declare (type integer seed))
	(loop for mapping in vmap do
		(let ((dest (nth 0 mapping)) (src (nth 1 mapping)) (range (nth 2 mapping)))
			(declare (type integer dest src range))
			(when (and (>= seed src) (< seed (+ src range)))
				(setf seed (+ dest (- seed src)))
				(return))))
	seed)

(defun apply-map-backwards (seed vmap)
	(declare (type integer seed))
	(loop for mapping in vmap do
		(let ((dest (nth 1 mapping)) (src (nth 0 mapping)) (range (nth 2 mapping)))
			(declare (type integer dest src range))
			(when (and (>= seed src) (< seed (+ src range)))
				(setf seed (+ dest (- seed src)))
				(return))))
	seed)

(defun apply-maps (seed maps)
	(loop for vmap in maps do
		(setf seed (apply-map seed vmap)))
	seed)

(defun apply-maps-backwards (seed maps)
	(loop for vmap in (reverse maps) do
		(setf seed (apply-map-backwards seed vmap)))
	seed)

(defun solve1 (lines)
	(let
		((seeds (parse-seeds (nth 0 lines)))
		(maps (parse-maps (rest lines))))
			(apply 'min
				(mapcar (lambda (seed) (apply-maps seed maps)) seeds))))

(defun solve2 (lines)
	(let
		((seeds (parse-seeds (nth 0 lines)))
		(maps (parse-maps (rest lines))))
		(loop for i from 90000000 to 110000000 do
			(let ((seed (apply-maps-backwards i maps)))
				(loop for j from 0 below (/ (list-length seeds) 2) do
					(let ((start (nth (* 2 j) seeds)) (end (+ (nth (* 2 j) seeds) (nth (+ (* j 2) 1) seeds))))
						(when (and (>= seed start) (< seed end))
							(print start)
							(print end)
							(print i)
							(return-from solve2))))))))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))

	;(print (solve1 lines)))
	(solve2 lines))
