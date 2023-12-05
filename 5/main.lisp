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
	(loop for mapping in vmap do
		(let ((dest (nth 0 mapping)) (src (nth 1 mapping)) (range (nth 2 mapping)))
			(when (and (>= seed src) (< seed (+ src range)))
				(setf seed (+ dest (- seed src)))
				(return))))
	seed)

(defun apply-maps (seed maps)
	(loop for vmap in maps do
		(setf seed (apply-map seed vmap)))
	seed)

(defun solve1 (lines)
	(let
		((seeds (parse-seeds (nth 0 lines)))
		(maps (parse-maps (rest lines))))
			(apply 'min
				(mapcar (lambda (seed) (apply-maps seed maps)) seeds))))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))

	(print (solve1 lines)))
	;(print (solve2 lines)))
