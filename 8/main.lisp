(defun parse-node (line)
	(list
		(subseq line 0 (position #\space line))
		(subseq line (+ 1 (position #\( line)) (position #\, line))
		(subseq line (+ 2 (position #\, line)) (position #\) line))))

(defun parse-nodes (lines)
	(mapcar 'parse-node lines))

(defun find-node (name nodes)
	(loop for node in nodes do
		(when (string= (first node) name)
			(return-from find-node node))))

(defun go-left (node nodes)
	(find-node (second node) nodes))
	
(defun go-right (node nodes)
	(find-node (third node) nodes))

(defun do-instruction (node nodes instruction)
	(if (char= instruction #\L)
		(go-left node nodes)
		(go-right node nodes)))

(defun solve1 (lines)
	(let (
			(directions (first lines))
			(nodes (parse-nodes (rest (rest lines))))
			(steps 0))
		(let ((node (find-node "AAA" nodes)))
			(loop
				(loop for instr across directions do
					(setf node (do-instruction node nodes instr))
					(print node)
					(incf steps)
					(when (string= (first node) "ZZZ")
						(return-from solve1 steps)))))))

(defun starting-nodes (nodes)
	(remove-if-not
		(lambda (node)
			(eq (char (first node) (- (length (first node)) 1)) #\A))
		nodes))

(defun finished (node)
	(eq (char (first node) (- (length (first node)) 1)) #\Z))

(defun dist-for-node (node nodes directions)
	(let ((steps 0))
		(loop
			(loop for instr across directions do
				(setf node (do-instruction node nodes instr))
				(incf steps)
				(when (finished node)
					(print (list node steps))
					(return-from dist-for-node steps))))))
	

(defun solve2 (lines)
	(let (
			(directions (first lines))
			(nodes (parse-nodes (rest (rest lines))))
			(steps 0))
		(let ((current-nodes (starting-nodes nodes)))
			(apply 'lcm
				(mapcar
					(lambda (node)
						(dist-for-node node nodes directions))
					current-nodes)))))


(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))

	;(print (solve1 lines)))
	(print (solve2 lines)))
