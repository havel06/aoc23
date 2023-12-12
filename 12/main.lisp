(load #p"/usr/share/common-lisp/source/cl-asdf/asdf.lisp")

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

(defun count-unknown (str)
	(count #\? str))

(defun add-symbol-to-combinations (symbol combinations)
	(mapcar
		(lambda (combination)
			(nconc combination (list symbol)))
		combinations))

(defun possible-combinations (n)
	(if (eq n 1)
		(list (list #\.) (list #\#))
		(nconc
			(add-symbol-to-combinations #\. (possible-combinations (- n 1)))
			(add-symbol-to-combinations #\# (possible-combinations (- n 1))))))

(defun apply-combination (str combination)
	(if (not combination)
		str
		(progn
			(setf (char str (position #\? str)) (first combination))
			(apply-combination str (rest combination)))))

(defun count-conseq (str)
	(remove 0
		(mapcar
			'length
			(uiop:split-string str :separator "."))))


(defun line-ok (str numbers)
	(equal numbers (count-conseq str)))

(defun solve-line (line)
	(print line)
	(count-if
		(lambda (applied-combo)
			(line-ok applied-combo (second line)))
		(mapcar
			(lambda (combination)
				(apply-combination (copy-seq (first line)) combination))
			(possible-combinations (count-unknown (first line))))))
	
(defun solve1 (lines)
	(apply '+
		(mapcar 'solve-line (parse-lines lines))))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))
	(print (solve1 lines)))
	;(print (solve2 lines)))
