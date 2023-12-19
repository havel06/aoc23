(load #p"/usr/share/common-lisp/source/cl-asdf/asdf.lisp")

(defun is-comparison (char)
	(or
		(char= char #\<)
		(char= char #\>)))

(defun parse-complicated-rule (str)
	(list
		(subseq str 0 (position-if 'is-comparison str)) ;param name
		(string (char str (position-if 'is-comparison str))) ;comparison operator
		(subseq str (+ 1 (position-if 'is-comparison str)) (position #\: str)) ;condition number
		(subseq str (+ 1 (position #\: str))))) ;next workflow

(defun parse-rule (str)
	(if (find #\: str)
		(parse-complicated-rule str)
		(list str)))

(defun parse-rules (str)
	(mapcar 'parse-rule (uiop:split-string str :separator ",")))

(defun parse-workflow (line)
	(list
		(subseq line 0 (position #\{ line))
		(parse-rules (subseq line (+ 1 (position #\{ line)) (position #\} line)))))

(defun parse-workflows (lines)
	(loop for line in lines while (not (string= line "")) collect
		(parse-workflow line)))

(defun parse-part (line)
	(read-from-string
		(concatenate 'string
			"("
			(substitute-if
				#\space
				(lambda (c)
					(not (digit-char-p c)))
				line)
			")")))

(defun parse-parts (lines)
	(loop for i from 0 do
		(when (string= (nth i lines) "")
			(return-from parse-parts (mapcar 'parse-part (nthcdr (+ 1 i) lines))))))

(defun find-workflow (workflows name)
	(loop for flow in workflows do
		(when (string= (first flow) name)
			(return-from find-workflow flow))))

(defun get-property (part property)
	(cond
		((string= property "x") (first part))
		((string= property "m") (second part))
		((string= property "a") (third part))
		((string= property "s") (fourth part))))

(defun rule-applies (rule part)
	(if (eq (list-length rule) 1)
		t
		(progn
			(eval
				(read-from-string
					(concatenate 'string
						"("
						(second rule) ;operator
						" "
						(write-to-string (get-property part (first rule))) ;property value
						" "
						(third rule) ;comparison value
						")"))))))

(defun apply-rule (workflows rule part)
	(cond
		((string= #\A (car (last rule))) t)
		((string= #\R (car (last rule))) nil)
		(t (apply-workflow
			 workflows
			(find-workflow workflows (car (last rule)))
			part))))

(defun apply-workflow (workflows workflow part)
	;(print (list "apply" workflow "to" part))
	(loop for rule in (second workflow) do
		(when (rule-applies rule part)
			(return-from apply-workflow (apply-rule workflows rule part)))))

(defun process-part (workflows part)
	(apply-workflow workflows (find-workflow workflows "in") part))

(defun part-value (part)
	(apply '+ part))

(defun solve1 (lines)
	(let (
		(flows (parse-workflows lines))
		(parts (parse-parts lines)))
			(loop for part in parts sum
				(if (process-part flows part)
					(part-value part)
					0))))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))
	(print (solve1 lines)))
	;(print (solve2 lines)))
