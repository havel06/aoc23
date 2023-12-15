(load #p"/usr/share/common-lisp/source/cl-asdf/asdf.lisp")

(defun parse-input (line)
	(uiop:split-string line :separator ","))

(defun hash-string (str)
	(let ((res 0))
		(loop for c across str do
			(incf res (char-code c))
			(setf res (mod (* res 17) 256)))
		res))

(defun solve1 (line)
	(apply '+
		(mapcar
			'hash-string
			(parse-input line))))

(defun get-label (instr sep)
	(subseq instr 0 (position sep instr)))

(defun focal-len (instr)
	(parse-integer (subseq instr (+ 1 (position #\= instr)))))

(defun add-lens (instr boxes)
	(let ((hash (hash-string (get-label instr #\=))))
		(loop for lens in (nth hash boxes) do
			(when (string= (first lens) (get-label instr #\=))
				(setf (second lens) (focal-len instr))
				(return-from add-lens)))
		(setf (nth hash boxes) (nconc (nth hash boxes) (list (list (get-label instr #\=) (focal-len instr)))))))

(defun remove-lens (instr boxes)
	(let ((hash (hash-string (get-label instr #\-))))
		(setf
			(nth hash boxes)
			(remove-if
				(lambda (lens)
					(string= (first lens) (get-label instr #\-)))
				(nth hash boxes)))))

(defun box-focus-power (boxes index)
	(loop for lens in (nth index boxes) for lens-index from 1 sum
		(*
			(+ 1 index)
			lens-index
			(second lens))))

(defun solve2 (line)
	(let ((boxes (make-list 256 :initial-element (list))))
		(loop for instr in (parse-input line) do
			(if (find #\= instr)
				(add-lens instr boxes)
				(remove-lens instr boxes)))
		(loop for i from 0 to 255 sum
			(box-focus-power boxes i))))

;(print (solve1 (read-line)))
(print (solve2 (read-line)))
