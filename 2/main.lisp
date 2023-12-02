(load #p"/usr/share/common-lisp/source/cl-asdf/asdf.lisp")

(defun split-to-sets (line)
	(uiop:split-string (subseq line (+ 1 (position #\: line))) :separator ";"))

(defun split-set (set-str)
	(uiop:split-string set-str :separator ","))

(defun count-valid (count)
	(let (
		(num (parse-integer (nth 0 count)))
		(colour (nth 1 count)))
			(or
				(and (string= colour "red")   (<= num 12))
				(and (string= colour "green") (<= num 13))
				(and (string= colour "blue")  (<= num 14)))))

(defun parse-count (count-str)
	(uiop:split-string (subseq count-str 1) :separator " "))

(defun count-valid-str (count-str)
	(count-valid (parse-count count-str)))

(defun set-valid (set)
	(every 'count-valid-str (split-set set)))

(defun game-valid (line)
	(every 'set-valid (split-to-sets line)))

(defun game-id-str (line)
	(subseq line 5 (position #\: line)))

(defun game-id (line)
	(parse-integer (game-id-str line)))

(defun colour-count-in-set (set colour)
	(let ((counts (mapcar 'parse-count (split-set set))) (result 0))
		(loop for count in counts do
			(when (string= colour (nth 1 count))
				(progn
					(setf result (parse-integer (nth 0 count)))
					(return))))
		result))

(defun min-colour (sets colour)
	(apply 'max
		(mapcar
			(lambda (set) (colour-count-in-set set colour))
			sets)))

(defun game-power (line)
	(let ((sets (split-to-sets line)))
		(*
			(min-colour sets "red")
			(min-colour sets "green")
			(min-colour sets "blue"))))

(defvar sum 0)

(loop
	(let ((line (read-line *standard-input* nil nil))) (progn
		(when (eq line nil) (return))
		;; solution 1
		;(when (game-valid line)
		;	(setq sum (+ sum (game-id line)))))))
		;; solution 2
		(setq sum (+ sum (game-power line))))))

(write-line (write-to-string sum))
