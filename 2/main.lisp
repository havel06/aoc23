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

(defun count-valid-str (count-str)
	(count-valid (uiop:split-string (subseq count-str 1) :separator " ")))

(defun set-valid (set)
	(every 'count-valid-str (split-set set)))

(defun game-valid (line)
	(every 'set-valid (split-to-sets line)))

(defun game-id-str (line)
	(subseq line 5 (position #\: line)))

(defun game-id (line)
	(parse-integer (game-id-str line)))

(defvar sum 0)

(loop
	(let ((line (read-line *standard-input* nil nil))) (progn
		(when (eq line nil) (return))
		(when (game-valid line)
			(setq sum (+ sum (game-id line)))))))

(write-line (write-to-string sum))
