(defconstant width 140)
(defconstant height 140)

(defun is-number (character)
	(if (eq character nil)
		nil
		(digit-char-p character)))

(defun is-symbol (character)
	(if (eq character nil)
		nil
		(and (not (eq character #\.)) (not (digit-char-p character)))))

(defun set-char (lines x y character)
	(cond
		((< x 0)      lines)
		((>= x width) lines)
		((< y 0)      lines)
		((>= y height) lines)
		(t
			(setf (char (nth y lines) x) character)
			lines)))

(defun get-char (lines x y)
	(cond
		((< x 0)      nil)
		((>= x width) nil)
		((< y 0)      nil)
		((>= y height) nil)
		(t
			(char (nth y lines) x))))

(defun filter-symbol (lines x y propagated)
	(cond
		((is-symbol (get-char lines x y))
			(setf lines (set-char lines x y #\.))
			(filter-symbol lines (+ x 1) y t)
			(filter-symbol lines (- x 1) y t)
			(filter-symbol lines x (+ y 1) t)
			(filter-symbol lines x (- y 1) t)
			(filter-symbol lines (+ x 1) (+ y 1) t)
			(filter-symbol lines (- x 1) (+ y 1) t)
			(filter-symbol lines (+ x 1) (- y 1) t)
			(filter-symbol lines (- x 1) (- y 1) t))
		((and propagated (is-number (get-char lines x y))
			(setf lines (set-char lines x y #\.))
			(filter-symbol lines (+ x 1) y t)
			(filter-symbol lines (- x 1) y t))))
	lines)

(defun remove-symbols (lines)
	(loop for x from 0 to width do
		(loop for y from 0 to height do
			(when (is-symbol (get-char lines x y))
				(setf lines (set-char lines x y #\.)))))
	lines)
	
(defun filter-symbols (lines)
	(loop for x from 0 to width do
		(loop for y from 0 to height do
			(filter-symbol lines x y nil)))
	lines)

(defun filter-dots (lines)
	(mapcar
		(lambda (line)
			(substitute #\SPACE #\. line))
		lines))

(defun sum-numbers-line (line)
	(apply '+ (read-from-string (concatenate 'string "(" (concatenate 'string line ")")))))

(defun sum-numbers (lines)
	(apply '+
		(mapcar 'sum-numbers-line lines)))

(defun deep-copy (lines)
	(mapcar 'copy-seq lines))
	
(defun solve1 (lines)
	(-
		(sum-numbers
			(filter-dots
				(remove-symbols (deep-copy lines))))
		(sum-numbers
			(filter-dots
				(filter-symbols lines)))))

(defun get-numbers-around (lines x y)
	(list
		(get-char lines (- x 1) y)
		(get-char lines    x    y)
		(get-char lines (+ x 1) y)))

(defun unique-numbers-around (lines x y)
	(let ((numbers (get-numbers-around lines x y)))
		(cond
			((and
				(is-number (nth 0 numbers))
				(not (is-number (nth 1 numbers)))
				(is-number (nth 2 numbers)))
					2)
			((some 'is-number numbers)
				1)
			(t 0))))

(defun get-number (lines x y)
	; shift to the first digit
	(loop while (is-number (get-char lines (- x 1) y)) do
		(setf x (- x 1)))
	(parse-integer (nth y lines) :start x :junk-allowed t))

(defun calculate-gear (lines x y)
	(let ((result 1))
		(block loops
			(loop for y2 from (+ y 1) downto (- y 1) do
				(loop for x2 from (+ x 1) downto (- x 1) do
					(when (is-number (get-char lines x2 y2))
							(setf result (* result (get-number lines x2 y2)))
							(return-from loops)))))
		(block loops
			(loop for y2 from (- y 1) to (+ y 1) do
				(loop for x2 from (- x 1) to (+ x 1) do
					(when (is-number (get-char lines x2 y2))
							(setf result (* result (get-number lines x2 y2)))
							(return-from loops)))))
		result))

(defun check-gear (lines x y)
	(if (eq (+
				(unique-numbers-around lines x y)
				(unique-numbers-around lines x (+ y 1))
				(unique-numbers-around lines x (- y 1)))
			2)
		(calculate-gear lines x y)
		;; else
		0))

(defun solve2 (lines)
	(let ((sum 0))
		(loop for x from 0 to width do
			(loop for y from 0 to height do
				(when (eq (get-char lines x y) #\*)
					(setf sum (+ sum (check-gear lines x y))))))
		sum))



(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))

	;(print (solve1 lines))
	(print (solve2 lines)))
