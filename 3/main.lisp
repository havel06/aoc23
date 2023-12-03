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
		((>= y width) lines)
		(t
			(setf (char (nth y lines) x) character)
			lines)))

(defun get-char (lines x y)
	(cond
		((< x 0)      nil)
		((>= x width) nil)
		((< y 0)      nil)
		((>= y width) nil)
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

(defvar lines (list))

(loop
	(let ((line (read-line *standard-input* nil nil))) (progn
		(when (eq line nil) (return))
		(setf lines (append lines (list line))))))

(print (solve1 lines))
