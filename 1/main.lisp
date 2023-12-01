(defun first-digit (line backwards)
	(values
		(position-if #'digit-char-p line :from-end backwards)
		(digit-char-p (find-if #'digit-char-p line :from-end backwards))
	)
)

(defun word-positions (line backwards)
	(list
		(search "one" line :from-end backwards)
		(search "two" line :from-end backwards)
		(search "three" line :from-end backwards)
		(search "four" line :from-end backwards)
		(search "five" line :from-end backwards)
		(search "six" line :from-end backwards)
		(search "seven" line :from-end backwards)
		(search "eight" line :from-end backwards)
		(search "nine" line :from-end backwards)
	)
)

(defun pos-comp-operator (backwards)
	(if backwards
		'>
		'<
	)
)

(defun first-word (line backwards)
	(let ((positions (word-positions line backwards)) (min-index nil) (min-number nil))
		(progn
			(loop for i from 0 to 8 do
				(cond
					((eq (nth i positions) nil) ())
					((or
						(eq min-index nil)
						(funcall (pos-comp-operator backwards) (nth i positions) min-index)
					)
						(setq min-number (+ i 1))
						(setq min-index (nth i positions))
					)
				)
			)
			(values min-index min-number)
		)
	)
)

(defun process-line (line)
	(multiple-value-bind (first-digit-pos first-digit-value) (first-digit line nil)
	(multiple-value-bind (last-digit-pos last-digit-value) (first-digit line t)
	(multiple-value-bind (first-word-pos first-word-value) (first-word line nil)
	(multiple-value-bind (last-word-pos last-word-value) (first-word line t)
	(let ((res 0)) (progn
		(if (and (not (eq first-word-pos nil)) (< first-word-pos first-digit-pos))
			(setf res (+ res (* 10 first-word-value)))
			(setf res (+ res (* 10 first-digit-value)))
		)

		(if (and (not (eq last-word-pos nil)) (> last-word-pos last-digit-pos))
			(setf res (+ res last-word-value))
			(setf res (+ res last-digit-value))
		)

		res
	)
	)))))
)

(defvar sum 0)

(loop
	(let ((line (read-line *standard-input* nil nil))) (progn
		(when (eq line nil) (return))
		(setq sum (+ sum (process-line line)))
	))
)

(write-line (write-to-string sum))
