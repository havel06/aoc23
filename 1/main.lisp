(defun first-digit (line)
	(loop for char across line do
		(let ((digit (digit-char-p char))) 
			(if (not (eq digit nil))
				(return digit)
				()
			)
		)
	)
)

(defun last-digit (line)
	(let ((digit 0) (last-digit 0)) 
		(loop for char across line do
			(setq digit (digit-char-p char))
			(if (not (eq digit nil))
				(setq last-digit digit)
				()
			)
		)
		last-digit
	)
)

(defun process-line (line)
	(+ (* 10 (first-digit line)) (last-digit line))
)

(defvar sum 0)

(loop
	(let ((line (read-line *standard-input* nil nil))) (progn
		(when (eq line nil) (return))
		(setq sum (+ sum (process-line line)))
	))
)

(write-line (write-to-string sum))
