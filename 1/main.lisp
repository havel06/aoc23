(defun first-digit (line)
	(digit-char-p (find-if #'digit-char-p line))
)

(defun last-digit (line)
	(digit-char-p (find-if #'digit-char-p line :from-end t))
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
