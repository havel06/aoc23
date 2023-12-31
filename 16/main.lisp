(defun get-char (lines x y)
	(cond
		((< x 0) nil)
		((< y 0) nil)
		((>= x (length (first lines))) nil)
		((>= y (list-length lines)) nil)
		(t (char (nth y lines) x))))

(defun is-horiz (x y)
	(eq y 0))

(defun is-vert (x y)
	(eq x 0))

(defun mask-get (mask x y)
	(char (nth y mask) x))

(defun mask-write (mask x y dirx diry)
	(let ((c (mask-get mask x y)))
		(cond
			((eq dirx 1)
				(cond
					((char= c #\.)
						(setf (char (nth y mask) x) #\>) t)
					((char= c #\<)
						(setf (char (nth y mask) x) #\-) t)
					((char= c #\V)
						(setf (char (nth y mask) x) #\F) t)
					((char= c #\A)
						(setf (char (nth y mask) x) #\L) t)
					((char= c #\7)
						(setf (char (nth y mask) x) #\#) t)
					((char= c #\J)
						(setf (char (nth y mask) x) #\#) t)
					((char= c #\|)
						(setf (char (nth y mask) x) #\#) t)
					(t nil)))
			((eq dirx -1)
				(cond
					((char= c #\.)
						(setf (char (nth y mask) x) #\<) t)
					((char= c #\>)
						(setf (char (nth y mask) x) #\-) t)
					((char= c #\V)
						(setf (char (nth y mask) x) #\7) t)
					((char= c #\A)
						(setf (char (nth y mask) x) #\J) t)
					((char= c #\F)
						(setf (char (nth y mask) x) #\#) t)
					((char= c #\L)
						(setf (char (nth y mask) x) #\#) t)
					((char= c #\|)
						(setf (char (nth y mask) x) #\#) t)
					(t nil)))
			((eq diry 1)
				(cond
					((char= c #\.)
						(setf (char (nth y mask) x) #\V) t)
					((char= c #\A)
						(setf (char (nth y mask) x) #\|) t)
					((char= c #\>)
						(setf (char (nth y mask) x) #\F) t)
					((char= c #\<)
						(setf (char (nth y mask) x) #\7) t)
					((char= c #\L)
						(setf (char (nth y mask) x) #\#) t)
					((char= c #\J)
						(setf (char (nth y mask) x) #\#) t)
					((char= c #\-)
						(setf (char (nth y mask) x) #\#) t)
					(t nil)))
			((eq diry -1)
				(cond
					((char= c #\.)
						(setf (char (nth y mask) x) #\A) t)
					((char= c #\V)
						(setf (char (nth y mask) x) #\|) t)
					((char= c #\>)
						(setf (char (nth y mask) x) #\L) t)
					((char= c #\<)
						(setf (char (nth y mask) x) #\J) t)
					((char= c #\7)
						(setf (char (nth y mask) x) #\#) t)
					((char= c #\F)
						(setf (char (nth y mask) x) #\#) t)
					((char= c #\-)
						(setf (char (nth y mask) x) #\#) t)
					(t nil))))))

(defun process (mask lines x y dirx diry)
	;(print (mask-count mask))
	(let ((current (get-char lines x y)))
		(when (not current)
			(return-from process))

		(when (not (mask-write mask x y dirx diry))
			(return-from process))

		(cond
			((char= current #\.)
				(process mask lines (+ x dirx) (+ y diry) dirx diry))
			((char= current #\|)
				(if (is-vert dirx diry)
					(process mask lines x (+ y diry) 0 diry)
					(progn
						(process mask lines x (+ y 1) 0 1)
						(process mask lines x (- y 1) 0 -1))))
			((char= current #\-)
				(if (is-horiz dirx diry)
					(process mask lines (+ x dirx) y dirx 0)
					(progn
						(process mask lines (+ x 1) y 1 0)
						(process mask lines (- x 1) y -1 0))))
			((char= current #\/)
				(cond
					((and (eq dirx 1) (eq diry 0))
						(process mask lines x (- y 1) 0 -1))
					((and (eq dirx -1) (eq diry 0))
						(process mask lines x (+ y 1) 0 1))
					((and (eq dirx 0) (eq diry -1))
						(process mask lines (+ x 1) y 1 0))
					((and (eq dirx 0) (eq diry 1))
						(process mask lines (- x 1) y -1 0))))
			((char= current #\\)
				(cond
					((and (eq dirx 1) (eq diry 0))
						(process mask lines x (+ y 1) 0 1))
					((and (eq dirx -1) (eq diry 0))
						(process mask lines x (- y 1) 0 -1))
					((and (eq dirx 0) (eq diry -1))
						(process mask lines (- x 1) y -1 0))
					((and (eq dirx 0) (eq diry 1))
						(process mask lines (+ x 1) y 1 0)))))))

(defun make-mask (lines)
	(loop for i from 0 below (list-length lines) collect (make-string (length (first lines)) :initial-element #\.)))

(defun mask-count (mask)
	(apply '+
		(mapcar
			(lambda (line)
				(count-if (lambda (c) (not (char= c #\.))) line))
			mask)))	


(defun solve1 (lines)
	(let ((mask (make-mask lines)))
		(process mask lines 0 0 1 0)
		(loop for line in mask do
			(write-line line))
		(mask-count mask)))

(defun configs (width height)
	(append
		(loop for x from 0 below width collect
			(list x 0 0 1))
		(loop for x from 0 below width collect
			(list x (- height 1) 0 -1))
		(loop for y from 0 below height collect
			(list 0 y 1 0))
		(loop for y from 0 below height collect
			(list (- width 1) y -1 0))))

(defun solve2 (lines)
	(apply 'max
		(mapcar
			(lambda (config)
				(let ((mask (make-mask lines)))
					(process mask lines (nth 0 config) (nth 1 config) (nth 2 config) (nth 3 config))
					(mask-count mask)))
			(configs (length (first lines)) (list-length lines)))))

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))
	;(print (solve1 lines)))
	(print (solve2 lines)))
