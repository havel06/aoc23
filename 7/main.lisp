(defun five-of-a-kind (hand)
	(loop for card in (rest hand) do
		(when (not (eq card (first hand)))
			(return-from five-of-a-kind nil)))
	t)

(defun four-of-a-kind (hand)
	(loop for card in hand do
		(when (eq 4 (count-if (lambda (c) (eq c card)) hand))
			(return-from four-of-a-kind t)))
	nil)

(defun three-of-a-kind (hand)
	(loop for card in hand do
		(when (eq 3 (count-if (lambda (c) (eq c card)) hand))
			(return-from three-of-a-kind t)))
	nil)


(defun one-pair-excluding (hand excluding)
	(loop for card in hand do
		(when (and (or (not excluding) (not (char= card excluding))) (eq 2 (count-if (lambda (c) (eq c card)) hand)))
			(return-from one-pair-excluding t)))
	nil)


(defun one-pair (hand)
	(one-pair-excluding hand nil))

(defun full-house (hand)
	(loop for card in hand do
		(when (eq 3 (count-if (lambda (c) (eq c card)) hand))
			(when (one-pair-excluding hand card)
				(return-from full-house t))))
	nil)

(defun two-pair (hand)
	(loop for card in hand do
		(when (eq 2 (count-if (lambda (c) (eq c card)) hand))
			(when (one-pair-excluding hand card)
				(return-from two-pair t))))
	nil)


(defun rank (hand)
	(cond
		((five-of-a-kind hand) 6)
		((four-of-a-kind hand) 5)
		((full-house hand) 4)
		((three-of-a-kind hand) 3)
		((two-pair hand) 2)
		((one-pair hand) 1)
		(t 0)))

(defun parse-line (line)
	(list
		(coerce (subseq line 0 (position #\space line)) 'list)
		(parse-integer (subseq line (position #\space line)))))

(defun parse-lines (lines)
	(mapcar 'parse-line lines))

(defun svalue (sym)
	(cond
		((char= sym #\1) 1)
		((char= sym #\2) 2)
		((char= sym #\3) 3)
		((char= sym #\4) 4)
		((char= sym #\5) 5)
		((char= sym #\6) 6)
		((char= sym #\7) 7)
		((char= sym #\8) 8)
		((char= sym #\9) 9)
		((char= sym #\T) 10)
		((char= sym #\J) 11)
		((char= sym #\Q) 12)
		((char= sym #\K) 13)
		((char= sym #\A) 14)))

(defun compare-equal-hands (hand1 hand2)
	(loop for i from 0 to 4 do
		(cond
			((< (svalue (nth i hand1)) (svalue (nth i hand2)))
				(return-from compare-equal-hands t))
			((> (svalue (nth i hand1)) (svalue (nth i hand2)))
				(return-from compare-equal-hands nil))))
	t)

(defun compare-hands (hand1 hand2)
	(let ((rank1 (rank hand1)) (rank2 (rank hand2)))
		(cond
			((< rank1 rank2)
				t)
			((> rank1 rank2)
				nil)
			(t
				(compare-equal-hands hand1 hand2)))))

(defun solve1 (lines)
	(let ((parsed (parse-lines lines)) (res 0))
		(sort parsed 'compare-hands :key 'first)
		(print parsed)
		(loop for i from 0 below (list-length parsed) do
			(setf res (+ res (* (+ 1 i) (second (nth i parsed))))))
		res))
	

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))

	(print (solve1 lines)))
	;(print (solve2 lines)))
