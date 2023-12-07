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

(defun most-common-card (hand)
	(let (
		(res (first hand))
		(n 1))
			(loop for card in hand do
				(when
					(and
						(not (char= card #\J))
						(or
							(> (count card hand) n)
							(and
								(eq (count card hand) n)
								(> (svalue card) (svalue res)))))
					(setf res card)
					(setf n (count card hand))))
			res))

(defun swap-jokers (hand)
	(substitute (most-common-card hand) #\J hand))


(defun rank (hand)
	(when (< 0 (count #\J hand))
		(setf hand (swap-jokers hand)))

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
		((char= sym #\J) 1)
		((char= sym #\1) 2)
		((char= sym #\2) 3)
		((char= sym #\3) 4)
		((char= sym #\4) 5)
		((char= sym #\5) 6)
		((char= sym #\6) 7)
		((char= sym #\7) 8)
		((char= sym #\8) 9)
		((char= sym #\9) 10)
		((char= sym #\T) 11)
		((char= sym #\Q) 13)
		((char= sym #\K) 14)
		((char= sym #\A) 15)))

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

(defun solve2 (lines)
	(let ((parsed (parse-lines lines)) (res 0))
		;(loop for hand in parsed do
		;	(print (first hand))
		;	(print (most-common-card (first hand))))))
		(sort parsed 'compare-hands :key 'first)
		(loop for i from 0 below (list-length parsed) do
			(setf res (+ res (* (+ 1 i) (second (nth i parsed))))))
		res))
	

(let (lines (list))
	(loop
		(let ((line (read-line *standard-input* nil nil))) (progn
			(when (eq line nil) (return))
			(setf lines (append lines (list line))))))

	(print (solve2 lines)))
