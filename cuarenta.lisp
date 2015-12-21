;; Cuarenta card game

(defparameter suits (list 'D 'H 'S 'C))

(defparameter face-values (list 1 2 3 4 5 6 7 8 9 10 11 12 13))

(defstruct card (value 1) (suit 1))

(defun new-deck-of-cards ()
	(reduce (lambda (first-list second-list) (append first-list second-list)) 
				(mapcar (lambda (s) 
					(mapcar (lambda (v) 
						(make-card :value v :suit s)) FACE-VALUES)) SUITS)))

(defparameter *table* '())

(defparameter *score* '())

(defun get-hand (player))

(defun place-card-on-table (card))

(defun swap (array index1 index2)
	(let ((value1 (aref array index1)))
		(update-array array index1 (aref array index2))
		(update-array array index2 value1))
		array)

(defun swap-list (list index1 index2)
	(array-to-list (swap (list-to-array list) index1 index2)))

(defun list-to-array (list)
	(make-array (length list) :initial-contents list))

(defun array-to-list (array)
	(coerce array 'list))

(defun update-array (array index value)
	(setf (aref array index) value))  

(defun game-overp (turn)
	(>= turn 10))

; (defun want-to-stealp

(defun deal-hands ()
	(list (deal-cards! 5) (deal-cards! 5)))

(defun deal-cards! (n &optional (deck-of-cards *draw-pile*))
	(if (= n 0)
		'()
		(cons (pop deck-of-cards) (deal-cards! (- n 1) deck-of-cards))))

(defun cards-equal (c1 c2)
	(and (= (card-value c1) (card-value c2))
		 (eq (card-suit c1) (card-suit c2))))

(defun card-matchp (card pile-of-cards)
	(dolist (c pile-of-cards)
		(if (= (card-value c) (card-value card))
			(return t))))

(defun verify-match-when-card-in-pile ()
	(card-matchp (make-card :value 2 :suit 'S) (list (make-card :value 2 :suit 'S))))

(defun verify-no-match-when-card-not-in-pile ()
	(not (card-matchp (make-card :value 2 :suit 'S) (list (make-card :value 3 :suit 'S)))))

(setf functions-to-test (list 'verify-match-when-card-in-pile 'verify-no-match-when-card-not-in-pile))

(defun report-result (result form)
	(format t "~:[FAIL~;pass~] ... ~a~%" result form)
	result)

(defmacro combine-results (&body forms)
  (let ((result (gensym)))
		`(let ((,result t))
			,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
			,result)))

(defmacro check (&body forms)
	`(combine-results
		,@(loop for f in forms collect `(report-result ,f ',f))))

(defun perform-tests ()
	(check
		(verify-match-when-card-in-pile)
		(verify-no-match-when-card-not-in-pile)))

(defun run-multiple-unit-tests (lst)
	(let ((passed 0) (failed 0) (test-result))
	(dolist (test-case lst)
		(setf test-result (eval-unit-test test-case))
		(if (eq 'PASS test-result)
			(setf passed (1+ passed))
			(setf failed (1+ failed)))
		(format t "~a: ~a~%" test-case test-result))
	(format t "______________________~%")
	(format t "Total tests run: ~a~%" (+ passed failed))
	(format t "Total tests passed: ~a~%" passed)
	(format t "Total tests failed: ~a~%" failed)
	(format t "Overall pass/fail: ~a~%" (if (= failed 0) "PASS" "FAIL"))))

(defun eval-unit-test (func)
	(if (funcall func)
		'PASS
		'FAIL))

(defun remove-cards (cards pile-of-cards)
	(dolist (card cards)
		(setf pile-of-cards (remove-if (lambda (c) (cards-equal c card)) pile-of-cards)))
	pile-of-cards)

(defun remove-cards-from-table (cards))
	(remove-cards cards *table*))

(defun place-card (card pile-of-cards)
	(append (list card) pile-of-cards))

(defun place-card-on-table (cards)
	(place-card card *table*))

(defun verify-capture-by-addition (players-card ))

; (defun equals)

; (defun run)

(defun deal-random-cards (n)
	(deal-cards! n (shuffle-deck (new-deck-of-cards))))

(defun sort-cards (cards)
	(sort cards #'<))

(defun prompt-player ()
	(values 'STEAL '(2 2 4)))

(defun shuffle-deck (cards)
	(labels ((shuffle-deck-helper (cards index)
				(if (= index 0)
					cards
					(shuffle-deck-helper (swap-list cards (random index) index) (- index 1)))))
		(shuffle-deck-helper cards (- (length cards) 1))))

(defparameter *draw-pile* (shuffle-deck (new-deck-of-cards)))

(defun game ()
	(let ((hands (deal-hands)))
		(do ((turn 1 (1+ turn)))
			((game-overp turn))
			(turn))))
			
(defun turn ()
	(multiple-value-bind (action cards) (prompt-player)
		(cond 
			((eq action 'STEAL) (progn (setf *table* (remove-cards-from-table cards)) (turn)))
			((eq action 'MATCH) (setf *table* (remove-cards-from-table cards)))
			((eq action 'ADD) (setf *table* (remove-cards-from-table cards)))
			((eq action 'PLACE) (setf *table* (place-card-on-table card))))))

; STEAL 2 2 4 5 6 7
; MATCH 7 J Q
; ADD 5 2 3 6
; PLACE J

; (RUN-MULTIPLE-UNIT-TESTS functions-to-test)
(perform-tests)