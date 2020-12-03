;;https://adventofcode.com/2020/day/1

;;takes two numbers and returns true if they sum to target, else false
(defun advent-001p (num1 num2 target)
  (equal (+ num1 num2) target))

(defun advent-001p-list (numbers target)
  (equal (reduce #'+ numbers) target))

;;takes a list of numbers and a target value and finds the two numbers on the list that sum to the target
(defun advent-001 (numbers target)
  (loop named outer for i from 0 to (- (length numbers) 2)
     do (loop for j from (+ i 1) to (- (length numbers) 1)
	   do (if (advent-001p (elt numbers i) (elt numbers j) target)
		  (return-from outer  (list (elt numbers i) (elt numbers j)))))))

(defun util-combinations (list)
  "Takes a list and returns every 2 item combination that exists in the list"
  (cond
    ;; terminating condition
    ;;((equal 2 (length list)) (list list))
    ((equal 1 (length list)) nil)
    ;; do something with the first element of the list
    (t
     (let ((new-list nil))
       (dolist (item (rest list)) (push (list (first list) item) new-list))
       ;;recur with the rest of the list
       (append new-list (util-combinations (rest list)))))))

(defun advent-001-list (numbers target)
  (dolist (combo numbers)
    (if (advent-001p-list combo target) (return combo))))

(defun read-file-to-list (filename)
  (let ((numbers nil))
    (with-open-file (in filename)
      (loop for line = (read in nil)
	 while line do (push line numbers)))
    numbers))

(defun advent-001-main (filename target)
  (reduce #'* (advent-001-list (util-combinations (read-file-to-list filename)) target))))
