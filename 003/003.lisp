;;https://adventofcode.com/2020/day/3

(defun read-input (filename)
"Read a file into a list, one line per list"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun convert-list-to-array (list)
  "Convert a list to an array, the hard way"
  (make-array '(323 31) :initial-contents list))

(defun traverse-array (array startX startY deltaX deltaY)
  "Return the element after traversal"
  (aref array (+ startX deltaX) (+ startY deltaY)))

(defun traverse-array-2 (array slopeY slopeX)
  "Traverse the slopes using the provided array and directions and count trees"
  (let ((x slopeX) (count 0))
    (loop
       for y from slopeY to 322
       do (print (aref array y x))
	 (if (char-equal #\# (aref array y x)) (incf count))
	 (setf x (+ x slopeX))
	 (if (> x 30) (setf x (- x 31)))
	 (if (> slopeY 1) (setf y (+ y (- slopeY 1)))))
    count))

(defun advent-003-main (filename slopeY slopeX)
"Glue it all together"
  (traverse-array-2 (convert-list-to-array (read-input filename)) slopeY slopeX))

;;Answer to the bonus problem
(defparameter *slopes* '(("input.txt" 1 1) ("input.txt" 1 3) ("input.txt" 1 5) ("input.txt" 1 7) ("input.txt" 2 1)))
(reduce #'* (mapcar (lambda (x) (apply #'advent-003-main x)) *slopes*))
