(ql:quickload "uiop")
(ql:quickload "cl-ppcre")

(defparameter *test-data* '((#\a #\b #\c) (#\a #\b #\c) (#\a #\b #\a #\c) (#\a #\a #\a #\a) (#\b)))

(defparameter *test-strings* '("abc" "abc" "abac" "aaaa" "b"))

(defun main (filename)
  (reduce #'+ (map 'list #'pull-uniques-from-list (map 'list #'string-to-list (read-input filename)))))


(defun read-input (filename)
  (let ((input nil))
    (setf input (uiop:split-string (ppcre::regex-replace-all "\\n" (ppcre:regex-replace-all "\\n\\n" (uiop:stripln (uiop:read-file-string filename)) "===") "") :separator "==="))
    (remove-if (lambda (x) (equal x "")) input)))

(defun string-to-list (str)
  (loop for n from 0 to (- (length str) 1)
	collecting (char str n)))

(defun pull-uniques-from-list (list &optional uniques)
  (cond
    ;;base case
    ((eq 0 (length list))
     (length uniques))
    ;;recurision
    ((null (find (first list) uniques))
     (pull-uniques-from-list (rest list) (push (first list) uniques)))
    (t (pull-uniques-from-list (rest list) uniques))))
