;;https://adventofcode.com/2020/day/2

(defun validate-password (min max policy password)
  "Validates that a password has at least min and no more than max occurences of the character in policy"
  (let ((count 0))
    (dotimes (i (length password))
      (if (char-equal (coerce policy 'character) (elt password i))
	  (incf count)))
    (and (>= count min) (<= count max))))

(defun validate-password-2 (pos1 pos2 policy password)
  "Validates that the character in policy is exclusively in pos1 or pos2 of password"
  (let ((policy-char (coerce policy 'character))
	(test1 nil) (test2 nil))
    (setf test1 (char-equal policy-char (elt password (- pos1 1))))
    (setf test2 (char-equal policy-char  (elt password (- pos2 1))))
    (and (or test1 test2) (not (and test1 test2))))))

(defun read-input (filename)
"Read input from file into a list"
  (let ((input nil))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
         while line do (push line input)))
    input))


(ql:quickload "cl-ppcre")

(defun parse-policy (input)
"Parses the input string using cl-ppcre for regex support"
(ppcre:register-groups-bind
    ((#'parse-integer min max) policy password)
    ("(\\d+)-(\\d+) (\\w): (.*)" input :sharedp t)
  (list min max policy password)))

(defun advent-002-main (filename password-validator-fun)
"Returns a count of valid passwords in filename according to the validator function used"
(let ((count 0) (passwords (read-input filename)))
  (dolist (input passwords)
    (if (apply password-validator-fun (parse-policy input))
	(incf count)))
  count))

(advent-002-main "input.txt" #'validate-password)
(advent-002-main "input.txt" #'validate-password-2)
