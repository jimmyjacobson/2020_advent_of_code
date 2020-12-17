(in-package :com.jimmyjacobson.aoc2020)

(defclass passport ()
  ((birth-year
    :initarg :birth-year
    :initform (error "Birth year required"))
   (issue-year
    :initarg :issue-year
    :initform (error "Issue year required"))
   (expiration-year
    :initarg :expiration-year
    :initform (error "Expiration year required"))
   (height
    :initarg :height
    :initform (error "Height required"))
   (hair-color
    :initarg :hair-color
    :initform (error "Hair color required"))
   (eye-color
    :initarg :eye-color
    :initform (error "Eye color"))
   (passport-id
    :initarg :passport-id
    :initform (error "Passport id required"))
   (country-id
    :initarg :country-id)))

(defparameter *passports* nil)

(defun make-passport (&key byr
			iyr
			eyr
			hgt
			hcl
			ecl
			pid
			cid)
  (make-instance 'passport
		 :birth-year byr
		 :issue-year iyr
		 :expiration-year eyr
		 :height hgt
		 :hair-color hcl
		 :eye-color ecl
		 :passport-id pid
		 :country-id cid))

(defgeneric valid-passport-p (passport)
  "Generic method for validating passports")

(defmethod valid-passport-p (passport)
  (with-slots (birth-year
		     issue-year
		     expiration-year
		     height
		     hair-color
		     eye-color
		     passport-id)
      passport
    (and (not (null birth-year))
	 (not (null issue-year))
	 (not (null expiration-year))
	 (not (null height))
	 (not (null hair-color))
	 (not (null eye-color))
	 (not (null passport-id)))))

(defun print-passport (passport)
  (with-slots (birth-year issue-year expiration-year height hair-color eye-color passport-id country-id) passport
      (format t "~a~%" birth-year)))

(defun read-char-stream-file (filename)
  (let ((str (make-adjustable-string ""))
	(strs '())
	(newline-count 0))
    (with-open-file (in filename)
      (loop for char = (read-char in nil)
	 while char do
	     (cond
	       ;; if newline count is 2 then reset str to empty
	       ((<= 2 newline-count)
		(push (string-trim " " str) strs)
		(setf newline-count 0)
		(setf str (make-adjustable-string ""))
		(vector-push-extend char str))
	       ;; keep track of consecutive newlines
               ((char-equal '#\NewLine char)
                (vector-push-extend '#\Space str)
                (incf newline-count))
	       ;; default is to push char to str
	       (t
		(vector-push-extend char str)
		(setf newline-count 0)))))
    strs))
;; For some reason, the character of new strings is getting cut off.

(defun make-adjustable-string (s)
  (make-array (length s)
	      :fill-pointer (length s)
	      :adjustable t
	      :initial-contents s
	      :element-type (array-element-type s)))

(defun parse-passport (passport-string)
  "parse parameters for a passport from a string of format 'byr: iyr: etc'"
  )

(defparameter *test-data* '("byr:2024" "iyr:2016" "eyr:2034"  "pid:985592671" "hcl:033b48" "hgt:181" "cid:166"))

(ql:quickload "cl-ppcre")
(defparameter *test-params*
  (loop for passport in *test-data*
       do (print passport)
    collect (mapcar (lambda (x)
	      (ppcre:register-groups-bind (a b) ("(\\w+):(\\w+)" x)
		(list (read-from-string (concatenate 'string ":" a))  b)))
	    (cl-ppcre:split "\\s" passport))))

(defparameter *p* (apply #'make-passport (flatten-list (first *test-params*))))

(defun flatten-list (lst)
  (print lst)
  (cond
    ;;terminating case - lst is empty
    ((null lst) nil)
    ;;terminating case - lst is an atom, return the atom in a list for appending
    ((atom lst) (list lst))
    ;;do something with first of list, and recur on rest
    (t (append (flatten-list (first lst)) (flatten-list (rest lst))))))
