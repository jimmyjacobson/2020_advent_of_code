(defclass passport ()
  ((birth-year
    :initarg :byr
    :initform (error "Birth Year Required"))
   (issue-year
    :initarg :iyr
    :initform (error "Issue Year Required"))
   (expiration-year
    :initarg :eyr
    :initform (error "Expiration Year Required"))
   (height
    :initarg :hgt
    :initform (error "Height Required"))
   (hair-color
    :initarg :hcl
    :initform (error "Hair Color Required"))
   (eye-color
    :initarg :ecl
    :initform (error "Eye Color Required"))
   (passport-id
    :initarg :pid
    :initform (error "Passport ID Required"))
   (country-id
    :initarg :cid
    :initform nil)))

(defclass strict-passport (passport)
  ((is-valid
    :initform nil)))

(defun make-passport (&rest params)
  (let ((passport (ignore-errors (apply #'make-instance 'strict-passport params))))
    (if (not (null passport)) (print-pass passport))
    passport))

(defmethod initialize-instance :after ((passport strict-passport) &key)
  (with-slots (birth-year
	       issue-year
	       expiration-year
	       height
	       hair-color
	       eye-color
	       passport-id
	       is-valid)
      passport
    (setf is-valid
	  (and (validate-integer birth-year 1920 2002)
	       (validate-integer issue-year 2010 2020)
	       (validate-integer expiration-year 2020 2030)
	       (validate-height height)
	       (validate-hair-color hair-color)
	       (validate-eye-color eye-color)
	       (validate-passport-id passport-id)))))

(defun validate-integer (data min max)
  (let ((num (parse-integer data)))
    (and num
	 (>= num min)
	 (<= num max))))

(ql:quickload "cl-ppcre")
(defun validate-height (data)
  (ppcre:register-groups-bind (num unit) ("^(\\d+)(in|cm)$" data)
    (cond
      ((string-equal unit "in")
       (validate-integer num 59 76))
       ((string-equal unit "cm")
	(validate-integer num 150 193))
       (t nil))))

(defun validate-hair-color (data)
  (not (null (ppcre:scan "^#[0-9a-f]{6}$" data))))

(defun validate-eye-color (data)
  (not (null (ppcre:scan "^(?:amb|blu|brn|gry|grn|hzl|oth)$" data))))

(defun validate-passport-id (data)
  (not (null (ppcre:scan "^[0-9]{9}$" data))))
			        
(defgeneric print-pass (passport))

(defmethod print-pass (passport)
  (with-slots (birth-year issue-year expiration-year height hair-color eye-color passport-id country-id) passport
    (format t "Birth Year: ~a~%" birth-year)
    (format t "Issue Year: ~a~%" issue-year)
    (format t "Exp Year: ~a~%" expiration-year)
    (format t "Height: ~a~%" height)
    (format t "Hair Color: ~a~%" hair-color)
    (format t "Eye Color: ~a~%" eye-color)
    (format t "Passport Id: ~a~%" passport-id)
    (format t "Country Id: ~a~%~%" country-id)))

(defmethod print-pass ((passport strict-passport))
  (with-slots (birth-year issue-year expiration-year height hair-color eye-color passport-id country-id is-valid) passport
    (format t "Birth Year: ~a~%" birth-year)
    (format t "Issue Year: ~a~%" issue-year)
    (format t "Exp Year: ~a~%" expiration-year)
    (format t "Height: ~a~%" height)
    (format t "Hair Color: ~a~%" hair-color)
    (format t "Eye Color: ~a~%" eye-color)
    (format t "Passport Id: ~a~%" passport-id)
    (format t "Country Id: ~a~%" country-id)
    (format t "Is Valid? ~a~%~%" is-valid)))


(defun flatten-list (lst)
  (cond
    ;;terminating case - lst is empty
    ((null lst) nil)
    ;;terminating case - lst is an atom, return the atom in a list for appending
    ((atom lst) (list lst))
    ;;do something with first of list, and recur on rest
    (t (append (flatten-list (first lst)) (flatten-list (rest lst))))))

(defun make-passport-from-params (params)
  (apply #'make-passport (flatten-list params)))
