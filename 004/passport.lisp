(defclass passport ()
  ((birth-year
    :initarg :birth-year
    :initform (error "Birth Year Required"))
   (issue-year
    :initarg :issue-year
    :initform nil)
   (expiration-year
    :initarg :expiration-year
    :initform nil)
   (height
    :initarg :height
    :initform nil)
   (hair-color
    :initarg :hair-color
    :initform nil)
   (eye-color
    :initarg :eye-color
    :initform nil)
   (passport-id
    :initarg :passport-id
    :initform nil)
   (country-id
    :initarg :country-id
    :initform nil)))

(defun make-passport (&key byr
			iyr
			eyr
			hgt
			hcl
			ecl
			pid
			cid)
  ;;this is why my object is creating invalid passports, and have to be checked after.
  ;;need to ensure that only existing params are passed into make-instance
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
  (:documentation "Generic method for validating passports"))

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
    (format t "Birth Year: ~a~%" birth-year)
    (format t "Issue Year: ~a~%" issue-year)
    (format t "Exp Year: ~a~%" expiration-year)
    (format t "Height: ~a~%" height)
    (format t "Hair Color: ~a~%" hair-color)
    (format t "Eye Color: ~a~%" eye-color)
    (format t "Passport Id: ~a~%" passport-id)
    (format t "Country Id: ~a~%" country-id)))

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
