(ql:quickload "uiop")
(ql:quickload "cl-ppcre")
(load "./passport.lisp")

(defun read-passports (filename)
  (let ((input nil))
    (setf input (uiop:split-string (ppcre::regex-replace-all "\\n" (ppcre:regex-replace-all "\\n\\n" (uiop:stripln (uiop:read-file-string filename)) "===") " ") :separator "==="))
    (remove-if (lambda (x) (equal x "")) input)))

(defun convert-passports-to-lists (passports)
  (loop for passport in passports
	collect (mapcar (lambda (x)
		      (ppcre:register-groups-bind (a b) ("(.+):(.+)" x)
			(list (read-from-string (concatenate 'string ":" a))  b)))
			(cl-ppcre:split "\\s" passport))))

;;My approach is to use CLOS and objects to only create valid passports and then count how many were created
(defun main (filename)
  (loop for passport in (convert-passports-to-lists (read-passports filename))
	counting  (make-passport-from-params passport)))

(defun main2 (filename)
  (let ((passports nil)
	(passport nil))
    (setf passports
	  (loop for passport-params in (convert-passports-to-lists (read-passports filename))
		do (setf passport (make-passport-from-params passport-params))
		when (and
		      (not (null passport))
		      (not (null (slot-value passport 'is-valid))))
		  collect passport))
	  passports))


  
