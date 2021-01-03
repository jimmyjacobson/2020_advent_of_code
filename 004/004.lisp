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

(defun main (filename)
  (loop for passport in (convert-passports-to-lists (read-passports filename))
	       counting (valid-passport-p (make-passport-from-params passport))))


