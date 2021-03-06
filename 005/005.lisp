(ql:quickload "uiop")

(defun populate-list (&optional (min 0) (max 100))
  (loop for x from min to max collecting x))

(defun bsp-traversal (directions set)
  ;;directions is a list of 0 (left) and 1 (right)
  ;;set is the list of items to partition
  ;;returns the number at the node
  (cond
    ;; base case, return set
     ((null directions)
      (first set))
     ;; partition left
     ((eq 0 (first directions))
      (bsp-traversal (rest directions) (subseq set 0 (/ (length set) 2))))
     ((eq 1 (first directions))
      (bsp-traversal (rest directions) (subseq set (/ (length set) 2))))
     ;; This is a problem
     (t (error "Direction not valid"))))

(defun airline-rows-to-directions (rows)
  ;;expects a string of F and B's and maps to a list of 0 and 1
  (loop for x from 0 to (- (length rows) 1)
	collecting (cond ((char-equal #\F (char rows x)) 0)
			 ((char-equal #\B (char rows x)) 1)
			 (t (error "Invalid Row")))))

(defun airline-seats-to-directions (seats)
  ;;expects a string of L and R
  (loop for x from 0 to (- (length seats) 1)
	collecting (cond ((char-equal #\L (char seats x)) 0)
			 ((char-equal #\R (char seats x)) 1))))

(defun process-boarding-pass (str)
  ;;takes a boarding pass as a string and performs the bsp traversal after parsing directions
  (let ((rows (airline-rows-to-directions (subseq str 0 7)))
	(seats (airline-seats-to-directions (subseq str 7))))
    (+ (* (bsp-traversal rows (populate-list 0 127)) 8)
       (bsp-traversal seats (populate-list 0 7)))))

;;this is the main problem function for part 1
(defun find-max-boarding-pass (filename)
  (loop for str in (uiop:read-file-lines filename)
	maximize (process-boarding-pass str)))

;; find missing ids for part 2
(defun find-missing-ids (filename)
  (set-difference (populate-list 85 890)
		(loop for str in (uiop:read-file-lines filename)
		      collecting (process-boarding-pass str))))
