(defparameter *test-rules* 
  (list
   (list "light red bag" (list "bright white bag" "muted yellow bag"))
   (list "dark orange bag" (list "bright white bag" "muted yellow bag"))
   (list "bright white bag" (list "shiny gold bag"))))

(defun find-pattern-expand-rule (pattern)
  (loop for r in *test-rules*
	when (string-equal pattern (car r))
	  appending (cdr r)))

   
