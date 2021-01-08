(defun make-tree (item)
  "creates a new node with a value of item"
  (cons (cons item nil) nil))

(defun add-child (tree child)
  (setf (car tree) (append (car tree) child))
  tree)

(defun first-child (tree)
  (if (null tree)
      nil
      (cdr (car tree))
      )
  )

(defun next-sibling (tree)
  (cdr tree)
  )

(defun data (tree)
  (car (car tree))
  )
