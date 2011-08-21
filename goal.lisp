;; Stephan Luther
;; Goal site project
;; This is the REPL driven portion

(defparameter *goals* nil)

(defparameter *id-count* 0)

(defun create-goal (title description)
	(let ((currid *id-count*))
		(setf *id-count* (1+ *id-count*))
		(list :id currid :title title :description description :accomplished nil)))

(defun prompt-read (section)
	(format t "~a: " section)
	(read-line))

(defun set-goal ()
	(push (create-goal (prompt-read 'title) (prompt-read 'description)) *goals*))

(defun search-id (id goals)
	(remove-if-not #'(lambda (goal) (equal (getf goal :id) id))
								 goals))

(defun search-title (title goals)
	(remove-if-not #'(lambda (goal) (equal (getf goal :title) title))
								 goals))

(defun set-complete (id))

(defun delete-goal (id))