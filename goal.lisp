;; Stephan Luther
;; Goal site project
;; This is the REPL driven portion

(defparameter *goals* nil)

(defparameter *id-count* 0)

(defun create-goal (title description)
	(let ((currid *id-count*))
		(setf *id-count* (1+ *id-count*))
		(list currid title description nil)))

(defun prompt-read (section)
	(format t "~a: " section)
	(read-line))

(defun set-goal ()
	(push (create-goal (prompt-read 'title) (prompt-read 'description)) *goals*))

(defun search-id (id goals)
	(assoc id goals))

(defun search-title (title goals))

(defun set-complete (id goals)
	(setf (car (cdr (cdr (cdr (search-id id goals))))) t))

(defun delete-goal (id goals)
	(setf *goals*
				(labels ((id-rem (tid)
									 (eq (car (assoc (car tid) goals)) id)))
					(remove-if #'id-rem goals))))