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

; This particular function is not in the iteration 1 spec so will leave as stub for now
(defun search-title (title goals))

(defun set-complete (id goals)
	(setf (car (cdr (cdr (cdr (search-id id goals))))) t))

(defun delete-goal (id)
	(setf *goals*
				(let ((goals *goals*))
					(labels ((id-rem (tid)
										 (eq (car (assoc (car tid) goals)) id)))
						(remove-if #'id-rem goals)))))

(defun save-goals-to-file (filename goals)
	(with-open-file (out filename
											 :direction :output
											 :if-exists :supersede)
		(with-standard-io-syntax
			(print goals out))))

(defun load-goals-from-file (filename)
	(progn
		(with-open-file (in filename
												:direction :input)
			(with-standard-io-syntax
				(setf *goals* (read in))))
		(let ((goals *goals*))
			(setf *id-count* (1+ (car (car goals))))))) ; Last goal has the highest id

(defun list-all-goals (goals)
	(labels ((print-goal (goal)
						 (format t "ID: ~a~%" (car goal))
						 (format t "Title: ~a~%" (car (cdr goal)))
						 (format t "Description: ~a~%" (car (cdr (cdr goal))))
						 (format t "Accomplished: ~a~%~%" (car (cdr (cdr (cdr goal)))))))
		(mapcar #'print-goal goals)))