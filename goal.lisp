;; Stephan Luther
;; Goal site project
;; This is the REPL driven portion

(defparameter *goals* nil)

(defparameter *todo-list* nil)

(defparameter *id-count* 0)

(defparameter *todo-count* 0)

(defun create-goal (title description)
	(let ((currid *id-count*))
		(setf *id-count* (1+ *id-count*))
		(list currid title description nil)))

(defun create-todo-item (item)
	(let ((currid *todo-count*)
				(timestamp (get-universal-time)))
		(setf *todo-count* (1+ *todo-count*))
		(list currid item timestamp nil)))

(defun prompt-read (section)
	(format t "~a: " section)
	(read-line))

(defun set-goal (title description)
	(push (create-goal title description) *goals*))

(defun set-todo (item)
	(push (create-todo-item item) *todo-list*))

(defun set-goal-repl ()
	(push (create-goal (prompt-read 'title) (prompt-read 'description)) *goals*))

(defun set-todo-repl ()
	(push (create-todo-item (prompt-read 'item)) *todo-list*))

(defun search-id (id goals)
	(assoc id goals))

; This particular function is not in the iteration 1 spec so will leave as stub for now
(defun search-title (title goals))

(defun set-complete (id goals)
	(setf (car (cdr (cdr (cdr (search-id id goals))))) t))

(defun set-todo-item-complete (id item)
	(setf (car (cdr (cdr (cdr (search-id id item))))) t))

(defun delete-goal (id)
	(setf *goals*
				(let ((goals *goals*))
					(labels ((id-rem (tid)
										 (eq (car (assoc (car tid) goals)) id)))
						(remove-if #'id-rem goals)))))

(defun delete-todo-item (id)
	(setf *todo-list*
				(let ((items *todo-list*))
					(labels ((id-rem (tid)
										 (eq (car (assoc (car tid) items)) id)))
						(remove-if #'id-rem items)))))

(defun save-goals-to-file (filename goals)
	(with-open-file (out filename
											 :direction :output
											 :if-exists :supersede)
		(with-standard-io-syntax
			(print goals out))))

(defun save-todo-items-to-file (filename items)
	(with-open-file (out filename
											 :direction :output
											 :if-exists :supersede)
		(with-standard-io-syntax
			(print items out))))

(defun load-goals-from-file (filename)
	(progn
		(with-open-file (in filename
												:direction :input)
			(with-standard-io-syntax
				(setf *goals* (read in))))
		(let ((goals *goals*))
			(setf *id-count* (1+ (car (car goals))))))) ; Last goal has the highest id

(defun load-todo-items-from-file (filename)
	(progn
		(with-open-file (in filename
												:direction :input)
			(with-standard-io-syntax
				(setf *todo-list* (read in))))
		(let ((items *todo-list*))
			(setf *todo-count* (1+ (car (car items)))))))

(defun list-all-goals (goals)
	(labels ((print-goal (goal)
						 (format t "ID: ~a~%" (car goal))
						 (format t "Title: ~a~%" (car (cdr goal)))
						 (format t "Description: ~a~%" (car (cdr (cdr goal))))
						 (format t "Accomplished: ~a~%~%" (car (cdr (cdr (cdr goal)))))))
		(mapcar #'print-goal goals)))