;;;; Stephan Luther
;;;; Goal site project
;;;; This is the REPL driven portion

(defparameter *goals* nil)

(defparameter *todo-list* nil)

(defparameter *id-count* 0)

(defparameter *todo-count* 0)

(defparameter *information* (list :owner nil :goals nil :todo-list nil :filename nil))

;;; This is common to all todo items, goals etc
;;; (unique-id, timestamp)
(defun create-header (id)
	(let ((timestamp (get-universal-time)))
		(list id timestamp)))

;;; Expects a list which contains headers in the first element, other it'll return nil or something else
(defun get-latest-id (data)
	(car (car (car data))))

;;; Same as above function but adds one to the id. This is to remove logic in calling code
(defun get-next-id (data)
	(if data
			(1+ (get-latest-id data))
			0))

(defun create-goal (id title description)
	(let ((header (create-header id))
				(body (list title description)))
		(list header body)))

(defun add-goal (goal data)
	(let ((goals (getf data :goals)))
		(if goals
				(push goal (getf data :goals))
				(setf (getf data :goals) (list goal)))))

(defun search-goal-id (id))

(defun delete-goal (id data))

;;; Debug function for adding a goal
(defun add-goal-repl ()
	(let ((goal (create-goal (get-next-id (getf *information* :goals)) (prompt-read 'Title) (prompt-read 'Description))))
		(add-goal goal *information*)))

(defun create-todo-item (item)
	(let ((currid *todo-count*)
				(timestamp (get-universal-time)))
		(setf *todo-count* (1+ *todo-count*))
		(list currid item timestamp nil)))

(defun prompt-read (section)
	(format t "~a: " section)
	(read-line))

(defun create-goal-note (id note)
	(let ((timestamp (get-universal-time)))
		(list id timestamp note)))

(defun add-note-to-goal (goal text)
	(let ((notes (car (cdr (cdr (cdr (cdr (cdr goal))))))))
		(if (not notes)
			(let ((note (create-goal-note 0 text)))
				(setf (cdr (cdr (cdr (cdr (cdr goal))))) (list (list note))))
			(let ((oldid (car (car (car (cdr (cdr (cdr (cdr (cdr goal))))))))))
				(let ((newid (1+ oldid)))
					(push (create-goal-note newid text) (car (cdr (cdr (cdr (cdr (cdr goal))))))))))))

(defun search-goal-note-id (goal id)	
	(let ((notes (car (cdr (cdr (cdr (cdr (cdr goal))))))))
		(if notes
				(assoc id notes))))

(defun get-all-goal-notes (goal)
	(car (cdr (cdr (cdr (cdr (cdr goal)))))))

(defun delete-goal-note (goal-id id)
	(let ((goal (search-id goal-id *goals*))
				(new-notes nil))
		(progn
			(let ((notes (car (cdr (cdr (cdr (cdr (cdr goal))))))))
				(setf new-notes 
							(labels ((id-rem (tid)
												 (eq (car (assoc (car tid) notes)) id)))
								(remove-if #'id-rem notes))))
			(setf (cdr (cdr (cdr (cdr (cdr goal))))) (list new-notes)))))
			
(defun set-todo (item)
	(push (create-todo-item item) *todo-list*))

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

(defun list-all-goals (goals)
	(labels ((print-goal (goal)
						 (format t "ID: ~a~%" (car goal))
						 (format t "Title: ~a~%" (car (cdr goal)))
						 (format t "Description: ~a~%" (car (cdr (cdr goal))))
						 (format t "Accomplished: ~a~%~%" (car (cdr (cdr (cdr goal)))))))
		(mapcar #'print-goal goals)))

(defun save-information-to-file (filename information)
	(with-open-file (out filename
											 :direction :output
											 :if-exists :supersede)
		(with-standard-io-syntax
			(print information out))))

(defun load-information-from-file (filename)
	(with-open-file (in filename
											:direction :input)
		(with-standard-io-syntax
			(setf *information* (read in)))))