;;;; Stephan Luther
;;;; Goal site project
;;;; This is the REPL driven portion

;;; Global list
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

(defun create-note (id text)
	(let ((header (create-header id))
				(body (list text)))
		(list header body)))

(defun create-goal (id title description)
	(let ((header (create-header id))
				(body (list title description)))
		(list header body)))

;;; Can be macro'd
(defun add-goal (goal data)
	(let ((goals (getf data :goals)))
		(if goals
				(push goal (getf data :goals))
				(setf (getf data :goals) (list goal)))))

;;; ditto
(defun add-todo-item (item data)
	(let ((items (getf data :todo-list)))
		(if items
				(push item (getf data :todo-list))
				(setf (getf data :todo-list) (list item)))))

;;; Returns a list containing the correct header id. Expects a list of lists containing headers as their first element
(defun search-id (id data)
	(car 
	 (labels ((narrow (segment)
							(let ((header (car segment)))
								(equal (car header) id))))
		 (remove-if-not #'narrow data))))

;;; Must feed it the *information* global for data or an equally laid out data source
;;; This and below are candidates for macroisation
(defun delete-goal (id data)
	(setf (getf data :goals)
				(let ((goals (getf data :goals)))
					(labels ((test (segment)
										 (let ((header (car segment)))
											 (equal (car header) id))))
						(remove-if #'test goals)))))

;;; Must feed it the *information* global for data or an equally laid out data source
;;; This and above can be turned into a macro.
(defun delete-todo-item (id data)
	(setf (getf data :todo-list)
				(let ((items (getf data :todo-list)))
					(labels ((test (segment)
										 (let ((header (car segment)))
											 (equal (car header) id))))
						(remove-if #'test items)))))

;;; Adds an extra field in the header portion of the segment with the value T. Expecting the header portion to only have
;;; two elements in the list since it will overwrite any third element. Expects data to be a list of lists that contain
;;; a header as the first item
(defun set-complete (id data)
	(let ((goal (search-id id data)))
		(if goal
				(setf (cdr (cdr (car goal))) (list t)))))

;;; Debug function for adding a goal
(defun add-goal-repl ()
	(let ((goal (create-goal (get-next-id (getf *information* :goals)) (prompt-read 'Title) (prompt-read 'Description))))
		(add-goal goal *information*)))

(defun add-todo-item-repl ()
	(let ((item (create-todo-item (get-next-id (getf *information* :todo-list)) (prompt-read 'Item-Text))))
		(add-todo-item item *information*)))

(defun create-todo-item (id item)
	(let ((header (create-header id))
				(body (list item)))
		(list header body)))

(defun prompt-read (section)
	(format t "~a: " section)
	(read-line))

;;; Function creates the note from the text and assigns the correct id to it.
(defun add-note-to-goal (goal text)
	(let ((body (car (cdr goal))))
		(let ((notes (car (cdr (cdr body)))))
			(if (not notes)
					(setf (cdr (cdr body)) (list (list (create-note 0 text))))
					(setf (cdr (cdr body)) (list (push (create-note (get-next-id notes) text) notes)))))))

(defun get-all-goal-notes (goal)
	(let ((body (car (cdr goal))))
		(let ((notes (cdr (cdr body))))
			(car notes))))

;;; Finds a goal note by id inside of a goal. Expects the specific goal to be passed.
(defun search-goal-note-id (id goal)
	(search-id id (get-all-goal-notes goal)))

;;; Deletes a goal note by id. Expects the specific goal to be passed.
(defun delete-goal-note (id goal)
	(let ((body (car (cdr goal))))
		(setf (cdr (cdr body)) (list
					(let ((notes (car (cdr (cdr body)))))
						(labels ((test (segment)
											 (let ((header (car segment)))
												 (equal (car header) id))))
							(remove-if #'test notes)))))))

;;; This particular function is not in the iteration 1 spec so will leave as stub for now
(defun search-title (title goals))

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