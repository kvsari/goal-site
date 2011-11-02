;; Stephan Luther
;; The web portion of the goal-site
;; 2011/08/30

(load "goal.lisp")
(load "webserver.lisp")

(defparameter *show-all* nil)

(defun goal-request-handler (path header params)
	(progn
		(html5-doctype)
		(tag html ()
				 (tag head ()
							(tag title ()
									 (princ "Goal Site"))
							(embed-css3))
				 (tag body ()
							(print-top-navigation-menu)
							(hide-show-completed)
							;(princ path)
							(load-information-from-file "information.goals")
							(if (equal path "GOALS")
									(progn
										(tag h2 ()
												 (princ "Goal section!<br>"))
										(process-parameters params)
										(create-goal-form)
										(tag h3 ()
												 (princ "All Goals<br>"))
										(list-all-goals-summary (get-pruned-list-from *show-all* (getf *information* :goals)))))
							(if (equal path "TODOLIST")
									(progn
										(create-todo-item-form)
										(process-parameters params)
										(tag h2 ()
												 (princ "Todo List<br>"))
										(list-todo-list (get-pruned-item-list-copy *show-all* (getf *information* :todo-list)))))
							(if (equal path "goal")
									(let ((goalid (parse-integer (cdr (assoc 'goalid params)))))
										(progn
											(process-goal-note-parameters params)
											(html-show-goal (search-id goalid (getf *information* :goals))))))
							(save-information-to-file "information.goals" *information*)))))

(defun save-show-all-to-file (filename show)
	(with-open-file (out filename
											 :direction :output
											 :if-exists :supersede)
		(with-standard-io-syntax
			(print show out))))

(defun load-show-all-from-file (filename)
	(with-open-file (in filename
											:direction :input)
		(with-standard-io-syntax
			(setf *show-all* (read in)))))

(defun print-top-navigation-menu ()
	(tag nav ()
			 (tag ul ()
						(tag li ()
								 (tag a (href 'goals)
											(princ "Goals")))
						(tag li ()
								 (tag a (href 'todolist)
											(princ "Todo List"))))))

(defun hide-show-completed ()
	(tag p ()
			 (tag form (method 'post)
						(tag input (type 'submit name 'showall value 'showall))
						(tag input (type 'submit name 'hidecomplete value 'hidecomplete)))))

(defun get-pruned-list-from (show data)
	(if (eq show nil)
			(labels ((cmpl-rem (data)
								 (eq (car (cdr (cdr data))) t)))
				(remove-if #'cmpl-rem data))
			data))

(defun html5-doctype ()
	(princ "<!DOCTYPE HTML>"))

(defun process-parameters (params)
	(if params
			(progn
				(load-show-all-from-file "showall.conf")
				(if (equal (car (assoc 'createGoalTitle params)) 'createGoalTitle)
						(process-create-goal-parameters params))
				(if (equal (car (assoc 'delete params)) 'delete)
						(let ((id (parse-integer (cdr (assoc 'goalid params)))))
							(delete-goal id)))
				(if (equal (car (assoc 'accomplished params)) 'accomplished)
						(let ((id (parse-integer (cdr (assoc 'goalid params)))))
							(set-complete id *goals*)))
				(if (equal (car (assoc 'createtodoitem params)) 'createtodoitem)
						(process-create-todo-item-parameters params))
				(if (equal (car (assoc 'todoitemdone params)) 'todoitemdone)
						(let ((id (parse-integer (cdr (assoc 'todoitemid params)))))
							(set-todo-item-complete id *todo-list*)))
				(if (equal (car (assoc 'todoitemdelete params)) 'todoitemdelete)
						(let ((id (parse-integer (cdr (assoc 'todoitemid params)))))
							(delete-todo-item id)))
				(if (equal (car (assoc 'showall params)) 'showall)
						;(error "Showing all"))
						(setf *show-all* t))
				(if (equal (car (assoc 'hidecomplete params)) 'hidecomplete)
						(setf *show-all* nil))
				(save-show-all-to-file "showall.conf" *show-all*)
				;(tag script ()
				;		 (princ "window.setTimeout('window.location=\"goals\"',1000)")); Clear out the address bar of parameters
				)))

(defun process-goal-note-parameters (params)
	(if params
			(progn
				(if (equal (car (assoc 'goalnote params)) 'goalnote)
						(let ((goalid (parse-integer (cdr (assoc 'goalid params))))
									(text (cdr (assoc 'goalnote params))))
							(add-note-to-goal (search-id goalid *goals*) text)))
				(if (equal (car (assoc 'deletenote params)) 'deletenote)
						(let ((goalid (parse-integer (cdr (assoc 'goalid params))))
									(noteid (parse-integer (cdr (assoc 'noteid params)))))
							(delete-goal-note goalid noteid))))))

(defun process-create-goal-parameters (params)
	(let ((title (cdr (assoc 'createGoalTitle params)))
				(desc (cdr (assoc 'createGoalDescription params))))
		(set-goal title desc)))

(defun process-create-todo-item-parameters (params)
	(let ((item (cdr (assoc 'createtodoitem params))))
		(set-todo item)))

(defun create-goal-form ()
	(tag form (method 'post id 'creategoalform)
			 (tag h3 ()
						(princ "Create Goal"))
			 (tag p ()
						(tag label (for 'name)
								 (princ "Title: "))
						(tag input (type 'text name 'createGoalTitle)))
			 (tag textarea (rows '3 cols '50 wrap 'physical name 'createGoalDescription)
						(princ "Goal description goes here"))
			 (princ "<br>")
			 (tag input (type 'submit value "Create Goal"))))

(defun create-todo-item-form ()
	(tag form (method 'post id 'createtodoitem)
			 (tag h3 ()
						(princ "Add todo item"))
			 (tag p ()
						(tag label (for 'item)
								 (princ "Item: "))
						(tag textarea (rows '1 cols '50 wrap 'physical name 'createTodoItem))
						(tag input (type 'submit value "Create Todo Item")))))

(defun embed-css3 ()
	(tag style (type "text/css")
			 (progn
				 (format t "~%")
				 (princ "{")
				 (princ "margin: 0;")
				 (princ "padding: 0;")
				 (princ "}")
				 (format t "~%~%")
				 (princ "header, footer, aside, nav, article {  display: block;}")
				 (format t "~%~%")
				 (princ "#CREATEGOALFORM {")
				 (princ "background-color: red;")
				 (princ "border-radius: 22px;")
				 (princ "padding: 25px;")
				 (princ "}")
				 (format t "~%~%")
				 (princ "#GOALINFO {")
				 (princ "background-color: green;")
				 (princ "border-radius: 22px;")
				 (princ "padding: 25px;")
				 (princ "}")
				 (format t "~%~%")
				 (princ "#EXPANDEDGOALINFO {")
				 (princ "background-color: yellow;")
				 (princ "border-radius: 22px;")
				 (princ "padding: 25px;")
				 (princ "}")
				 (format t "~%~%")
				 (princ "#TODOITEM {")
				 (princ "background-color: green;")
				 (princ "border-radius: 5px;")
				 (princ "padding: 0px;")
				 (princ "}")
				 )))

(defun list-all-goals-summary (goals)
	(mapcar #'html-show-goal-summary goals))

(defun html-show-goal-summary (goal)
	(let ((header goal)
				(body (cdr goal)))
		(tag section (id 'goalinfo)
				 (tag h4 ()
							(let ((link (make-string-output-stream)))
								(progn
									(format link "goal?goalid=~a" (car (car header)))
									(tag a (href (get-output-stream-string link))
											 (format t "~a" (car (car body)))))))
				 (tag p ()
							(format t "~a" (car (cdr (car body)))))
				 (tag p ()
							(progn
								(princ "Status: ")
								(if (eq (car (cdr (cdr header))) nil)
										(let ((link (make-string-output-stream)))
											(progn
												(format link "acheivegoal?goalid=~a" (car (car header)))
												(tag a (href (get-output-stream-string link))
														 (format t "~a" "Incomplete"))))
										(princ "Acheived")))))))

(defun html-show-goal (goal)
	(html-show-goal-summary goal)
	(princ "<br>")
	(tag section (id 'addgoalnote)
			 (tag p ()
						(tag form (method 'post)
								 (tag label (for 'note)
											(princ "New Note: "))
								 (tag textarea (rows '10 cols '100 wrap 'physical name 'goalnote))
								 (tag input (type 'submit name 'submitnote value 'submitnote)))))
	(princ "<br>")
	(tag section (id 'goalnoteinfo)
			 (tag h4
						(princ "All Notes... <br>"))
			 (mapcar #'goal-note (get-all-goal-notes goal))))

(defun goal-note (note)
	(let ((header note)
				(body (cdr note)))
		(tag p (id 'goalnote)
				 (format t "~a" (car body)))))

(defun goal-note-info (note)
	(tag p ()
			 (progn
				 (format t "~a<br>" (car (cdr (cdr note))))
				 (tag form (method 'post)
							(progn
								(tag input (type 'hidden name 'noteid value (car note)))
								(tag input (type 'submit name 'deletenote value 'deletenote)))))))

(defun list-todo-list (todolist)
	(mapcar #'modifiable-todo-item-info todolist))

(defun todo-item-info (item)
	(tag section (id 'todoitem)
			 (tag p ()
						(if (eq (car (cdr (cdr (cdr item)))) nil)
								(princ "[ ] ")
								(princ "[*] ")) ; Need to come up with something better than this
						(princ (car (cdr item))))))

(defun modifiable-todo-item-info (item)
	(tag section (id 'todoitem)
			 (progn
				 (tag p ()
							(if (eq (car (cdr (cdr (cdr item)))) nil)
									(princ "[ ] ")
									(princ "[*] "))
							(princ (car (cdr item)))
							(tag form (method 'post)
									 (progn
										 (tag input (type 'hidden name 'todoitemid value (car item)))
										 (tag input (type 'submit name 'todoitemdone value 'done))
										 (tag input (type 'submit name 'todoitemdelete value 'delete))))))))

(defun modifiable-goal-info (goal)
	(tag section (id 'expandedgoalinfo)
			 (progn
				 (goal-info goal)
				 (tag form (method 'post)
							(progn
								(tag input (type 'hidden name 'goalid value (car goal)))
								(tag input (type 'submit name 'accomplished value 'Accomplish))
								(tag input (type 'hidden name 'goalid value (car goal)))
								(tag input (type 'submit name 'delete value 'delete)))))))
