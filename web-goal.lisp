;; Stephan Luther
;; The web portion of the goal-site
;; 2011/08/30

(load "goal.lisp")
(load "webserver.lisp")

(defun goal-request-handler (path header params)
	(if (equal path "goals")
			(progn
				(load-goals-from-file "test.goals")
				(html5-doctype)
				(tag html ()
						 (tag head ()
									(tag title ()
											 (princ "Goal Site"))
									(embed-css3))
						 (tag body ()
									(tag h2 ()
											 (princ "Goal Site!<br>"))
									(process-parameters params)
									(create-goal-form)
									(tag h3 ()
											 (princ "All Goals<br>"))
									(list-modifiable-goals *goals*)))
				(save-goals-to-file "test.goals" *goals*))))

(defun html5-doctype ()
	(princ "<!DOCTYPE HTML>"))

; Currently only handles form input for adding goals.
(defun process-parameters (params)
	(if params
			(progn
				(if (equal (car (assoc 'createGoalTitle params)) 'createGoalTitle)
						(process-create-goal-parameters params))
				(if (equal (car (assoc 'delete params)) 'delete)
						(let ((id (parse-integer (cdr (assoc 'goalid params)))))
							(delete-goal id)))
				(if (equal (car (assoc 'accomplished params)) 'accomplished)
						(let ((id (parse-integer (cdr (assoc 'goalid params)))))
							(set-complete id *goals*)))
				(tag script ()
						 (princ "window.setTimeout('window.location=\"goals\"',1000)"))); Clear out the address bar of parameters
			))

(defun process-create-goal-parameters (params)
	(let ((title (cdr (assoc 'createGoalTitle params)))
				(desc (cdr (assoc 'createGoalDescription params))))
		(set-goal title desc)))

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
				 )))

(defun list-goals (goals)
	(mapcar #'goal-info goals))

(defun list-modifiable-goals (goals)
	(mapcar #'modifiable-goal-info goals))

(defun goal-info (goal)
	(tag section (id 'goalinfo)
			 (tag h4 ()
						(format t "~a" (car (cdr goal))))
			 (tag p ()
						(format t "~a" (car (cdr (cdr goal)))))
			 (tag p ()
						(progn
							(princ "Status: ")
							(if (eq (car (cdr (cdr (cdr goal)))) nil)
									(princ "Incomplete")
									(princ "Achieved"))))))

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
							