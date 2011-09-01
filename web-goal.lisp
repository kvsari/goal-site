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
									(princ "Goal Site!<br>")
									(process-parameters params)
									(create-goal-form)))
				(save-goals-to-file "test.goals" *goals*))))

(defun html5-doctype ()
	(princ "<!DOCTYPE HTML>"))

; Currently only handles form input for adding goals.
(defun process-parameters (params)
	(if params
			(if (equal (car (assoc 'createGoalTitle params)) 'createGoalTitle)
					(progn
						(process-create-goal-parameters params)
						(tag script ()
								 (princ "window.setTimeout('window.location=\"goals\"',100)"))); Clear out the address bar of parameters
					)))

(defun process-create-goal-parameters (params)
	(let ((title (cdr (assoc 'createGoalTitle params)))
				(desc (cdr (assoc 'createGoalDescription params))))
		(set-goal title desc)))

(defun create-goal-form ()
	(tag form (method 'post)
			 (tag h3
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
				 (princ "{")
				 (princ "margin: 0;")
				 (princ "padding: 0;")
				 (princ "}"))))
	
(defun goal-info ())

(defun expanded-goal-info ())