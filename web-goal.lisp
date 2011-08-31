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
					(open-html-tag)
					(open-head-tag)
					(print-header)
					(embed-css3)
					(close-head-tag)
					(open-body-tag)
					(princ "Goal Site!<br>")
					(process-parameters params)
					(set-goal-form)
					(close-body-tag)
					(close-html-tag)
					(save-goals-to-file "test.goals" *goals*))))

(defun html5-doctype ()
	(princ "<!DOCTYPE HTML>"))

(defun open-html-tag ()
	(princ "<html>"))

(defun close-html-tag ()
	(princ "</html>"))

(defun open-head-tag ()
	(princ "<head>"))

(defun close-head-tag ()
	(princ "</head>"))

(defun open-body-tag ()
	(princ "<body>"))

(defun close-body-tag ()
	(princ "</body>"))

(defun print-header ()
	(princ "<title>Goal Site</title>"))

; Currently only handles form input for adding goals.
(defun process-parameters (params)
	(if params
			(progn
				(let ((title (cdr (assoc 'goalAddTitle params)))
							(desc (cdr (assoc 'goalAddDescription params))))
					(set-goal title desc))
				) ; Put timeout here to clean params?
			))

(defun set-goal-form ()
	(progn
		(princ "<form method='post'>")
		(princ "<h3>Add Goal</h3>")
		(princ "<p><label for='name'>Title: </label><input type='text' name='goalAddTitle' /></p>")
		;(princ "Description:<input type='text' name='goalAddDescription' /><br>")
		(princ "<textarea rows='3' cols='50' wrap='physical' name='goalAddDescription'>")
		(princ "Add goal description here")
		(princ "</textarea><br>")
		(princ "<input type='submit' value='Add Goal' />")
		(princ "</form>")))

(defun embed-css3 ()
	(progn
		(princ "<style type='text/css'>")
		(princ "</style>")))
	
(defun goal-info ())

(defun expanded-goal-info ())