;; Stephan Luther
;; The web portion of the goal-site
;; 2011/08/30

(load "goal.lisp")
(load "webserver.lisp")

(defun goal-request-handler (path header params)
	(if (equal path "goals")
			(princ "<html><body>Goal Site</body></html>")))