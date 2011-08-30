;; Stephan Luther
;; 2011/07/04

(in-package :cl-user)

(defun print-tag (name alst closingp)
	(princ #\<)
	(when closingp
		(princ #\/))
	(princ (string-downcase name))
	(mapc (lambda (att)
					(format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
				alst)
	(princ #\>))

(defmacro tag (name atts &body body)
	`(progn (print-tag ',name
										 (list ,@(mapcar (lambda (x)
																			 `(cons ',(car x) ,(cdr x)))
																		 (pairs atts)))
										 nil)
					,@body
					(print-tag ',name nil t)))

(defmacro svg (width height &body body)
	`(tag svg (xmlns "http://www.w3.org/2000/svg"
									 "xmlns:xlink" "http://www.w3.org/1999/xlink"
									 height ,height
									 width ,width)
		 ,@body))

(defun brightness (col amt)
	(mapcar (lambda (x)
						(min 255 (max 0 (+ x amt))))
					col))

(defun svg-style (colour)
	(format nil
					"~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
					(append colour
									(brightness colour -100))))

(defun circle (center radius colour)
	(tag circle (cx (car center)
									cy (cdr center)
									r radius
									style (svg-style colour))))

(defun polygon (points colour)
	(tag polygon (points (format nil
															 "~{~a,~a ~}"
															 (mapcan (lambda (tp)
																				 (list (car tp) (cdr tp)))
																			 points))
											 style (svg-style colour))))

(defun random-walk (value length)
	(unless (zerop length)
		(cons value
					(random-walk (if (zerop (random 2))
													 (1- value)
													 (1+ value))
											 (1- length)))))