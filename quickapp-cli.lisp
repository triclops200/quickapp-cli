(in-package #:quickapp-cli)

(defun fix-named-arg (arg)
  (destructuring-bind (name . value) arg
	(list
	 (read-from-string (concatenate 'string ":" name))
	 (if (string= name "dependencies")
		 (cons 'list (read-from-string value))
		 value))))

(defun fix-named-args (parsed-args)
  (destructuring-bind (unnamed named) parsed-args
	(list unnamed (mapcan #'fix-named-arg named))))

(defun -main (&optional args)
  "Entry point"
  (let* ((arg-defs
		  '(("h" "help" "Display this help menu")
			("d" "dependencies" "(:dep1 [:dep2 ...])" "The dependencies")
			("p" "project-name" "NAME" "The project name")
			("a" "project-author" "NAME" "The name of the author")
			("s" "project-description" "DESCRIPTION" "The project description")
			("e" "executable-name" "NAME" "The executable name")))
		 (parsed-args (quickapp:parse-args arg-defs (cdr args))))
	(if (or (/= (length (first parsed-args)) 1)
			(assoc "help" (second parsed-args) :test #'string=))
		(progn (format t "Usage: ~a PROJECT-PATH [OPTIONS]~%OPTIONS:~%~a~%~a~a~a~%~a~%~a~%"
					   (first args)
					   (quickapp:generate-flag-string arg-defs)
					   "Example Usage: " (first args) "test-project \\"
					   " -d\"(:sdl2 :cl-opengl)\" \\"
					   " --author=cluser"))
		(let* ((fixed-args (fix-named-args parsed-args))
			   (cmd `(quickapp:quickapp ,(caar fixed-args) ,@(cadr fixed-args))))
		  (eval cmd)))))

(defun disable-debugger ()
  (labels
	  ((exit (c h)
		 (declare (ignore h))
		 (format t "~a~%" c)
		 (sb-ext:exit)))
	(setf *debugger-hook* #'exit)))
