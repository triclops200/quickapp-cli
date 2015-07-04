(in-package #:quickapp-cli)

(defmacro loop-rec (bindings &body body)
  "Similar to the clojure loop macro"
  (let ((names (mapcar #'car bindings))
		(init-vals (mapcar #'cadr bindings)))
	`(labels ((rec ,names
				,@body))
	   (rec ,@init-vals))))

(defun reduce-full (f &rest args)
  "A reducer that allows f to control the iteration as well as the
accumulator"
  (if (= (length args) 1)
	  (reduce-full f (caar args) (cdar args))
	  (loop-rec ((acc (list (car args) (cadr args))))
		 (if (cadr acc)
			 (rec (apply f acc))
			 (car acc)))))


(defmacro with-string-stream (stream &body body)
  "A macro to allow one to write to a variable string stream and get
the body result out"
  (let ((st-name (gensym))
		(res-name (gensym)))
	`(let ((,st-name (make-array '(0) :element-type 'base-char
								 :fill-pointer 0 :adjustable t)))
	   (let ((,res-name (with-output-to-string (,stream ,st-name)
						  ,@body)))
		 (values ,st-name ,res-name)))))

(defun subseq-rel (seq fst &optional (l 0 l-p))
  "A relative subsequence function"
  (if l-p
	  (subseq seq fst (+ fst l))
	  (subseq seq fst)))

(defun split-string-first (seperator str)
  "Returns a value pair representing the first split in the string
matching the seperator string"
  (let ((l (length seperator))
		(el (length str)))
	(with-string-stream st 
	  (loop-rec ((i 0)) ;; Loop over indicies
		 (cond ((> (+ i l) el) ;; If we have hit the end
				(write-string (subseq-rel str i (1- l)) st) ;; Write the rest of the string to the stream
				"") ;;There is nothing to seperate, return an empty string for the second half
			   ((string= seperator (subseq-rel str i l)) ;; We have found the seperator
				(subseq str (+ i l))) ;; Return the rest of the string as the second half
			   (t (progn ;; We need to keep going
					(write-char (char str i) st) ;; Copy the current char to the stream
					(rec (1+ i))))))))) ;; Iterate to the next string

(defun parse-arg (acc arg-list)
  "Parses one argument/argument pair into the named/unnamed lists
appropriately"
  (destructuring-bind (unnamed named) acc ;; keep track of the unnamed and named args
	(let* ((x (car arg-list)) ;; let x be the first argument
		   (l (length x))) ;; let l be the next argument
	  (cond
		((and (>= l 2) (string= "--" (subseq x 0 2)))
		 (multiple-value-bind (fst snd) (split-string-first "=" x)
		   (list (list unnamed (cons (cons (subseq fst 2) snd) named)) (cdr arg-list))))
		((string= "-" (subseq x 0 1))
		 (list (list unnamed (cons (cons (subseq x 1) (cadr arg-list)) named)) (cddr arg-list)))
		(t
		 (list (list (cons x unnamed) named) (cdr arg-list)))))))

(defun parse-args (args)
  (reduce-full #'parse-arg (list nil nil) args))

(defun -main (&optional args)
  (format t "~a~%" "I don't do much yet"))

(defun disable-debugger ()
  (labels
	  ((exit (c h)
		 (declare (ignore h))
		 (format t "~a~%" c)
		 (sb-ext:exit)))
	(setf *debugger-hook* #'exit)))
