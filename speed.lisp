(load "~/quicklisp/setup.lisp")
(ql:quickload 'lhstats)
(asdf:oos 'asdf:load-op 'utils)

(defun plist-vs-hashtable (putlength)
  (let* ((pl nil)
	 (ht (make-hash-table))
	 (putit (loop repeat putlength collect (random 400000000)))
	 (getit (loop for i below 1000 collect i)))
    (dolist (i putit) (setf (getf pl i) i))
    (dolist (i putit) (setf (gethash i ht) i))
    (timediff ((loop for i in getit collect (aif (getf pl i) it 0)))
	      ((loop for i in getit collect (aif (gethash i ht) it 0)))
	      :showtimes t
	      :maxtime 2)))

(defun alist-vs-hashtable (putlength)
  (let* ((ht (make-hash-table))
	 (putit (loop repeat putlength collect (random 400000000)))
	 (al (pairlis putit putit))
	 (getit (loop for i below 1000 collect i)))
    (dolist (i putit) (setf (gethash i ht) i))
    (timediff ((loop for i in getit collect (aif (assoc i al) it 0)))
	      ((loop for i in getit collect (aif (gethash i ht) it 0)))
	      :showtimes t
	      :maxtime 2)))

(defun recurse-normal (depth)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0)))
  (if (= 0 depth)
      0
      (1+ (recurse-normal (1- depth)))))

(defun recurse-restart (depth)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0)))
  (restart-case
      (if (= 0 depth)
	  0
	  (1+ (recurse-restart (1- depth))))
    (restart-name ())))

(defun restart-vs-no-restart (depth)
  (timediff ((recurse-normal depth))
	    ((recurse-restart depth))
	    :showtimes t
	    :maxtime 2))
