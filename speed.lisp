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
	      :disable-gc t
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
	      :disable-gc t
	      :maxtime 2)))

(defun recurse-normal (depth)
  (if (= 0 depth)
      'result
      (recurse-normal (1- depth))))

(defun recurse-restart (depth)
  (restart-case
      (if (= 0 depth)
	  'result
	  (recurse-restart (1- depth)))
    (restart-name ())))

(defun restart-vs-no-restart (depth)
  (timediff ((recurse-normal depth))
	    ((recurse-restart depth))
	    :showtimes t
	    :maxtime 2))
