(asdf:oos 'asdf:load-op :lispbuilder-sdl)
(asdf:oos 'asdf:load-op :geometry)

(defstruct camera
  ;; camera direction vectors: the width (dir[0]) and height (dir[1]) of the projection screen as vectors and the vector to the middle of the projection screen (dir[2]). all in world coordinates   
  (origin (make-vector3d 0 0 0) :type vector3d)
  (width +unit3d-x+ :type vector3d)
  (width-length 1 :type rational)
  (height +unit3d-y+ :type vector3d)
  (height-length 1 :type rational)
  (depth +unit3d-z+ :type vector3d)) ;; depth has to have length 1

(defun camera-leftup (camera)
  (vector3d-add (camera-origin camera)
		(camera-depth camera)
		(vector3d-scale -1/2 (camera-height camera))
		(vector3d-scale -1/2 (camera-width camera))))

(defparameter *camera-origin* (make-camera :origin +origin3d+
					   :width (make-vector3d 1 0 0)
					   :width-length 1
					   :height (make-vector3d 0 2/3 0)
					   :height-length 2/3
					   :depth (make-vector3d 0 0 1)))
(defparameter *camera-top* (make-camera :origin (make-vector3d -5 -5 -5)
					   :width (make-vector3d 2 0 0)
					   :width-length 2
					   :height (make-vector3d 0 4/3 0)
					   :height-length 4/3
					   :depth (make-vector3d 0 0 1)))
(defparameter *camera-back* (make-camera :origin (make-vector3d 0 0 3)
					   :width (make-vector3d -1 0 0)
					   :width-length 1
					   :height (make-vector3d 0 -2/3 0)
					   :height-length 2/3
					   :depth (make-vector3d 0 0 -1)))

(defparameter *cameras* (list *camera-origin* *camera-top* *camera-back*))

(defparameter *camera* *camera-origin*)

(defstruct display
  (origin (make-vector2d 0 0) :type vector2d)
  (size (make-vector2d 300 200) :type vector2d)
  (camera *camera* :type camera))

(defparameter *displays* (list (make-display :origin (make-vector2d 0 0)
					     :size (make-vector2d 300 200)
					     :camera *camera-origin*)
			       (make-display :origin (make-vector2d 300 0)
					     :size (make-vector2d 300 200)
					     :camera *camera-top*)
			       (make-display :origin (make-vector2d 0 200)
					     :size (make-vector2d 300 200)
					     :camera *camera-back*)))

(defparameter *display* (car *displays*))

(defparameter *window-width*
  (apply #'max (mapcar (lambda (d) (+ (vector2d-x (display-origin d))
				      (vector2d-x (display-size d))))
		       *displays*)))

(defparameter *window-height*
  (apply #'max (mapcar (lambda (d) (+ (vector2d-y (display-origin d))
				      (vector2d-y (display-size d))))
		       *displays*)))


(defun camera-move-f (offset &key (camera *camera*))
  (asetf (camera-origin camera) (vector3d-add it offset)))

(defun camera-move (offset &key (camera *camera*))
  (camera-move-f offset :camera (copy-camera camera)))

(defun camera-rotate-f (axis angle &key (camera *camera*))
  (setf (camera-width camera)
	(vector3d-rotate-axis (camera-width camera) axis angle))
  (setf (camera-height camera)
	(vector3d-rotate-axis (camera-height camera) axis angle))
  (setf (camera-depth camera)
	(vector3d-rotate-axis (camera-depth camera) axis angle))
  camera)

(defun camera-rotate (axis angle &key (camera *camera*))
  (camera-rotate-f axis angle :camera (copy-camera camera)))

(defun camera-projector (&key (camera *camera*))
  (let ((projector (projector-into (camera-width camera)
				   (camera-height camera)
				   (camera-depth camera)))
	(camera-middle (vector3d-add (camera-origin camera)
				     (camera-depth camera))))
    (lambda (vector)
      (let ((diff (vector3d-sub vector camera-middle)))
	(funcall projector diff)))))

(defun camera-infront? (point &key (camera *camera*))
  (<= 0
      (vector3d-scalar (camera-depth camera)
		       (vector3d-sub point (camera-leftup camera)))))

(defun vector3d->2d (point3d)
  (let ((depth (1+ (vector3d-z point3d))))
    (assert (<= 1 depth))
    (make-vector2d (/ (vector3d-x point3d) depth)
		   (/ (vector3d-y point3d) depth))))

(defun display-point2d (point2d &key (display *display*))
  (let* ((dx (vector2d-x (display-origin display)))
	 (dy (vector2d-y (display-origin display)))
	 (dw (vector2d-x (display-size display)))
	 (dh (vector2d-y (display-size display))))
    (sdl:point :x (+ dx (floor (* (+ 1/2 (vector2d-x point2d)) dw)))
	       :y (+ dy (floor (* (+ 1/2 (vector2d-y point2d)) dh))))))

(defun display-line2d (from to &key (display *display*))
  (let ((sdl-from (display-point2d from :display display))
	(sdl-to (display-point2d to :display display)))
    (sdl:draw-line sdl-from sdl-to)))

;;(defun (polygon

(defun display-line (from to &key (display *display*))
  ;;(prind "display-line" from to)
  (let* ((projector (camera-projector :camera (display-camera display)))
	 (from-point (funcall projector from))
	 (to-point (funcall projector to)))
    ;;(prind "projected" from-point to-point)
    (cond
      ((and (<= 0 (vector3d-z from-point)) (> 0 (vector3d-z to-point)))
       (let ((factor (/ (- (vector3d-z to-point))
			(- (vector3d-z from-point) (vector3d-z to-point)))))
	 (setf to-point (make-vector3d (lerp factor
					     (vector3d-x to-point)
					     (vector3d-x from-point))
				       (lerp factor
					     (vector3d-y to-point)
					     (vector3d-y from-point))
				       0))))
      ((and (> 0 (vector3d-z from-point)) (<= 0 (vector3d-z to-point)))
       (let ((factor (/ (- (vector3d-z from-point))
			(- (vector3d-z to-point) (vector3d-z from-point)))))
	 (setf from-point (make-vector3d (lerp factor
					       (vector3d-x from-point)
					       (vector3d-x to-point))
					 (lerp factor
					       (vector3d-y from-point)
					       (vector3d-y to-point))
					 0)))))
    ;;(prind "corrected" from-point to-point)
    (if (and (<= 0 (vector3d-z from-point)) (<= 0 (vector3d-z to-point)))
	(display-line2d (vector3d->2d from-point)
			(vector3d->2d to-point)
			:display display))))


(defmethod display ((camera camera) &key (display *display*))
  (let* ((origin (camera-origin camera))
	 (c00 (camera-leftup camera))
	 (c10 (vector3d-add c00 (camera-width camera)))
	 (c01 (vector3d-add c00 (camera-height camera)))
	 (c11 (vector3d-add c01 (camera-width camera))))
    (display-line origin c00 :display display)
    (display-line origin c01 :display display)
    (display-line origin c10 :display display)
    (display-line origin c11 :display display)
    (display-line c00 c01 :display display)
    (display-line c01 c11 :display display)
    (display-line c11 c10 :display display)
    (display-line c10 c00 :display display)))

(defmethod display (it &key (display *display*))
  (declare (ignore display))
  (break (format nil "unknown display object type ~A" (type-of it))))

(defparameter *objects* (append *cameras*))

(defun display-all ()
  (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))
  (loop for d in *displays* do
       (mapcar (lambda (x) (display x :display d)) *objects*))
  (sdl:update-display))

(defun camera-mouse-move-sideways (m-x m-y)
  (camera-move-f (vector3d-add (vector3d-scale m-x (camera-width *camera*))
			       (vector3d-scale m-y (camera-height *camera*)))))

(defun camera-mouse-rotate (m-x m-y)
  (assert (= (vector3d-norm (camera-depth *camera*)) 1))
  (let* ((m-angle (atan (- m-y) (- m-x)))
	 (axis (vector3d-rotate-axis (camera-height *camera*)
				     (camera-depth *camera*)
				     m-angle))
	 (axis-norm (vector3d-scale (/ (camera-height-length *camera*))
				    axis))
	 (angle (/ (+ (pow2 m-x) (pow2 m-y)) 10)))
;;    (prind axis axis-norm (vector-norm axis))
    (if (> angle 0)
	(camera-rotate-f axis-norm angle))))

(defun mouse-motion-event (state x y x-rel y-rel)
  ;;(prind "mouse-motion" state x y x-rel y-rel)
  (let ((left-p (= 1 (ldb (byte 1 0) state)))
	(middle-p (= 1 (ldb (byte 1 1) state)))
	(right-p (= 1 (ldb (byte 1 2) state))))
    (flet ((mouse-offset (state x-rel y-rel)
	     (if state
		 (let* ((m-x (/ (- (float x-rel)) *window-width*))
			(m-y (/ (- (float y-rel)) *window-height*)))
		   (values t m-x m-y))
		 nil)))
      (multiple-value-bind (m-move m-x m-y)
	  (mouse-offset state x-rel y-rel)
	(if (and m-move left-p)
	    (progn
	      (camera-mouse-move-sideways m-x m-y)
	      (display-all)))
	(if (and m-move middle-p)
	    (progn
	      (camera-mouse-rotate m-x m-y)
	      (display-all)))
	))))
  

(defun event-loop ()

  (sdl:with-init ()
      
    (sdl:window *window-width* *window-height*)

    ;; (sdl:draw-surface
    ;;  (sdl:load-image
    ;;   "/usr/share/images/desktop-base/moreblue-orbit-splash.png"))
    
    (display-all)
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (format t "keypress:~A~%" key)
		       (sdl:draw-pixel-* (random 10) (random 10)
					 :color sdl:*white*)
		       (sdl:update-display)
		       (when (sdl:key= key :sdl-key-escape)
			 (sdl:push-quit-event)))
      ;; If the cursor is hidden using SDL-SHOW-CURSOR and the input
      ;; is grabbed using SDL-WM-GRAB-INPUT, then the mouse will give
      ;; relative motion events even when the cursor reaches the edge
      ;; of the screen. This is currently only implemented on Windows
      ;; and Linux/Unix-alikes.  (sdl:show-cursor nil)
      ;; (sdl:sdl-wm-grab-input 1)
      (:mouse-motion-event (:state state :x x :y y :x-rel x-rel :y-rel y-rel)
			   (mouse-motion-event state x y x-rel y-rel))
      (:video-expose-event ()
			   (progn
			     (format t "update-display~&")
			     (sdl:draw-pixel-* (random 10) (random 10)
					       :color sdl:*white*)
			     (display-all)
			     (sdl:update-display))))
    ))
  