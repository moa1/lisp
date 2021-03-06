(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :sdl2)

(defun fformat (stream string &rest rest)
  (apply #'format stream string rest)
  (finish-output))

(defun test-rendercopy ()
  (declare (optimize (debug 3)))
  (let* ((shape-size 16)
	 (src-r (sdl2:make-rect 0 0 shape-size shape-size))
	 (dest-r (sdl2:make-rect (floor 640 2) (floor 580 2) shape-size shape-size))
	 main-window
	 main-renderer
	 background-tx
	 blueshapes)
    (fformat t "create-window~%")
    (setf main-window (sdl2:create-window :x 0 :y 0 :w 640 :h 580))
    (fformat t "create-renderer~%")
    (setf main-renderer (sdl2:create-renderer main-window -1 '(:software)))
    (fformat t "main-renderer:~S~%" main-renderer)
    ;;(fformat t "(sdl2:get-renderer-info main-renderer):~S~%" (sdl2:get-renderer-info main-renderer))
    (fformat t "create-texture background-tx~%")
    (setf background-tx (sdl2:create-texture-from-surface main-renderer
							  (let (loading-surf)
							    (fformat t "load-bmp~%")
							    (setf loading-surf (sdl2:load-bmp "/home/toni/soft/SDL2-2.0.3/test/controllermap.bmp"))
							    (prog1 loading-surf (fformat t "loading-surf:~S~%" loading-surf))
							    ;;(fformat t "free-surface~%")
							    ;;(sdl2:free-surface loading-surf)
							    )))
    (fformat t "background-tx:~S~%" background-tx)
    (fformat t "create-texture blueshapes~%")
    (setf blueshapes (sdl2:create-texture-from-surface main-renderer
						       (let (loading-surf)
							 (fformat t "load-bmp~%")
							 (setf loading-surf (sdl2:load-bmp "/home/toni/soft/SDL2-2.0.3/test/axis.bmp"))
							 (prog1 loading-surf (fformat t "loading-surf:~S~%" loading-surf))
							 ;;(fformat t "free-surface~%")
							 ;;(sdl2:free-surface loading-surf)
							 )))
    (fformat t "blueshapes:~S~%" blueshapes)
    (dotimes (i 2)
      (dotimes (n 4)
	(fformat t "Main loop i:~S n:~S~%" i n)
	(setf (sdl2:rect-x src-r) (* shape-size (mod n 2)))
	(setf (sdl2:rect-y src-r) (if (> n 1) shape-size 0))
	(let ((r (random 256)) (g (random 256)) (b (random 256)))
	  (fformat t "set-render-draw-color ~S ~S ~S ~S~%" r g b 0)
	  (sdl2:set-render-draw-color main-renderer r g b 0))
	(fformat t "render-clear~%")
	(sdl2:render-clear main-renderer)
	(fformat t "render-draw-line~%")
	(sdl2:render-draw-line main-renderer 1 1 100 100)
	;;(sdl2-ffi.functions:sdl-render-draw-line main-renderer 1 1 100 100)
	
	(fformat t "render-copy background-tx~%")
	(fformat t "(sdl2-ffi.functions:sdl-render-copy main-renderer:~S background-tx:~S src-r:~S dest-r:~S)~%" main-renderer background-tx src-r dest-r)
	(sdl2:render-copy main-renderer background-tx)
	;;(sdl2-ffi.functions:sdl-render-copy main-renderer background-tx (cffi:null-pointer) (cffi:null-pointer))
	(fformat t "render-copy blueshapes~%")
	(sdl2:render-copy main-renderer blueshapes :source-rect src-r :dest-rect dest-r)
	
	(fformat t "render-present~%")

	(sdl2:render-present main-renderer)
	(sleep .5)
	))

    (sdl2:destroy-texture blueshapes)
    (sdl2:destroy-texture background-tx)
    (sdl2:destroy-renderer main-renderer)
    (sdl2:destroy-window main-window)))
