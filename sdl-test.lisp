;(clc:clc-require :lispbuilder-sdl)
;(load "/usr/share/common-lisp/source/asdf/asdf.lisp")
;(asdf:oos 'asdf:load-op 'lispbuilder-sdl)
(asdf:operate 'asdf:load-op :lispbuilder-sdl)
;(require :lispbuilder-sdl)


(sdl:with-init ()
  
  (sdl:window 320 240)
  ;(sdl:draw-pixel-* 5 5)
  (sdl:draw-surface (sdl:load-image "/usr/share/images/desktop-base/moreblue-orbit-splash.png"))
  
  (sdl:update-display)
  (sdl:with-events ()
    (:quit-event () t)
    (:key-down-event (:key key)
		     (format t "keypress:~A~%" key)
		     (when (sdl:key= key :sdl-key-escape)
		       (sdl:push-quit-event)))
    (:mouse-motion-event (:state state :x x :y y :x-rel x-rel :y-rel y-rel)
			 (prind "mouse-motion" state x y x-rel y-rel))
    (:video-expose-event ()
			 (progn
			   (format t "update-display~&")
			   (sdl:draw-pixel-* 5 5)
			   (sdl:update-display))))
  )
