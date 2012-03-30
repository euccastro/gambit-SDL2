(load "sdl")
(load "gl")
(load "ffi")

(define window-width 640)
(define window-height 480)
(define window-x #f)
(define window-y #f)

(define-macro (when check . exprs)
  `(if ,check (begin ,@exprs) #f))

(define (critical-error . msgs)
  (apply println msgs)
  (exit 1))

(define (handle-event event dt)
  (let ((evt-type (SDL_Event-type event)))
    (cond
     ((= evt-type SDL_KEYDOWN)
      (let* ((kevt (SDL_Event-key event))
	     (key (SDL_Keysym-sym
		   (SDL_KeyboardEvent-keysym
		    kevt))))
	(cond ((= key SDLK_ESCAPE)
	       (println "Bye.")
	       'quit)
	      (else
	       (println "Unknown key: " key)))))
     ((= evt-type SDL_QUIT)
      (println "Got SDL_QUIT.  Bye!")
      'quit)
     ((= evt-type SDL_WINDOWEVENT)
      (let* ((wevt (SDL_Event-window event))
	     (type (SDL_WindowEvent-event wevt))
	     (data1 (SDL_WindowEvent-data1 wevt))
	     (data2 (SDL_WindowEvent-data2 wevt)))
	(cond
	 ((= type SDL_WINDOWEVENT_RESIZED)
	  (println "Got resize event: (" data1 ", " data2 ")."))
	 ((= type SDL_WINDOWEVENT_MOVED)
	  (println "Got move event: (" data1 ", " data2 ")."))
	 (else (println "Got window event " type)))))
     (else
      (println "Got event type " evt-type)))))

(define (update dt)
  'tbd)

(define (render)
  (glClearColor 0.3 0.6 0.9 1.0)
  (glClear GL_COLOR_BUFFER_BIT))

(define mainloop
  (let* ((evt (make-SDL_Event))
         (evt* (SDL_Event-pointer evt)))
    (lambda (window)
      (call-with-current-continuation
       (lambda (k)
	 (let ((return (lambda () (k #f)))
	       (old (SDL_GetPerformanceCounter)))
	   (let frame-loop ((old old))
	     (let* ((now (SDL_GetPerformanceCounter))
		    (dt (min 0.1
			     (/ (- now old)
				(SDL_GetPerformanceFrequency)))))
	       (let event-loop ()
		 (when (= (SDL_PollEvent evt*) 1)
		       (if (eq? (handle-event evt dt) 'quit)
			   (return)
			   (event-loop))))
	       (if (> dt 0) (update dt))
	       (render)
	       (SDL_GL_SwapWindow window)
	       (SDL_Delay 1)
	       (frame-loop now)))))))))

(define (test)
  (when (< (SDL_Init SDL_INIT_VIDEO) 0)
	(critical-error "Couldn't initialize SDL!"))
  (SDL_GL_SetAttribute SDL_GL_DOUBLEBUFFER 1)
  (SDL_GL_SetAttribute SDL_GL_DEPTH_SIZE 24)
  (SDL_GL_SetAttribute SDL_GL_RED_SIZE 8)
  (SDL_GL_SetAttribute SDL_GL_GREEN_SIZE 8)
  (SDL_GL_SetAttribute SDL_GL_BLUE_SIZE 8)
  (SDL_GL_SetAttribute SDL_GL_ALPHA_SIZE 8)
  (let ((win (SDL_CreateWindow
	      "Hello SDL"
	      SDL_WINDOWPOS_CENTERED
	      SDL_WINDOWPOS_CENTERED
	      window-width window-height
	      (bitwise-ior SDL_WINDOW_OPENGL
			   SDL_WINDOW_RESIZABLE
			   SDL_WINDOW_SHOWN)))
        (xpos (make-int*))
	(ypos (make-int*)))
    (when (not win)
	  (critical-error "Unable to create render window"
			  (SDL_GetError)))
    (SDL_GetWindowPosition win xpos ypos)
    (set! window-x (dereference-read-int* xpos))
    (set! window-y (dereference-read-int* ypos))
    (println "Window started at (" window-x ", " window-y ").")
    (let ((ctx (SDL_GL_CreateContext win)))
      (println "GL version seems to be " (glGetString GL_VERSION))
      (SDL_GL_SetSwapInterval 1)
      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (glOrtho 0.0 (exact->inexact window-width)
               0.0 (exact->inexact window-height)
               0.0 1000.0)
      (glMatrixMode GL_MODELVIEW)
      (mainloop win)
      (SDL_GL_DeleteContext ctx)
      (SDL_DestroyWindow win)
      (SDL_Quit))))

(test)
