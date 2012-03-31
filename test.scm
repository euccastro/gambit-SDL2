(load "sdl")
(load "gl")
(load "ffi")
(load "math")

; Global state variables.

; Distance units are in pixels, time units are in seconds.
(define window-width 640)
(define window-height 480)
(define window-x #f)
(define window-y #f)
(define ball-x #f)
(define ball-y #f)
(define ball-vel-x 300.)
(define ball-vel-y 300.)
(define ball-radius 10.)
(define paddle-radius 40.)
(define paddle-x #f)
(define paddle-speed (* 3. (/ ball-vel-x 2.)))
(define two-pi (* 2 M_PI))
(define pressing-left #f)
(define pressing-right #f)

(define (paddle-y)
  (exact->inexact (+ window-y window-height)))

(define (with-window-extents fn)
  (fn window-x window-y
      (+ window-x window-width) (+ window-y window-height)))

(define (check-paddle-wall-collision)
  (with-window-extents
   (lambda (left-wall bottom-wall right-wall top-wall)
     (let ((paddle-left (- paddle-x paddle-radius))
	   (paddle-right (+ paddle-x paddle-radius)))
       (if (< paddle-left left-wall)
	   (set! paddle-x (+ left-wall paddle-radius)))
       (if (> paddle-right right-wall)
	   (set! paddle-x (- right-wall paddle-radius)))))))

(define (check-ball-wall-collision)
  (with-window-extents
   (lambda (left-wall bottom-wall right-wall top-wall)
     (let ((ball-left (- ball-x ball-radius))
	   (ball-bottom (- ball-y ball-radius))
	   (ball-right (+ ball-x ball-radius))
	   (ball-top (+ ball-y ball-radius)))
       (if (< ball-left left-wall)
	   (begin
	     (set! ball-x (+ left-wall ball-radius))
	     (if (< ball-vel-x 0)
		 (set! ball-vel-x (- ball-vel-x)))))
       (if  (< ball-bottom bottom-wall)
	    (begin
	      (set! ball-y (+ bottom-wall ball-radius))
	      (if (< ball-vel-y 0)
		  (set! ball-vel-y (- ball-vel-y)))))
       (if (> ball-right right-wall)
	   (begin
	     (set! ball-x (- right-wall ball-radius))
	     (if (> ball-vel-x 0)
		 (set! ball-vel-x (- ball-vel-x)))))
       (if (> ball-top top-wall)
	   (begin
	     (set! ball-y (- top-wall ball-radius))
	     (if (> ball-vel-y 0)
		 (set! ball-vel-y (- ball-vel-y)))))))))

(define (critical-error . msgs)
  (apply println msgs)
  (exit 1))

(define (on-resize width height)
  (glViewport 0 0 width height)
  (set! window-width width)
  (set! window-height height)
  (adjust-projection))

(define (adjust-projection)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (apply glOrtho
	 (map exact->inexact
	      (list window-x (+ window-x window-width)
		    (+ window-y window-height) window-y
		    0.0 1000.0)))
  (glMatrixMode GL_MODELVIEW))

(define (on-window-move x y)
  (set! window-x x)
  (set! window-y y)
  (adjust-projection))

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
	      ((= key SDLK_LEFT)
	       (set! pressing-left #t))
	      ((= key SDLK_RIGHT)
	       (set! pressing-right #t))
	      (else
	       (println "Unknown key: " key)))))
     ((= evt-type SDL_KEYUP)
      (let* ((kevt (SDL_Event-key event))
	     (key (SDL_Keysym-sym
		   (SDL_KeyboardEvent-keysym kevt))))
	(cond ((= key SDLK_LEFT)
	       (set! pressing-left #f))
	      ((= key SDLK_RIGHT)
	       (set! pressing-right #f))
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
	  (on-resize data1 data2))
	 ((= type SDL_WINDOWEVENT_MOVED)
	  (on-window-move data1 data2))
	 (else (println "Got window event " type)))))
     (else 'nevermind))))

(define (update dt)
  (set! ball-x (+ ball-x (* ball-vel-x dt)))
  (set! ball-y (+ ball-y (* ball-vel-y dt)))
  (let ((speed (* paddle-speed (- (if pressing-right 1 0)
				  (if pressing-left 1 0)))))
    (set! paddle-x (+ paddle-x (* speed dt))))
  (check-ball-wall-collision)
  (check-paddle-wall-collision))

(define draw-circle
  (let ((num-divisions 32))
    (lambda (center-x center-y radius)
      (glPushMatrix)
      (glTranslatef center-x center-y 0.)
      (glBegin GL_POLYGON)
      (let loop ((division 0.))
	(let ((angle (* (/ division num-divisions)
			two-pi)))
	  (glVertex2f (* radius (cos angle))
		      (* radius (sin angle)))
	  (if (< division num-divisions)
	      (loop (+ division 1.)))))
      (glEnd)
      (glPopMatrix))))

(define (render)
  (glClearColor 0.3 0.6 0.9 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (draw-circle ball-x ball-y ball-radius)
  (draw-circle paddle-x (paddle-y) paddle-radius))

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
		 (if (= (SDL_PollEvent evt*) 1)
		     (if (eq? (handle-event evt dt) 'quit)
			 (return)
			 (event-loop))))
	       (if (> dt 0) (update dt))
	       (render)
	       (SDL_GL_SwapWindow window)
	       (SDL_Delay 1)
	       (frame-loop now)))))))))

(define (test)
  (if (< (SDL_Init SDL_INIT_VIDEO) 0)
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
    (if (not win)
	(critical-error "Unable to create render window"
			(SDL_GetError)))
    (SDL_GetWindowPosition win xpos ypos)
    (set! window-x (dereference-read-int* xpos))
    (set! window-y (dereference-read-int* ypos))
    (on-resize window-width window-height)
    (println "Window started at (" window-x ", " window-y ").")
    (set! ball-x (+ window-x (/ window-width 2.)))
    (set! ball-y (+ window-y (/ window-height 2.)))
    (set! paddle-x ball-x)
    (println "Ball started at (" ball-x " " ball-y)
    (let ((ctx (SDL_GL_CreateContext win)))
      (println "GL version " (glGetString GL_VERSION))
      (SDL_GL_SetSwapInterval 1)
      (mainloop win)
      (SDL_GL_DeleteContext ctx)
      (SDL_DestroyWindow win)
      (SDL_Quit))))

(test)
