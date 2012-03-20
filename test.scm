(load "sdl.o1")
(load "gl.o1")

(define-macro (when check . exprs)
  `(if ,check (begin ,@exprs) #f))

(define (critical-error . msgs)
  (apply println msgs)
  (exit 1))

(define (mainloop)
  (thread-sleep! 2))

(define window_width 640)
(define window_height 480)

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
               window_width window_height
               (bitwise-ior SDL_WINDOW_OPENGL SDL_WINDOW_SHOWN))))
    (when (not win)
      (critical-error "Unable to create render window" (SDL_GetError)))
    (let ((ctx (SDL_GL_CreateContext win)))
      (println "GL version seems to be " (glGetString GL_VERSION))
      (SDL_GL_SetSwapInterval 1)
      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (glOrtho 0.0 (exact->inexact window_width) (exact->inexact window_height) 0.0 0.0 1000.0)
      (glMatrixMode GL_MODELVIEW)
      (mainloop)
      (SDL_GL_DeleteContext ctx)
      (SDL_DestroyWindow win)
      (SDL_Quit))))

(test)
