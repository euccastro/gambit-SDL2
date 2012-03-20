(include "ffi.scm")

(c-declare "#include \"SDL.h\"")

; SDL

(c-define-type SDL_EventType int)
(c-define-type SDL_GLattr int)
(c-define-type SDL_GLContext (pointer void))
(c-define-type SDL_Window* (pointer "SDL_Window"))
(c-define-type SDL_WindowEvent (struct "SDL_WindowEvent"))
(c-struct SDL_WindowEvent
  (type unsigned-int32)
  (timestamp unsigned-int32)
  (windowID unsigned-int32)
  (event unsigned-int8)
  (data1 int)
  (data2 int))
(c-define-type SDL_Keysym (struct "SDL_Keysym"))
(c-struct SDL_Keysym
  (
(c-define-type SDL_TextEditingEvent (struct "SDL_TextEditingEvent"))
(c-struct SDL_KeyboardEvent
  (type unsigned-int32)
  (timestamp unsigned-int32)
  (windowID unsigned-int32)
  (state unsigned-int8)
  (repeat unsigned-int8)
  (keysym SDL_Keysym void-star))


(c-define-type SDL_WindowFlags int)

(c-constants
  SDL_INIT_TIMER
  SDL_INIT_AUDIO
  SDL_INIT_VIDEO
  SDL_INIT_JOYSTICK
  SDL_INIT_HAPTIC
  SDL_INIT_NOPARACHUTE
  SDL_INIT_EVERYTHING
  SDL_WINDOWPOS_UNDEFINED
  SDL_WINDOWPOS_CENTERED
  SDL_WINDOW_FULLSCREEN
  SDL_WINDOW_OPENGL
  SDL_WINDOW_SHOWN
  SDL_WINDOW_HIDDEN
  SDL_WINDOW_BORDERLESS
  SDL_WINDOW_RESIZABLE
  SDL_WINDOW_MINIMIZED
  SDL_WINDOW_MAXIMIZED
  SDL_WINDOW_INPUT_GRABBED
  SDL_WINDOW_INPUT_FOCUS
  SDL_WINDOW_MOUSE_FOCUS
  SDL_WINDOW_FOREIGN
  SDL_GL_RED_SIZE
  SDL_GL_GREEN_SIZE
  SDL_GL_BLUE_SIZE
  SDL_GL_ALPHA_SIZE
  SDL_GL_BUFFER_SIZE
  SDL_GL_DOUBLEBUFFER
  SDL_GL_DEPTH_SIZE
  SDL_GL_STENCIL_SIZE
  SDL_GL_ACCUM_RED_SIZE
  SDL_GL_ACCUM_GREEN_SIZE
  SDL_GL_ACCUM_BLUE_SIZE
  SDL_GL_ACCUM_ALPHA_SIZE
  SDL_GL_STEREO
  SDL_GL_MULTISAMPLEBUFFERS
  SDL_GL_MULTISAMPLESAMPLES
  SDL_GL_ACCELERATED_VISUAL
  SDL_GL_RETAINED_BACKING
  SDL_GL_CONTEXT_MAJOR_VERSION
  SDL_GL_CONTEXT_MINOR_VERSION
  SDL_GL_CONTEXT_FLAGS
  SDL_GL_CONTEXT_PROFILE_MASK
  SDL_RELEASED
  SDL_PRESSED
  SDL_FIRSTEVENT
  SDL_QUIT
  SDL_WINDOWEVENT
  SDL_SYSWMEVENT
  SDL_KEYDOWN
  SDL_KEYUP
  SDL_TEXTEDITING
  SDL_TEXTINPUT
  SDL_MOUSEMOTION
  SDL_MOUSEBUTTONDOWN
  SDL_MOUSEBUTTONUP
  SDL_MOUSEWHEEL
  SDL_INPUTMOTION
  SDL_INPUTBUTTONDOWN
  SDL_INPUTBUTTONUP
  SDL_INPUTWHEEL
  SDL_INPUTPROXIMITYIN
  SDL_INPUTPROXIMITYOUT
  SDL_JOYAXISMOTION
  SDL_JOYBALLMOTION
  SDL_JOYHATMOTION
  SDL_JOYBUTTONDOWN
  SDL_JOYBUTTONUP
  SDL_FINGERDOWN
  SDL_FINGERUP
  SDL_FINGERMOTION
  SDL_TOUCHBUTTONDOWN
  SDL_TOUCHBUTTONUP
  SDL_DOLLARGESTURE
  SDL_DOLLARRECORD
  SDL_MULTIGESTURE
  SDL_CLIPBOARDUPDATE
  SDL_DROPFILE
  SDL_USEREVENT
  SDL_LASTEVENT
  )

(define SDL_CreateWindow
  (c-lambda (char-string int int int int unsigned-int32)
	    SDL_Window*
	    "SDL_CreateWindow"))

(define SDL_GetError
  (c-lambda () char-string "SDL_GetError"))

(define SDL_GL_CreateContext
  (c-lambda (SDL_Window*) SDL_GLContext "SDL_GL_CreateContext"))

(define SDL_GL_DeleteContext
  (c-lambda (SDL_GLContext) void "SDL_GL_DeleteContext"))

(define SDL_DestroyWindow
  (c-lambda (SDL_Window*) void "SDL_DestroyWindow"))

(define SDL_GL_GetAttribute
  (c-lambda (SDL_GLattr (pointer int)) int 
    ; In this and other functions taking C enums, explicitly casting to the
    ; appropriate enum type is required for compatibility with C++ compilers
    ; (building with these is untested, though).
    "___result = SDL_GL_GetAttribute((SDL_GLattr)___arg1, (int*)___arg2_voidstar);"))

(define SDL_GL_SetAttribute
  (c-lambda (SDL_GLattr int) int 
    "___result = SDL_GL_SetAttribute((SDL_GLattr)___arg1, ___arg2);"))

(define SDL_GL_SetSwapInterval
  (c-lambda (int) int "SDL_GL_SetSwapInterval"))

(define SDL_Init
  (c-lambda (unsigned-int32) int "SDL_Init"))

(define SDL_Quit
  (c-lambda () void "SDL_Quit"))

