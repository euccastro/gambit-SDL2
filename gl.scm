;; OpenGL

(include "ffi.scm")

(c-declare "#include \"SDL_opengl.h\"")

(c-define-type GLenum unsigned-int)	
(c-define-type GLboolean unsigned-char)
(c-define-type GLbitfield unsigned-int)
(c-define-type GLvoid void)
(c-define-type GLbyte signed-char)
(c-define-type GLshort short)
(c-define-type GLint int)
(c-define-type GLubyte unsigned-char)
(c-define-type GLushort unsigned-short)
(c-define-type GLuint unsigned-int)
(c-define-type GLsizei int)
(c-define-type GLfloat float)
(c-define-type GLclampf float)
(c-define-type GLdouble double)
(c-define-type GLclampd double)

(c-constants
  GL_MATRIX_MODE
  GL_MODELVIEW
  GL_PROJECTION
  GL_TEXTURE
  GL_VENDOR
  GL_RENDERER
  GL_VERSION
  GL_EXTENSIONS)

(define glBegin
  (c-lambda (GLenum) void "glBegin"))

(define glClear
  (c-lambda (GLbitfield) void "glClear"))

(define glClearColor
  (c-lambda (GLclampf GLclampf GLclampf GLclampf) void "glClearColor"))

(define glEnd
  (c-lambda () void "glEnd"))

(define glGetString
  (c-lambda (GLenum) char-string "glGetString"))

(define glLoadIdentity
  (c-lambda () void "glLoadIdentity"))

(define glMatrixMode
  (c-lambda (GLenum) void "glMatrixMode"))

(define glOrtho
  (c-lambda (GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble) void
            "glOrtho"))

(define glVertex3f
  (c-lambda (GLfloat GLfloat GLfloat) void "glVertex3f"))
