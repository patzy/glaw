The goal of GLAW is to provide a collection of basic functionalities to help developping games.

Basic ideas are: 
 - core depends only on [cl-opengl](http://common-lisp.net/project/cl-opengl/)
 - do *not* force the user to a specific game genre
 - independent of windowing API (i.e. not tied to sdl or glut)


Examples depend on:
 - [glop](http://github.com/patzy/glop)  for windowing and input.
 - [imago](http://common-lisp.net/project/imago/) for texture loading.

It is possible to use the examples with lispbuilder-sdl and lispbuilder-sdl-image instead of glop
and imago.
Just change the `:depends-on` line in `glaw-examples.asd`, comment the GLOP part in
`glaw-examples.lisp` (from `;; Using GLOP` to `;; Using SDL`) and uncomment the SDL part
(from `;; Using SDL` to the end of the file).
 
