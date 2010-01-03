Glaw
====
The goal of GLAW is to provide a collection of basic functionalities to help developping games.

Basic ideas are: 

 - core depends only on [cl-opengl](http://common-lisp.net/project/cl-opengl/)
 - do *not* force the user to a specific game genre
 - independent of windowing API (i.e. not tied to sdl or glut)


Examples
--------
A few examples are provided in the `examples/` directory, they depend on:

 - [glop](http://github.com/patzy/glop)  for windowing and input.
 - [imago](http://common-lisp.net/project/imago/) for texture loading.

It is possible to use the examples with lispbuilder-sdl and lispbuilder-sdl-image instead of glop
and imago.
To do so:

 - change the `:depends-on` line in `glaw-examples.asd`
 - commment the GLOP part in `glaw-examples.lisp` (from `;; Using GLOP` to `;; Using SDL`)
 - uncomment the SDL part (from `;; Using SDL` to the end of the file)
 
To load the examples do:
 
     (asdf:operate 'asdf:load-op :glaw-examples)
     
Then to run an example:
 
     (glaw-examples:run-example 'glaw-examples:example-name)
     
Where `example-name` may be one of the following:
 
 - `sprites`
 - `gui`
 - `particles`
 
There's also a simple breakout game using glaw [here](http://github.com/patzy/outbreak).
