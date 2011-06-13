Glaw
====
The goal of GLAW is to provide a collection of basic functionalities to help developping games.

Quickstart
-----------

### Using Quicklisp

You can get glaw directly from quicklisp by just doing:

    (ql:quickload "glaw")

This may not be the latest git version depending on when I pushed changes.

If you want the latest git version first clone this repository:

    git clone git://github.com/patzy/glaw.git
    
Then change to the new `glaw` directory and issue the `quickload` call from here.
    
If you want to try the examples just do:

    (ql:quickload "glaw-examples")

### Manual install

Get the following required dependencies (and their respective dependencies):

 - [cl-opengl](http://github.com/3b/cl-opengl/);
 - [cl-openal](http://github.com/sykopomp/cl-openal).

If you want to use the included extension you may need:

 - [imago](http://common-lisp.net/project/imago/): `glaw-imago` for image based assets loading
 - lispbuilder-sdl and lispbuilder-sdl-image: `glaw-sdl` for image based assets loading

Get ASDF and configure it so glaw's `.asd` are in the search path and then run:

    (asdf:operate 'asdf:load-op :glaw)

for the lib itself, or:

    (asdf:operate 'asdf:load-op :glaw-examples)
    
for the examples.

Examples
--------
A few examples are provided in the `examples/` directory, they depend on:

 - [glop](http://github.com/patzy/glop)  for windowing and input.
 - [imago](http://common-lisp.net/project/imago/) for texture loading.

It is possible to use the examples with lispbuilder-sdl and lispbuilder-sdl-image instead of glop
and imago.
To do so:

 - change the `:depends-on` line in `glaw-examples.asd`
 - use `:glaw-examples-sdl` in `*features*` instead of `:glaw-examples-glop`
 
Note that I don't test often with lb-sdl and it may not work/compile properly.
     
To run an example:
 
     (glaw-examples:run-example 'glaw-examples:example-name)
     
Where `example-name` may be one of the following:
 
 - `sprites`
 - `gui`
 - `particles`
 - `text`
 - `pathfinding`
 - `texture`
 - `tilemap`
 - `sound`
 - `skeletons`
 - `console`
 - `input`
 
In all examples you may use `ESC` or `q` to quit and `alt-s` to toggle render stats display.
 
There's also a simple breakout game using glaw [here](http://github.com/patzy/outbreak),
a more polished asteroid clone [here](http://github.com/patzy/kayou) and
an adaptation of the clans board game [here](http://github.com/patzy/clans).
