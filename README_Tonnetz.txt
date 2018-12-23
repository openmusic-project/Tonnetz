----- The Tonnetz library -----

--- Dependencies

The Tonnetz library depends on the reactive library developped by Jean
Bresson, so you'll need it to make it work. If you don't want it
anyway, you can comment the line 

(defclass TonnetzBox (#+om-reactive om::omreactiveboxeditcall
#-om-reactive om:OMBoxEditCall)

by

(defclass TonnetzBox (om::OMBoxEditCall)

--- File organization

The source code is divided in five files:
- Tonnetz.lisp, which contains the initialization source code for the
library.
- tonnetz-editor.lisp, which describes how the box and the editor
works.
- tonnetz-view.lisp, which describes the main view and the drawing of
the Tonnetz.
- tonnetz-tiles.lisp, which describes the tiles of the Tonnetz.
- tonnetz-utils.lisp, which contains some useful functions for
programming and the algebraic description of the internals of the
Tonnetz.

--- How does it work?

The Tonnetz library provides a single representation of the Tonnetz,
which parameters (generators, chords, size, etc.) can be modified
dynamically via the editor. You can act on this representation
directly in the editor, or by interleaving Tonnetz boxes and functions
acting on it.

You can also bind the library with the reactive library in order to
have real-time display of the chords in the Tonnetz! A patch is given
as example.

