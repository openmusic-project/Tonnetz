----- The Tonnetz library -----

--- File organization

The source code is divided in five files:
- Tonnetz.lisp, which contains the initialization source code for the
library.
- tonnetz-editor.lisp, which describes how the box and the editor
work.
- tonnetz-view.lisp, which describes the main view and the drawing of
the Tonnetz.
- tonnetz-tiles.lisp, which describes the tiles of the Tonnetz.
- tonnetz-utils.lisp, which contains some useful functions for
programming and the algebraic description of the internals of the
Tonnetz.

--- Installation

Copy or move the "Tonnetz" folder to the "Libraries" folder of OM.
No extra steps are needed for installation. You're set.

--- How does it work?

The Tonnetz library provides a single representation of the Tonnetz, which parameters (generators, chords, size, etc.) can be modified dynamically via the editor.

--- First Steps — the Basic Rig:

You must attach a second-order list (of lists) of pitches, such as ((0 4 7)) or ((0 4 7) (2 5 9)) — a first-order list of pitches, such as (0 4 7), won't work. You must attach it to the third inlet (the one on the right) of the TONNETZ box. Only then, evaluate the box (click on it once, and press v.) Double-click on the box, the editor opens, and you're set. 

Remember to always attach a second-order list of pitches to the box before evaluating the box and opening the editor.

--- Basic Key Commands:

'Space' — Plays the current chord.

'right-arrow' — Toggles the next chord, and plays it if "Auto-play" is enabled.

'left-arrow' — Toggles the first chord, and plays it if "Auto-play" is enabled.

'+'  — Increases the Zoom Level (the tiles' size.) 

'-'  — Decreases the Zoom Level (the tiles' size.) 

These are only some of the key commands. Use the 'h' key for more help.

