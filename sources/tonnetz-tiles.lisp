(in-package :om)

; Tiles

; Classes and functions implementing the tiles and the drawing thereof.

; Represents a tile (note) in a Tone Network.

(defclass note-view (om-item-view)
  ((note :accessor note :initform 0 :initarg :note)
   (column :accessor col :initform 0 :initarg :column)
   (row :accessor row :initform 0 :initarg :row)
   (level :accessor level :initform '())
   (color :accessor color :initform (om-make-color 1 1 1))))

; Draws a tile.

(defmethod om-draw-contents ((self note-view))
  (let* ((x3 (w self))
         (x1 (/ x3 3))
         (c 4)
         (ystart (+ c (/ (h self) 6)))
         (y1 (- (h self) ystart))
         (y2 (/ (+ y1 ystart) 2))
         (name (netz-p2n (note self))))
    (om-with-focused-view self
      (om-with-fg-color
          self (if (member (currentchord (om-view-container (om-view-container self))) (level self)) (om-make-color 1 1 0) (color self)) 
        (om-fill-ellipse (/ (w self) 2) (/ (h self) 2) (- (/ (w self) 2) 2) (- (/ (w self) 2) 2)))
      (om-with-fg-color
          self (if (member (currentchord (om-view-container (om-view-container self))) (level self)) (om-make-color 1 1 1) (om-make-color 0.25 0.25 0.25))
        (om-draw-ellipse (/ (w self) 2) (/ (h self) 2) (- (/ (w self) 2) 2) (- (/ (w self) 2) 2))
        )
      ; Defines a cell's font, and its location within the cell.
     (om-with-font (om-make-font "Arial" (/ (w self) 3))
                   (case (length name)
                     (1 (om-draw-string (+ 3 x1) (+ 4 y2) name))
                     (2 (om-draw-string (+ 0 x1) (+ 4 y2) name)))))))

(defmethod add-to-currentchord ((self note-view))
  "Adds a note to the currently selected chord and displays it"
  (let ((cchord (currentchord (om-view-container (om-view-container self)))))
    (if (member cchord (level self))
        (setf (level self) (remove cchord (level self)))
      (push cchord (level self))))
  (setf (color self)
        (if (null (level self))
            (om-make-color 1 1 1)
          (om-make-color 
                         (+ 0.3125 (/ (+ 0.25 (apply #'max (level self))) (max-level (main-view (om-view-container (om-view-container self)))))) 
                         (+ 0.3125 (/ (+ 0.25 (apply #'max (level self))) (max-level (main-view (om-view-container (om-view-container self)))))) 
                         0.3125)))
  (generate-chords (om-view-container (om-view-container self)))
  (om-invalidate-view self))

(defmethod create-chord ((self note-view))
  "Creates a new chord and refreshes the main view"
  (let ((ed (om-view-container (om-view-container self))))
    (setf (chords (object (om-view-container (om-view-container self))))
          (append (chords (object (om-view-container (om-view-container self))))
                  (list (list (note self)))))
    (setf (currentchord ed) (max-level (main-view ed)))
    (refresh-main-view ed
                       (columns (main-view ed))
                       (lines (main-view ed))
                       (om-view-size (main-view ed))
                       (gens (object ed))
                       (tile-size (main-view ed)))
  ))

; Changes the state of a tile
(defmethod om-view-click-handler ((self note-view) position)
  ; Is the ctrl key pressed or not?
  (if (om-command-key-p)
      (create-chord self)
    (add-to-currentchord self)))
