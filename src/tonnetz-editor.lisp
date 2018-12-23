(in-package :tnetz)

;;;=======================
;;; EDITOR
;;;=======================

;;; main editor
(defclass tonnetz-editor (om::editorview)
  ((main-view :accessor main-view :initform nil :initarg :main-view)
   (controls-view :accessor controls-view :initform nil :initarg :controls-view)
   (max-n-chords :accessor max-n-chords :initform 5)
   (current-chord :accessor cchord :initform 0)
   (auto-play :accessor auto-play :initform nil)
   (columns :accessor columns :initform 12 :initarg :columns)
   (rows :accessor rows :initform 12 :initarg :rows)
   (cyclic :accessor cyclic :initform t)
   (played-chord :accessor chrd :initform nil)))

(defmethod get-box ((self tonnetz-editor))
  "Returns a reference to the editor's box"
  (om::ref self))

;;; the editor control panel
(defclass tonnetz-control (om::om-view)
  ((chord-text-view :accessor chord-text-view :initform nil :initarg :chord-text-view)
   (columns-edit :accessor columns-edit)
   (lines-edit :accessor lines-edit)))

;;; main tonnetz view 
(defclass tonnetz-view (om::om-view)
  ((tiles :accessor tiles :initform nil)
   (paths :accessor paths :initform nil)
   ;(gens :accessor gens :initarg :gens)
   (notes :accessor notes :initform nil)
   (tile-size :accessor tile-size :initform 36 :initarg :tile-size)))

(defmethod editor ((self tonnetz-view)) 
  (om-view-container self))

(defmethod columns ((self tonnetz-view)) 
  (columns (editor self)))

(defmethod rows ((self tonnetz-view)) 
  (rows (editor self)))

;;;======================
;; Represents a tile (a note) in a Tonnetz
(defclass note-view () ;(om-item-view)
  ((note :accessor note :initform 0 :initarg :note)
   (col :accessor col :initform 0 :initarg :col)
   (row :accessor row :initform 0 :initarg :row)
   (x :accessor x :initform 0 :initarg :x)
   (y :accessor y :initform 0 :initarg :y)
   (w :accessor w :initform 10 :initarg :w)
   (h :accessor h :initform 10 :initarg :h)
   (level :accessor level :initform '())
   (color :accessor color :initform (om-make-color 1 1 1))))

 ;; Draws the tonnetz
(defmethod initialize-instance ((self tonnetz-view) &rest args)
  (call-next-method)
  ;(om-set-bg-color self (om-make-color 1 1 1))
  ;(apply #'om-add-subviews (cons self (tiles self))) ;; Too long list
  ;;(when (tiles self)
  ;  (mapcar (lambda (elt) (om-add-subviews self elt)) (tiles self)))
  )

(defmethod add-tiles ((self tonnetz-view))
  ;(apply 'om-add-subviews
  ;       (cons self 
               (setf (tiles self) (make-tiles self))
  ;             ))
  )


(defun get-tile-position (i j w h)
  (om-make-point (* i w) (+ (if (oddp i) (/ h 2) 0) (* j h))))

(defun set-position (tile pos)
  (setf (x tile) (om-point-x pos))
  (setf (y tile) (om-point-y pos)))
  
(defun set-size (tile size)
  (setf (w tile) (om-point-x size))
  (setf (h tile) (om-point-y size)))


(defmethod make-tiles ((self tonnetz-view))
  "Adds the tiles to the view. Call it once the view is added in the editor"
  (when (om-view-container self) ;; uses the object of its container
  (set-tile-size (om-view-container self))
  (let* ((cols (columns self))
         (rows (rows self))
         (width (car (tile-size self)))
         (height (cadr (tile-size self)))
         (tnetz (om::object (om-view-container self))))
    ;; Initializing the tiles
    (reverse 
     (loop for x in (om::arithm-ser 0 (1- cols) 1)
           for n1 in (computeMelody (gen-funs tnetz) 0 
                                    (mapcar #'list (apply #'append (make-list cols :initial-element '(1 3)))))
           append
           (loop for y in (om::arithm-ser 0 (- rows (if (oddp x) 2 1)) 1)
                 for n2 in (computeMelody (gen-funs tnetz) n1 (mapcar #'list (make-list cols :initial-element 2)))
                 collect
                 (let ((pos (get-tile-position x y width height)))
                       
                 (om-make-view 'note-view
                               :col  x
                               :row y
                               :note n2
                               :x (om-point-x pos) :y (om-point-y pos)
                               :w width :h height
                               ;:bg-color :transparent
                               )
                 )))))))

(defmethod set-tile-size ((self tonnetz-editor))
  (setf (tile-size (main-view self))
        (list (/ (om::w (main-view self)) (columns self)) 
              (/ (om::h (main-view self)) (rows self)))))

(defmethod zoom ((self tonnetz-editor) zoom)
  (setf (columns self)
        (max 6 (round (* (columns self) zoom)))
        (rows self)
        (max 6 (round (* (rows self) zoom))))
  (set-tile-size self)
  (add-tiles (main-view self))
  (color-notes self)
  (om-invalidate-view (main-view self)))

(defmethod om::update-subviews ((self tonnetz-editor))
  ;; Called on resizing
  (call-next-method)
  (om-set-view-size (main-view self) (om-subtract-points (om-view-size self) (om-make-point 200 40)))
  (om-set-view-position (controls-view self) (om-make-point (- (om::w self) 160) 20))
  (om-set-view-size (controls-view self) (om-make-point 140 (- (om::h self) 40)))
  ;; redraw the main-view
  (set-tile-size self)
  (om-with-delayed-redraw (main-view self)
  (mapc #'(lambda (tile)
            (set-position tile (get-tile-position (col tile) (row tile) 
                                                  ;(tile-size (main-view self)) 
                                                  ;(* 1.5 (tile-size (main-view self)))
                                                  (car (tile-size (main-view self))) 
                                                  (cadr (tile-size (main-view self)))
                                                  ))
            (set-size tile (om-make-point (car (tile-size (main-view self))) 
                                          (cadr (tile-size (main-view self))) 
                                          ;(* 1.5 (tile-size (main-view self)))
                                          ))
            )
        (tiles (main-view self)))
  )
  ;(refresh-main-view self 
  ;                   (columns (main-view self))
  ;                   (rows (main-view self))
  ;                   (om-view-size (main-view self))
  ;                   (gen-funs (om::object self))
  ;                   (max (/ (- (om::w self) 160) (columns (main-view self))) (/ (om::h self) (rows (main-view self)))))
  )


(defmethod om-draw-contents ((self tonnetz-view))
  (let* ((width (car (tile-size self)))
         (height (cadr (tile-size self)))
         ;(height (* 1.5 (tile-size self)))
         ;; Diagonals (from 0,0 to x,y)
         (startx1 0)
         (starty1 (+ 4 (/ height 6)))
         (endx1 (* width (columns self)))
         (endy1 (+ starty1 (* height (/ (columns self) 2))))
         ;; Diagonals (from 0,y to x,0)
         (startx2 0)
         (starty2 (+ starty1 (* height (/ (columns self) 2))))
         (endx2 (* width (columns self)))
         (endy2 (+ 4 (/ height 6)))
         )
  (om-with-focused-view self
    (om-with-fg-color self (om-make-color 0 0 0)
      ;; Diagonals (from 0,0 to x,y)
      (loop for x in (mapcar (lambda (e) (* 2 e)) (iota (/ (columns self) 2))) do
            (om-draw-line (+ startx1 (* x width)) starty1 (+ (* x width) endx1) endy1))
      (loop for y in (iota (rows self)) do
            (om-draw-line startx1 (+ (* y height) starty1) endx1 (+ (* y height) endy1)))
      ;; Diagonals (from 0,y to x,0)
      (loop for x in (mapcar (lambda (e) (- (* 2 e) 1)) (iota (/ (columns self) 2))) do
            (om-draw-line (- startx2 (* x width)) starty2 (- endx2 (* x width)) endy2))
      (loop for y in (iota (rows self)) do
            (om-draw-line startx2 (+ (* y height) (+ (/ height 2) starty2)) endx2 (+ (* y height) (+ (/ height 2) endy2))))
      ;; Vertical rows
      (dotimes (x (columns self))
        (om-draw-line (+ (* x width) (/ width 2)) 
                      6 ;(+ (if (evenp x) (/ height 2) 0) starty1) 
                      (+ (/ width 2) (* x width)) 
                      ;(+ (if (evenp x) (/ height 2) 0) starty1 (* height (rows self)))
                      (- (om::h self) 6)
                      ))
      )
    
    (loop for ti in (tiles self) do
          (draw-tile ti self))

    )))

;; Draws a tile
;(defmethod om-draw-contents ((self note-view))
;  (draw-tile self nil))

(defun draw-tile (self &optional on-view)
  (let* ((rx (/ (w self) 2))
         (ry (/ (h self) 2))
         (cx (+ (x self) rx))
         (cy (+ (y self) ry))
         (border (* rx 0.3))
         (name (stringNote (note self)))
         (view (or on-view self)))
    (om-with-focused-view view
      ;(om-with-fg-color view (om::om-random-color)
      ;  (om-fill-rect (x self) (y self) (w self) (h self)))
      (om-with-fg-color view 
          (if (member (cchord (editor view)) (level self)) 
              (om-make-color 1 0.5 0.4) 
            (color self)
            ) 
         (om-fill-ellipse cx cy (- rx border) (- ry border)))
      (om-with-fg-color view 
          (if (member (cchord (editor view)) (level self)) 
                   (om-make-color 1 1 0) 
            (om-make-color 0 0 0))
        (om-draw-ellipse cx cy (- rx border) (- ry border))
        )
      
      (let ((refsize (min (w self) (h self))))
     (om-with-font (om-make-font "Arial" (/ refsize 3))
                   (case (length name)
                     (1 (om-draw-string (- cx (/ refsize 8)) (+ cy (/ refsize 10)) name))
                     (2 (om-draw-string (- cx (/ refsize 6)) (+ cy (/ refsize 10)) name))
                     ))
     )
      )))




(defmethod om::update-editor-after-eval ((self tonnetz-editor) val)
  ;; After evaluation, we want a complete redraw of the Tonnetz
  (call-next-method) ;; will set (value self) to val
  (reset-notes self)
  (color-notes self)
  )

(defmethod color-notes ((self tonnetz-editor))
  (let ((chords (om::first-n (reverse (chords (om::object self))) (max-n-chords self))))
  (loop for c in (cdr chords)
        do (loop for tile in (tiles (main-view self))
                           when (find (note tile) c :test '=)
                           do (setf (color tile) (if (= n 1) 
                                                     (om-make-color 1 0.6 0.4)
                                                   (om-make-color 0.4 0.8 0.8)))
                           ))
  (om-invalidate-view (main-view self))))


(defmethod color-notes ((self tonnetz-editor))
  (let ((chords (om::first-n (reverse (chords (om::object self))) (max-n-chords self))))
  (loop for c in (cdr chords)
        do (loop for tile in (tiles (main-view self))
                 when (find (note tile) c :test '=)
                 do (setf (color tile) (om-make-color 1 0.9 0.6))))
  (loop for tile in (tiles (main-view self))
        when (find (note tile) (car chords) :test '=)
        do (setf (color tile) (om-make-color 1 0.6 0.4)))
                
  (om-invalidate-view (main-view self))))


;;;===============================

(defmethod add-to-cchord ((self note-view))
  "Adds a note to the currently selected chord and displays it"
  (let ((curchord (cchord (om-view-container (om-view-container self)))))
    (if (member curchord (level self))
        (setf (level self) (remove curchord (level self)))
      (push curchord (level self))))
  (setf (color self)
        (if (null (level self))
            (om-make-color 1 1 1)
          (om-make-color 0.5 
                         (/ (apply #'max (level self)) (max-n-chords (main-view (om-view-container (om-view-container self))))) 
                         (/ (apply #'max (level self)) (max-n-chords (main-view (om-view-container (om-view-container self))))))))
  (generate-chords (om-view-container (om-view-container self)))
  (om-invalidate-view self))

(defmethod create-chord ((self note-view))
  "Creates a new chord and refreshes the main view"
  (let ((ed (om-view-container (om-view-container self))))
    (setf (chords (om::object (om-view-container (om-view-container self))))
          (append (chords (om::object (om-view-container (om-view-container self))))
                  (list (list (note self)))))
    (setf (cchord ed) (max-n-chords (main-view ed)))
    (refresh-main-view ed
                       (columns ed)
                       (rows ed)
                       (om-view-size (main-view ed))
                       (gen-funs (om::object ed))
                       (tile-size (main-view ed)))
  ))


(defmethod reset-notes ((self tonnetz-editor))
  "Blanks the tonnetz"
  (mapcar (lambda (elt) 
            (progn
              (setf (level elt) '())
              (setf (color elt) (om-make-color 1 1 1)))) 
          (tiles (main-view self))))


;; Changes the state of a tile
(defmethod om-view-click-handler ((self note-view) position)
  ;; Is the ctrl key pressed or not?
  (if (om-command-key-p)
      (create-chord self)
    (add-to-cchord self)))

;;;==========================================


(defun fromPath1 (path1 cols rows)
  "Computes the position in the grid from a Tonnetz path"
  (flet ((tmp (v)
             (case v
               (0 (+ 1 (- rows)))
               (1 (+ rows))
               (2 1)
               (3 (- rows 1))
               (4 (- rows))
               (5 -1))))
    (reduce #'+ (mapcar #'tmp path1))))

(defun fromPath (path cols rows)
  (mapcar (lambda (x) (fromPath1 x cols rows)) path))

;(defmethod refresh-main-view ((self tonnetz-editor) cols rows size gens tile-size)
;  "Deletes and recreate the main view"
;  (om-remove-subviews self (main-view self))
;  (setf (main-view self)
;          (om-make-view 'tonnetz-view :position (om-make-point 20 20)
;                        :columns cols
;                        :rows rows
;                        :size size
;                        ;:gens gens
;                        :tile-size tile-size))
;  
;  (om-add-subviews self (main-view self))
;  (add-tiles (main-view self))
;  (light-notes self)
;  (om::report-modifications self))

(defmethod initialize-instance ((self tonnetz-editor) &rest args)
  "Initializes the tonnetz editor window"
  (call-next-method)
  (om-set-bg-color self (om-make-color 0.3 0.3 0.3))
  (setf (main-view self) 
        (om-make-view 'tonnetz-view :position (om-make-point 20 20)
                      :size (om-make-point 600 600) ;;; will be changed by update-subviews
                      ;:gens (gen-funs (om::object self))
                      ))
  (setf (controls-view self) 
        (om-make-view 'tonnetz-control
                      :position (om-make-point (- (om::w self) 100) 20)
                      :size (om-make-point 80 (- (om::h self) 40))
                      :bg-color :transparent))

  (om-add-subviews (controls-view self)
                   ;; Columns
                   ;(setf (columns-edit (controls-view self))
                   ;      (om-make-dialog-item 'om::edit-numbox
                   ;                           (om-make-point 0 0)
                   ;                           (om-make-point 120 20)
                   ;                           (format nil " ~D" (columns self))
                   ;                           :value (columns self)
                   ;                           ;; Makes the selector fail
                   ;                           ;:di-action #'(lambda (item)
                   ;                           ;               (om::set-value item (round (parse-integer (om-dialog-item-text item)) 2)))
                   ;                           :afterfun #'(lambda (item)
                   ;                                         (if (cyclic self)
                   ;                                             (refresh-main-view self 
                   ;                                                                (* 2 (round (parse-integer (om-dialog-item-text item)) 2))
                   ;                                                                (rows self)
                   ;                                                                (om-view-size (main-view self))
                   ;                                                                (gens (main-view self))
                   ;                                                                (tile-size (main-view self)))
                   ;                                           (om::update-subviews self)))))
                   ;; Rows
                   ;(setf (lines-edit (controls-view self))
                   ;      (om-make-dialog-item 'om::edit-numbox
                   ;                           (om-make-point 0 20)
                   ;                           (om-make-point 120 20)
                   ;                           (format nil " ~D" (rows self))
                   ;                           :value (rows self)
                   ;                           :afterfun #'(lambda (item)
                   ;                                         (if (cyclic self)
                   ;                                             (refresh-main-view self 
                   ;                                                                (columns self) 
                   ;                                                                (* 2 (round (parse-integer (om-dialog-item-text item)) 2))
                   ;                                                                (om-view-size (main-view self))
                   ;                                                                (gens (main-view self))
                   ;                                                                (tile-size (main-view self)))
                   ;                                           (om::update-subviews self)))))
                    ;; Indicates the length of the window (number of chords activated)
                    (setf (chord-text-view (controls-view self))
                          (om-make-dialog-item 'oa::om-text-view
                                               (om-make-point 10 50)
                                               (om-make-point 120 20)
                                               (chord-text self)
                                               :fg-color *om-white-color*
                                               :bg-color :transparent
                                               ))
                    ;; List of generators
                    (om-make-dialog-item 'oa::om-pop-up-dialog-item
                                               (om-make-point 10 80)
                                               (om-make-point 120 20)
                                               ""
                                               :range (mapcar 'car *tonnetz-generators*)
                                               :value NIL ;;; HOW TO FIND THE VALUE FROM THE TONNETZ ???
                                               :di-action #'(lambda (item)
                                                              (setf (gen-funs (om::object self))
                                                                    (eval (cadr (nth (om-get-selected-item-index item) *tonnetz-generators*))))
                                                              (refresh-main-view self 
                                                                          (columns (main-view self))
                                                                          (rows (main-view self))
                                                                          (om-view-size (main-view self))
                                                                          (gen-funs (om::object self))
                                                                          (tile-size (main-view self)))
                                                              ))
                    ;; Switches the chord autoplay
                    (om-make-dialog-item 'oa::om-check-box
                                         (om-make-point 10 120)
                                         (om-make-point 120 20)
                                         "Auto-play on switch"
                                         :fg-color *om-white-color*
                                         :value (auto-play self) 
                                         :di-action #'(lambda (item)
                                                        (setf (auto-play self)
                                                              (om-checked-p item)))
                                         )
                    (om-make-dialog-item 'oa::om-check-box
                                         (om-make-point 10 120)
                                         (om-make-point 120 200)
                                         "Cyclic or simili-infinite"
                                         :fg-color *om-white-color*
                                         :value (auto-play self) 
                                         :di-action #'(lambda (item)
                                                        (setf (cyclic self)
                                                              (om-checked-p item)))
                                         )
                    (om-make-dialog-item 'oa::om-text-view
                                         (om-make-point 10 160)
                                         (om-make-point 60 20)
                                         "Memory"
                                         :fg-color *om-white-color*
                                         :bg-color :transparent
                                         )
                    ;; Allows the user to change the number of displayed chords - the trace
                    (om-make-dialog-item 'om::edit-numbox
                                   (om-make-point 70 158)
                                   (om-make-point 40 20) 
                                   (format nil " ~D" (max-n-chords self))
                                   :bg-color *om-white-color*
                                   :value (max-n-chords self)
                                   :min-val 1 :max-val 30
                                   :afterfun #'(lambda (item)
                                                 (setf (max-n-chords self) (om::value item))
                                                 (om-set-dialog-item-text (chord-text-view (controls-view self)) (chord-text self))
                                                 ;; Change the colors
                                                 (erase-notes self)
                                                 (light-notes self)
                                                 (om-invalidate-view (main-view self)))
                                   ))
  (om-add-subviews self (main-view self) (controls-view self))
  
  (add-tiles (main-view self))
  ;; Lighting the notes from the chords if chords isn't empty
  (light-notes self)
  (om-invalidate-view (main-view self)))




(defun tiles-num (cols rows)
  "Returns the number of tiles, given the number of columns and rows"
  (- (* cols rows) (floor (/ cols 2))))

(defmethod draw-light ((self tonnetz-editor) poslist)
  "Toggles the state of each tile"
  (loop for k in poslist do
        (let ((i (cadr k))
              (l (car k)))
          (loop for x in l do
                (push i (level (nth x (tiles (main-view self)))))
                (setf (color (nth x (tiles (main-view self)))) (om-make-color 0.5 
                                                                              (/ i (max-n-chords self))
                                                                              (/ i (max-n-chords self))))))))

(defmethod light-notes ((self tonnetz-editor))
  "Light the selected notes given that the tonnetz is correctly initialized"
  (let* ((tiles (tiles (main-view self)))
         (tchs (last (chords (om::object self)) (max-n-chords self)))
         (pchs (mapcar #'permute tchs))
         (gs (gen-funs (om::object self)))
         (tpaths (mapcar (lambda (tmp) (mapcar (lambda (c) (list c (computePath gs c))) tmp)) pchs)) ;; Paths associated to a given chord
         (chordnpaths (mapcar #'most-compact1 tpaths))
         (paths (mapcar #'cadr chordnpaths))
         (chs (mapcar #'car chordnpaths))
         (cols (columns (main-view self)))
         (rows (rows (main-view self))))
    ;(setf (max-n-chords self) (length paths))
    (if chs
        (progn 
          (draw-light self (compute-paths self paths cols rows chs gs))
          '()))))

(defmethod compute-paths ((self tonnetz-editor) paths cols rows chs gs)
  (let ((index (position 0 (mapcar #'note (tiles (main-view self))) :start (* 6 rows))) ;; We have to search for a tile with a 0 note (C)
        ;; The best would be to look for such a tile, with the requirement that the path fits into the tonnetz
        (tmpl '())
        (bestlastnote 0)
        (poslist '()))
    (setf (paths (main-view self)) paths)
    (loop for c in paths
          for i in (iota (min (max-n-chords self) (length paths))) do
          (if (/= i 0)
              (setf bestlastnote (car (min-ch 
                                       (mapcar (lambda (note) (list note (cadr (chord-distance gs note (nth i chs))))) (nth (- i 1) chs)) 
                                       #'cadr))
                    index (nth (position bestlastnote (nth (- i 1) chs)) tmpl)))
          (let* ((fstnote (fromPath1 (computePathb gs (interval bestlastnote (car (nth i chs)))) cols rows))
                 (l (tnetz:scanl1 #'+ (cons (setf index (+ fstnote index)) (fromPath c cols rows)))))
            (push (list l i) poslist)
            (setf tmpl l)
            ))
    poslist))

;; Bound to the button action
;; To be removed, maybe
;; Necessary for tiles and stuff
(defmethod generate-chords ((self tonnetz-editor))
  (let ((tonz (om::object self))
        (tiles (tiles (main-view self))))
    (setf (chords tonz)
          (remove-if #'null
                     (loop for x in (iota (max-n-chords self)) collect
                           (loop for n in (remove-if (lambda (l) (not (member x (level l)))) tiles) collect
                                 (note n)))))
    ;; Might be wanted or not. Maybe add a boolean?
    ;(erase-notes self)
    ;(light-notes self)
    ;;; update-panel invalidates the editor and reports the modification to the box
    (om::update-panel self t)))


;; 


(defmethod chord-text ((self tonnetz-editor))
  "String displaying the currently selected chord"
  (concatenate 'string "Chord:" (write-to-string (+ 1 (cchord self))) "/" (write-to-string (max-n-chords self))))

(defmethod transpose-chords-tonn ((self tonnetz-editor) genindex)
  "Transposes the set of chords given a generator (an index in the list of generators)"
  (let ((trans (nth genindex (gen-funs (om::object self)))))
    (setf (chords (om::object self))
          (mapcar (lambda (l) (mapcar (lambda (elt) (funcall trans elt)) l)) (chords (om::object self)))))
  (erase-notes self)
  (light-notes self))

(defun transchord (tchord)
  "Transposes a Tonnetz chord to a midicent-based one"
  (mapcar (lambda (note) (* 100 (+ (* 12 4) note))) tchord))

(defmethod play-tonnetz-chord ((self tonnetz-editor))
  "Helper function which plays the currently selected chord"
  (let* ((lastc (nth (cchord self) (chords (om::object self))))
         (chord (transchord lastc)))
    (setf c (make-instance 'om::chord :lmidic chord))
    (om::play c)
    ))

(defmethod set-current-chord ((self tonnetz-editor) n)
  "Modifies the currently selected chord and plays it if selected by the user"
  (setf (cchord self) n)
  (om-set-dialog-item-text (chord-text-view (controls-view self)) (chord-text self))
  (when (auto-play self)
    (play-tonnetz-chord self)))

(defmethod om::handle-key-event ((self tonnetz-editor) key)
  (case key 
    (:om-key-tab ;; Changes the current selected chord
     (set-current-chord self (mod (1+ (cchord self)) (max-n-chords self)))
     )
    (#\- ;; Decreases the size of a tile
         (zoom self 0.8))
    (#\+ ;; Increases the size of a tile
         (zoom self 1.2)) ;; Dummy values
    (#\Z (print "key = Z"))
    (#\c ;; Manages the change between the views: one chord at a time, or several chords at once
         ;; Not yet implemented
         )
    ;; Transposition keys
    (#\t
     (transpose-chords-tonn self 5))
    (#\y
     (transpose-chords-tonn self 2))
    (#\h
     (transpose-chords-tonn self 3))
    (#\f 
     (transpose-chords-tonn self 4))
    (#\v 
     (transpose-chords-tonn self 0))
    (#\b
     (transpose-chords-tonn self 1))
    (#\Space 
     (play-tonnetz-chord self))
    (otherwise (print key)))
  (om-invalidate-view self)
  )

;; 
;(defmethod resize-tiles ((self tonnetz-editor) factor)
;  "Resizes the tile network given a resizing factor"
;  (let ((tsize (* factor (tile-size (main-view self))))
;        (viewsize (om-view-size (main-view self))))
;    (om-remove-subviews self (main-view self))
;    (setf (main-view self)
;          (om-make-view 'tonnetz-view :position (om-make-point 20 20)
;                        :size viewsize ;;; will be changed by update-subviews
;                       :gens (gen-funs (om::object self))
;                       :tile-size tsize))
;    (om-add-subviews self (main-view self))
;    (light-notes self)))



;;;;;;;;;;;;;
;; Box drawing

(defun gen-to-pos (g)
  "Associates a direction (a two-coordinates vector) to each generator"
  (let ((x 35)
        (y 40))
    (apply #'om-make-point
           (case g
             (0 (list (- x) (/ y -2)))
             (1 (list x (/ y -2)))
             (2 (list 0 (- y)))
             (3 (list x (/ y 2)))
             (4 (list (- x) (/ y 2)))
             (5 (list 0 y))))))

(defmethod om::draw-obj-in-rect ((self tonnetz) x1 x2 y1 y2 params view)
  ;; Drawing the hexagons
  (let ((editor (om::editorframe (om::object (om-view-container view)))))
    (if editor
        (let* ((i (cchord editor))
               (stiles (remove-if (lambda (ti) (not (find i (level ti)))) (tiles (main-view editor))))
               (chord (mapcar #'note stiles))
               (pchord (permute chord))
               (paths (mapcar (lambda (ch) (computePath (gen-funs (om::object editor)) ch)) pchord))
               (path (most-compact paths))
               (tcoords (mapcar (lambda (ti) (if (listp ti)
                                                 (reduce #'om-add-points (mapcar #'gen-to-pos ti))
                                               (gen-to-pos ti))) path))
               (coords (scanl #'om-add-points (om-make-point (/ x2 2) (/ y2 2)) tcoords)))
          (print coords)
          (om-with-focused-view view
            (loop for point in coords do
                  (let ((xc (om-point-x point))
                        (yc (om-point-y point)))
                    (om-draw-ellipse xc yc 19 19)
                    (om-with-fg-color view (om-make-color 0.7 0.15 0.05)
                      (om-fill-ellipse xc yc 19 19)
                      ))))))))


