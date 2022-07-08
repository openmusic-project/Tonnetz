(in-package :om)

(defclass! tonnetz ()
  ((gens :accessor gens :initform 'tstandardinv :initarg :gens)
   (chords :accessor chords :initform '((0 4 7)) :initarg :chords))
  (:icon 3470)
  (:documentation "Represents the diagram of a triadic Tone Network (Tonnetz) with three generators as well as a chord-list (here, a list of lists of pitches) as arguments."))

; The generators available as pre-defined parameters of the Tone Network.

(defparameter *tonnetz-generators* 
  '(("Standard (K[3, 4, 5])" tstandardinv) 
    ("K[1, 1, 10]" tk1110inv)
    ("K[1, 2, 9]" tk129inv)
    ("K[1, 3, 8]" tk138inv)
    ("K[1, 4, 7]" tk147inv)
    ("K[1, 5, 6]" tk156inv)
    ("K[2, 3, 7]" tk237inv)
    ("K[2, 5, 5]" tk255inv)))

; Functions to implement basic operability of the Tone Network as an OM class.

(defmethod class-has-editor-p ((self tonnetz)) t)
(defmethod get-editor-class ((self tonnetz)) 'tonnetz-editor)
(defmethod get-type-of-ed-box ((self tonnetz)) 'tonnetz-box)

(defclass tonnetz-box (omboxeditcall)
  ((paths :accessor paths :initform '())))

(defmethod get-slot-names ((self tonnetz))
  (values ; names/accessors
          (list "self" "gens" "chords")
          ; default values
          (list nil 'tstandardinv '((0 4 7)))
          ; doc
          (list "Object" "Generators" "Chords")
          ; menus
          (list nil 
                (list (list 1 *tonnetz-generators*))
                nil)))

; The Tone Network's editor.

(defclass tonnetz-editor (EditorView)
  ((main-view :accessor main-view :initform nil :initarg :main-view)
   (controls-view :accessor controls-view :initform nil :initarg :controls-view)
   (current-chord :accessor currentchord :initform 0)
   (auto-play :accessor autoplay :initform nil)
   (cyclic :accessor cyclic :initform nil)
   (played-chord :accessor playedchord :initform nil)))

(defmethod get-box ((self tonnetz-editor))
  "Returns a reference to the editor's box"
  (ref self))

(defclass tonnetz-control (om-view)
  ((chord-text-view :accessor chord-text-view :initform nil :initarg :chord-text-view)
   (columns-edit :accessor columns-edit)
   (lines-edit :accessor lines-edit)))

(defmethod initialize-instance ((self tonnetz) &rest args)
  (call-next-method)
  (if (symbolp (gens self))
      (setf (gens self) (eval (gens self))))
  (when (chords self) (setf (chords self) (chords self)))
  )

(defun from-path (path cols lines)
  "Computes the position in the grid from a Tonnetz path"
  (flet ((tmp (v)
             (case v
               (0 (+ 1 (- lines)))
               (1 (+ lines))
               (2 1)
               (3 (- lines 1))
               (4 (- lines))
               (5 -1))))
    (reduce #'+ (mapcar #'tmp path))))

(defun frompath (path cols lines)
  (mapcar (lambda (x) (from-path x cols lines)) path))

(defmethod refresh-main-view ((self tonnetz-editor) cols lines size gens tile-size)
  "Resets the main view."
  (om-remove-subviews self (main-view self))
  (setf (main-view self)
          (om-make-view 'tonnetz-view 
                        :position (om-make-point 20 20)
                        :columns cols
                        :lines lines
                        :size size
                        :gens gens
                        :tile-size tile-size))
    (om-add-subviews self (main-view self))
    (light-notes self)
    (report-modifications self))

(defmethod initialize-instance ((self tonnetz-editor) &rest args)
  "Initializes the tonnetz-editor window."
  (call-next-method)
  (om-set-bg-color self (om-make-color 0.4 0.4 0.4))
  (setf (main-view self) 
        (om-make-view 'tonnetz-view 
                      :position (om-make-point 20 20)
                      :size (om-make-point 600 600)
                      :gens (gens (object self))))
  (setf (controls-view self) 
        (om-make-view 'tonnetz-control
                      :position (om-make-point (- (w self) 100) 20)
                      :size (om-make-point 80 (- (h self) 40))
                      :bg-color :transparent))

  (om-add-subviews (controls-view self)
                   ;Displays the number of columns.
                   (setf (columns-edit (controls-view self))
                         (om-make-dialog-item 'edit-numbox
                                              (om-make-point 0 0)
                                              (om-make-point 40 20)
                                              (format nil " ~D" (columns (main-view self)))
                                              :fg-color *om-white-color*
                                              :value (columns (main-view self))
                                              ; The following "shadowed" lines cause the selector to fail.
                                              ;:di-action #'(lambda (item)
                                              ;               (set-value item (round (parse-integer (om-dialog-item-text item)) 2)))
                                              :afterfun #'(lambda (item)
                                                            (if (cyclic self)
                                                                (refresh-main-view self 
                                                                                   (* 2 (round (parse-integer (om-dialog-item-text item)) 2))
                                                                                   (lines (main-view self))
                                                                                   (om-view-size (main-view self))
                                                                                   (gens (main-view self))
                                                                                   (tile-size (main-view self)))
                                                              (update-subviews self)))))
                   ; Displays the number of lines.
                   (setf (lines-edit (controls-view self))
                         (om-make-dialog-item 'edit-numbox
                                              (om-make-point 0 25)
                                              (om-make-point 40 20)
                                              (format nil " ~D" (lines (main-view self)))
                                              :value (lines (main-view self))
                                              :fg-color *om-white-color*
                                              :afterfun #'(lambda (item)
                                                            (if (cyclic self)
                                                                (refresh-main-view self 
                                                                                   (columns (main-view self)) 
                                                                                   (* 2 (round (parse-integer (om-dialog-item-text item)) 2))
                                                                                   (om-view-size (main-view self))
                                                                                   (gens (main-view self))
                                                                                   (tile-size (main-view self)))
                                                              (update-subviews self)))))
                    ; Indicates the length of the window (number of chords activated.)
                    (setf (chord-text-view (controls-view self))
                          (om-make-dialog-item 'om-text-view
                                               (om-make-point 10 55)
                                               (om-make-point 120 20)
                                               (chord-text self)
                                               :fg-color #+cocoa *om-white-color* #+(or linux win32) *om-black-color*
                                               :bg-color #+cocoa :transparent #+(or linux win32) *om-white-color*
                                               ))
                    ; Lists the available generators when clicked on, toggles and displays the Tone Network according to the current ones.
                    (om-make-dialog-item 'om-pop-up-dialog-item
                                               (om-make-point 10 85)
                                               (om-make-point 120 20)
                                               ""
                                               :range (mapcar 'car *tonnetz-generators*)
                                               :value NIL ; Q: HOW TO FIND THE VALUE FROM THE TONNETZ?
                                               :di-action #'(lambda (item)
                                                              (setf (gens (object self))
                                                                    (eval (cadr (nth (om-get-selected-item-index item) *tonnetz-generators*))))
                                                              (refresh-main-view self 
                                                                          (columns (main-view self))
                                                                          (lines (main-view self))
                                                                          (om-view-size (main-view self))
                                                                          (gens (object self))
                                                                          (tile-size (main-view self)))
                                                              ))
                    ; Toggles auto-play.
                    (om-make-dialog-item 'oa::om-check-box
                                         (om-make-point 10 165)
                                         (om-make-point 120 40)
                                         "Auto-play"
                                         :fg-color *om-light-gray-color*
                                         :value (autoplay self) 
                                         :di-action #'(lambda (item)
                                                        (setf (autoplay self)
                                                              (om-checked-p item)))
                                         )
                    (om-make-dialog-item 'oa::om-check-box
                                         (om-make-point 10 205)
                                         (om-make-point 120 40)
                                         "Cyclic" ; otherwise, Simile-infinite.
                                         :fg-color *om-light-gray-color*
                                         :value (cyclic self) 
                                         :di-action #'(lambda (item)
                                                        (setf (cyclic self)
                                                              (om-checked-p item)))
                                         )
                    (om-make-dialog-item 'om-text-view
                                         (om-make-point 10 125)
                                         (om-make-point 20 20)
                                         "#"
                                         :fg-color *om-light-gray-color*
                                         :bg-color :transparent
                                         )
                    ; Allows the user to change the number of displayed chords — the trace.
                    (om-make-dialog-item 'edit-numbox
                                   (om-make-point 30 122)
                                   (om-make-point 40 20) 
                                   (format nil " ~D" (max-level (main-view self)))
                                   :bg-color *om-black-color*
                                   :fg-color *om-white-color*
                                   :value (max-level (main-view self))
                                   :min-val 1 :max-val 100
                                   :afterfun #'(lambda (item)
                                                 (setf (max-level (main-view self)) (value item))
                                                 (om-set-dialog-item-text (chord-text-view (controls-view self)) (chord-text self))
                                                 ; Changes every remaining chord's hue, as it applies the same gradient to a different number of chords.
                                                 (erase-notes self)
                                                 (light-notes self)
                                                 (om-invalidate-view (main-view self)))
  ))
  (om-add-subviews self (main-view self) (controls-view self))
  ; Highlights the notes from the chords if there are any.
  (light-notes self)
  (om-invalidate-view (main-view self)))

(defun tiles-num (cols rows)
  "Returns the number of tiles, given the number of columns and lines."
  (- (* cols rows) (floor (/ cols 2))))

(defmethod draw-light ((self tonnetz-editor) poslist)
  "Toggles the state of each tile visible in the main view."
  (loop for k in poslist do
        (let ((i (cadr k))
              (l (car k)))
          (loop for x in l do
                (push i (level (nth x (tiles (main-view self)))))
                (setf (color (nth x (tiles (main-view self)))) (om-make-color
                                                                              (+ 0.3125 (/ (+ 0.25 i) (max-level (main-view self))))
                                                                              (+ 0.3125 (/ (+ 0.25 i) (max-level (main-view self)))) 
                                                                              0.3125))))))

(defmethod light-notes ((self tonnetz-editor))
  "Highlights the selected notes, given that the Tone Network has been correctly initialized."
  (let* ((tiles (tiles (main-view self)))
         (tchs (last (chords (object self)) (max-level (main-view self))))
         (pchs (mapcar #'netz-permute tchs))
         (gs (gens (main-view self)))
         (tpaths (mapcar (lambda (tmp) (mapcar (lambda (c) (list c (netz-computepath gs c))) tmp)) pchs)) ; Paths associated to a given chord
         (chordnpaths (mapcar #'most-compact tpaths))
         (paths (mapcar #'cadr chordnpaths))
         (chs (mapcar #'car chordnpaths))
         (cols (columns (main-view self)))
         (lines (lines (main-view self))))
    ;(setf (max-level (main-view self)) (length paths))
    (if chs
        (progn 
          (draw-light self (print (compute-paths self paths cols lines chs gs)))
      '()))))

(defmethod compute-paths ((self tonnetz-editor) paths cols lines chs gs)
  (let ((index (position 0 (mapcar #'note (tiles (main-view self))) :start (* 6 lines))) ; We have to search for a tile with a 0 note (C). The best would be to look for such a tile, with the requirement that the path fits into the tonnetz.
        (tmpl '())
        (bestlastnote 0)
        (poslist '()))
    (setf (paths (main-view self)) paths)
    (loop for c in paths
          for i in (iota (min (max-level (main-view self)) (length paths))) do
          (if (/= i 0)
              (setf bestlastnote (car (min-ch 
                                       (mapcar (lambda (note) (list note (cadr (distance-note2chord note (nth i chs) gs)))) (nth (- i 1) chs)) 
                                       #'cadr))
                    index (nth (position bestlastnote (nth (- i 1) chs)) tmpl)))
          (let* ((onenote (from-path (compute-path gs (netz-interval bestlastnote (car (nth i chs)))) cols lines))
                 (l (scanl-minus #'+ (cons (setf index (+ onenote index)) (frompath c cols lines)))))
            (push (list l i) poslist)
            (setf tmpl l)
            ))
    poslist))

; Bound to the button action. To be removed, maybe. But necessary for tiles and stuff.

(defmethod generate-chords ((self tonnetz-editor))
  (let ((ton (object self))
        (tiles (tiles (main-view self))))
    (setf (chords ton)
          (remove-if #'null
                     (loop for x in (iota (max-level (main-view self))) collect
                           (loop for n in (remove-if (lambda (l) (not (member x (level l)))) tiles) collect
                                 (note n)))))
    ; The following "shadowed" lines might be wanted or not. And, maybe, even add a boolean?
    ;(erase-notes self)
    ;(light-notes self)
    ; Invalidates the editor and reports the modification to the box.
    (update-panel self t)))

(defmethod erase-notes ((self tonnetz-editor))
  "Erases all notes from the editor's main view."
  (mapcar (lambda (el) 
            (progn
              (setf (level el) '())
              (setf (color el) (om-make-color 1 1 1)))) (tiles (main-view self))))

(defmethod chord-text ((self tonnetz-editor))
  "Displays, in a string, the index of the currently selected chord."
  (concatenate 'string "Chord: " (write-to-string (+ 1 (currentchord self))) "/" (write-to-string (max-level (main-view self)))))

(defmethod netz-transpose-chords ((self tonnetz-editor) n-gen)
  "Transposes the set of chords given a generator (by its index in a list of generators.)"
  (let ((trans (nth n-gen (gens (object self)))))
    (setf (chords (object self))
          (mapcar (lambda (l) (mapcar (lambda (el) (funcall trans el)) l)) (chords (object self)))))
  (erase-notes self)
  (light-notes self)
  (update-subviews self))

(defun netz-convert2mc (tchord)
  "Converts a list of pitches (from a Tone Network) into a list of midicents."
  (mapcar (lambda (note) (* 100 (+ (* 12 5) note))) tchord))

(defmethod netz-play-chord ((self tonnetz-editor))
  "Plays the currently selected chord."
  (let* ((onechord (nth (currentchord self) (chords (object self))))
          (chord (netz-convert2mc onechord)))
    (setf c (make-instance 'chord :lmidic chord))
    (play c)
    ))

(defmethod netz-modify-current ((self tonnetz-editor) n)
  "Modifies the currently selected chord and plays it if selected by the user."
  (setf (currentchord self) n)
  (om-set-dialog-item-text (chord-text-view (controls-view self)) (chord-text self))
  (when (autoplay self)
    (netz-play-chord self))
  (update-subviews self))

; Defines the key commands available.

(defmethod handle-key-event ((self tonnetz-editor) key)
  (case key 
    (:om-key-right ; Toggles the next chord, and plays it if "Auto-play" is enabled.
         (netz-modify-current self (mod (+ 1 (currentchord self)) (max-level (main-view self)))))
    (:om-key-left ; Toggles the first chord, and plays it if "Auto-play" is enabled.
         (netz-modify-current self 0))    
    (#\Space ; Plays the current chord.
         (netz-play-chord self))
    (:om-key-esc ; Clears the Tone Network's main view.
         (erase-notes self))
    (#\- ; Decreases the Zoom Level (the tiles' size.)
         (resize-tiles self 0.875))
    (#\+ ; Increases the Zoom Level (the tiles' size.)
         (resize-tiles self 1.125))
    (#\t ; Toggles the opening chord, and plays it if "Auto-play" is enabled — identical to "left-arrow" key command, only here to accompany the "p" key command. ;;marche pas!
         (netz-modify-current self 0))    
    (#\p ; Plays the current chord and toggles the next — only use it if "Auto-play" is disabled, otherwise use the "space" and "right-arrow" key commands.
         (netz-play-chord self)
         (netz-modify-current self (mod (+ 1 (currentchord self)) (max-level (main-view self)))))
   ; (#\z (print "Zzzz"))

    (#\h (show-help-window (format nil "Commands for ~A Editor" 
                                          (string-upcase (class-name (class-of (object (editor self)))))) 
                           (get-help-list self)))

    ; Transposition key commands.
    
    (#\0 
     (netz-transpose-chords self 0))
    (#\1
     (netz-transpose-chords self 1))
    (#\2
     (netz-transpose-chords self 2))
    (#\3
     (netz-transpose-chords self 3))
    (#\4 
     (netz-transpose-chords self 4))
    (#\5
     (netz-transpose-chords self 5))

    (otherwise (print nil)))
  (om-invalidate-view self)
  )


(defmethod get-help-list ((self tonnetz-editor)) 
  (list '(("lr" "Toggles next chord")
          (("-") "Decrease zoom")
          (("+") "Increase zoom")
          ("space" "Plays current chord")
          ("esc" "Clears Network's main view")
          ; (("t") "Toggles chord and plays it if Auto-play is enabled")
          )
        '((("0") "Transpose chords by 0")
          (("1") "Transpose chords by 1")
          (("2") "Transpose chords by 2")
          (("3") "Transpose chords by 3")
          (("4") "Transpose chords by 4")
          (("5") "Transpose chords by 5")
          )))


; Redraws a Tone Network with resized tiles by a factor of one's choice. 

(defmethod resize-tiles ((self tonnetz-editor) factor)
  "Resizes the tiles by a given factor."
  (let ((tsize (* factor (tile-size (main-view self))))
        (viewsize (om-view-size (main-view self))))
    (om-remove-subviews self (main-view self))
    (setf (main-view self)
          (om-make-view 'tonnetz-view :position (om-make-point 20 20)
                        :size viewsize
                        :gens (gens (object self))
                        :tile-size tsize))
    (om-add-subviews self (main-view self))
    (light-notes self)))

(defmethod update-editor-after-eval ((self tonnetz-editor) val)
  ; After evaluation, completely redraws the Tone Network.
  (call-next-method)
  (erase-notes self)
  (light-notes self))

(defmethod update-subviews ((self tonnetz-editor))
  ; Called when resizing.
  (call-next-method)
  (om-set-view-size (main-view self) (om-subtract-points (om-view-size self) (om-make-point 200 40)))
  (om-set-view-position (controls-view self) (om-make-point (- (w self) 160) 20))
  (om-set-view-size (controls-view self) (om-make-point 140 (- (h self) 40)))
  ; Redraws the main view.
  (refresh-main-view self 
                     (columns (main-view self))
                     (lines (main-view self))
                     (om-view-size (main-view self))
                     (gens (object self))
                     (max (/ (- (w self) 160) (columns (main-view self))) (/ (h self) (lines (main-view self))))))

; Functions to draw the box.

(defun gen-to-vect (g)
  "Associates a direction (a two-dimmensional vector) to each generator."
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

(defmethod draw-obj-in-rect ((self tonnetz) x1 x2 y1 y2 params view)
  ; Draws the hexagons.
  (let ((editor (editorframe (object (om-view-container view)))))
    (if editor
        (let* ((i (currentchord editor))
               (stiles (remove-if (lambda (ti) (not (find i (level ti)))) (tiles (main-view editor))))
               (chord (mapcar #'note stiles))
               (pchord (netz-permute chord))
               (paths (mapcar (lambda (ch) (netz-computepath (gens (object editor)) ch)) pchord))
               (path (mostcompact paths))
               (tcoords (mapcar (lambda (ti) (if (listp ti)
                                                 (reduce #'om-add-points (mapcar #'gen-to-vect ti))
                                               (gen-to-vect ti))) path))
               (coords (scanl #'om-add-points (om-make-point (/ x2 2) (/ y2 2)) tcoords)))
          ;; (print coords)
          (om-with-focused-view view
            (loop for point in coords do
                  (let ((xc (om-point-x point))
                        (yc (om-point-y point)))
                    (om-draw-ellipse xc yc 19 19)
                    (om-with-fg-color view (om-make-color 0.875 0.875 0)
                      (om-fill-ellipse xc yc 19 19)
                      ))))))))


; Toolbox.

; Converts a Chord-Seq into a Tone Network.

(defmethod! cseq2netz ((cs chord-seq))
  :doc "Converts a Chord-Seq into a Tone Network."
  :indoc '("Chord-Seq")
  :outdoc '("The Tone Network representing the Chord-Seq's chords.")
  :icon 3470
  (make-instance 'tonnetz :chords (mapcar (lambda (l) (mapcar (lambda (el) (mod (/ el 100) 12)) l)) (lmidic cs))))

; Converts a Tone Network into a Chord-Seq.

(defmethod! netz2cseq ((ton tonnetz))
  :doc "Converts a Tone Network into a Chord-Seq."
  :indoc '("Tone Network")
  :outdoc '("The Chord-Seq representing the Tone Network's chords.")
  :icon 3470
  (make-instance 'chord-seq :lmidic (mapcar (lambda (chr) (mapcar (lambda (note) (* 100 (+ (* 12 5) note))) chr)) (chords ton))))

; Transposes a Tone Network given a direction defined by one of its generators.

(defmethod! transpose-tonnetz ((ton tonnetz) n-gen)
   :doc "Transposes a Tone Network's path following the axis defined by one of its generators."
   :indoc '("Tone Network" "Integer : index of one of its generators.")
   :outdoc '("The transposed Tone Network.")
   :icon 3470
   :initvals '(nil 0)
   :menuins '((1 (("1" 0) ("2" 1) ("3" 2) ("4" 3) ("5" 4) ("6" 5))))
   (make-instance 'tonnetz :gens (gens ton) :chords (tonnetz-transpose-chords (chords ton) (gens ton) n-gen)))

; Creates a list of generators. (Not Working! And No Real Need for It!)

(defun make-generators (n1 n2 n3)
  "Creates a list of generators given three modulo 12 natural numbers (representing intervals)."
  (list (netz-interval 0 n1) (netz-interval 0 n2) (netz-interval 0 n3)))



(defmethod omNG-save ((self tonnetz) &optional (values? nil)) (print "ME SAVING")
  (let ((theclass (class-name (class-of self))))
  `(make-instance ',theclass
     :gens 'tstandardinv
     :chords ',(chords self)
     )))