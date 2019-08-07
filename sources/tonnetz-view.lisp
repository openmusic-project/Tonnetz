(in-package :om)

; View

; Classes and functions implementing the drawing of the Tone Network.

; The main view.

(defclass tonnetz-view (om-view)
  ((tiles :accessor tiles :initform nil)
   (paths :accessor paths :initform nil)
   (gens :accessor gens :initarg :gens)
   (notes :accessor notes :initform nil)
   (columns :accessor columns :initform 36 :initarg :columns)
   (lines :accessor lines :initform 24 :initarg :lines)
   (max-level :accessor max-level :initform 4)
   (tile-size :accessor tile-size :initform 36 :initarg :tile-size)))

 ; Draws the tonnetz

(defmethod initialize-instance ((self tonnetz-view) &rest args)
  (call-next-method)
  (om-set-bg-color self (om-make-color 0.75 0.75 0.75))
  (add-tiles self)
  (setf (tiles self) (reverse (tiles self)))
    ;(apply #'om-add-subviews (cons self (tiles self))) ; Too long list
  (mapcar (lambda (el) (om-add-subviews self el)) (tiles self)))

(defmethod add-tiles ((self tonnetz-view))
  "Adds the tiles to the view"
  (let* ((cols (columns self))
         (lines (lines self))
         (width (tile-size self))
         (height (* 1.5 width))
         (shift (/ height 2)))
    ; Initializing the tiles
    (loop for x in (iota cols)
          for n1 in (netz-computemelody (gens self) 0 (mapcar #'list (apply #'append (make-list cols :initial-element '(1 3)))))
          do
          (loop for y in (iota (- lines (if (oddp x) 1 0)))
                for n2 in (netz-computemelody (gens self) n1 (mapcar #'list (make-list cols :initial-element 2)))
                do
                (setf (tiles self) (cons (om-make-view 'note-view
                                                       :position (om-make-point (* x width) (+ (if (oddp x) shift 0) (* y height)))
                                                       :column x
                                                       :row y
                                                       :size (om-make-point width height)
                                                       :bg-color :transparent
                                                       :note n2)
                                         (tiles self)))))))

(defmethod om-draw-contents ((self tonnetz-view))
  (let* ((width (tile-size self))
         (height (* 1.5 (tile-size self)))
         ; Diagonals (from 0,0 to x,y)
         (startx1 0)
         (starty1 (+ 4 (/ height 6)))
         (endx1 (* width (columns self)))
         (endy1 (+ starty1 (* height (/ (columns self) 2))))
         ; Diagonals (from 0,y to x,0)
         (startx2 0)
         (starty2 (+ starty1 (* height (/ (columns self) 2))))
         (endx2 (* width (columns self)))
         (endy2 (+ 4 (/ height 6)))
         )
  (om-with-focused-view self
    (om-with-fg-color self (om-make-color 0.5 0.5 0.5)
      ; Diagonals (from 0,0 to x,y)
      (loop for x in (mapcar (lambda (e) (* 2 e)) (iota (/ (columns self) 2))) do
            (om-draw-line (+ startx1 (* x width)) starty1 (+ (* x width) endx1) endy1))
      (loop for y in (iota (lines self)) do
            (om-draw-line startx1 (+ (* y height) starty1) endx1 (+ (* y height) endy1)))
      ; Diagonals (from 0,y to x,0)
      (loop for x in (mapcar (lambda (e) (- (* 2 e) 1)) (iota (/ (columns self) 2))) do
            (om-draw-line (- startx2 (* x width)) starty2 (- endx2 (* x width)) endy2))
      (loop for y in (iota (lines self)) do
            (om-draw-line startx2 (+ (* y height) (+ (/ height 2) starty2)) endx2 (+ (* y height) (+ (/ height 2) endy2))))
      ; Vertical lines
      (loop for x in (iota (columns self)) do
            (om-draw-line (+ (/ width 2) (* x width)) (+ (if (evenp x) (/ height 2) 0) starty1) (+ (/ width 2) (* x width)) (+ (if (evenp x) (/ height 2) 0) starty1 (* height (lines self)))))
                ))))
