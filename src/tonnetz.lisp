(in-package :tnetz)
;(use-package :om)


(om::defclass! tonnetz ()
  ((gens :accessor gens :initform '(3 4 7) :initarg :gens)
   (gen-funs :accessor gen-funs :initform nil)
   (chords :accessor chords :initform nil :initarg :chords))
  (:documentation "Represents a tone network (Tonnetz) which takes three generators as argument as well as a list of chords in a given format: chords is a list of lists of natural integers modulo 12 such as ((5 9 10) (3 4))"))



;; Boilerplate code
(defmethod om::class-has-editor-p ((self tonnetz)) t)
(defmethod om::get-editor-class ((self tonnetz)) 'tonnetz-editor)

;(defmethod om::get-type-of-ed-box ((self tonnetz)) 'TonnetzBox)

;(defclass TonnetzBox (om::OMBoxEditCall)
;  ((paths :accessor paths :initform '())))

;(defmethod om::get-slot-in-out-names ((self tonnetz))
;  (values ;;; names/accessors
;          '("self" "generators" "chords")
;          ;;; default values
;          '(nil (3 4 7) nil)
;          ;;; doc
;          '("tonnetz object" "tonnetz generators" "list of chords")
;          ;;; menus
;          '(nil nil nil)
;          ))


(defun transpose-fun (a)
  "Creates a function which transposes a given pitch in Z/12Z"
  (lambda (b) (mod (+ a b) 12)))

(defun inverse-fun (i)
  "Negates an interval"
  (transpose-fun (- 12 (funcall i 0))))

;; The null element of the group of transpositions
(defvar nol (transpose-fun 0))

;; The generators of the standard Tonnetz: minor and major thirds, perfect fifth
(defvar tstandard (mapcar #'transpose-fun '(3 4 7)))
(defvar tstandardi (append tstandard (mapcar #'inverse-fun tstandard)))
(defvar ttonelead (mapcar #'transpose-fun '(1 2 3)))
(defvar ttoneleadi (append ttonelead (mapcar #'inverse-fun ttonelead)))

;; To be generated automatically
(defparameter *tonnetz-generators* 
  '(("Générateur standard" tstandardi) 
    ("Générateur tonelead" ttoneleadi)))


(defmethod initialize-instance :after ((self tonnetz) &rest args)
  ;; Sometimes, problems occur with the gens argument...
  (ignore-errors 
    (setf (gen-funs self) 
        (cond ((symbolp (gens self))
               (eval (gens self)))
              ((consp (gens self))
               (let ((funs (mapcar #'transpose-fun (gens self))))
                 (append funs
                         (mapcar #'inverse-fun funs))))))
  ;(print (gens self))
  ;(print (gen-funs self))
  self))

;(mapcar #'transpose-fun (consp '(3 4 7))



;;;;;; TOOLBOX FUNCTIONS

;; 
(om::defmethod! chordseq2t ((cs om::chord-seq))
  :doc "Converts a chord-seq into a Tonnetz"
  :indoc '("A chord-seq")
  :outdoc '("The Tonnetz respresenting the chord-seq chords")
  (make-instance 'tonnetz :chords (mapcar (lambda (l) (mapcar (lambda (elt) (mod (/ elt 100) 12)) l)) (om::lmidic cs))))

;; 
(om::defmethod! tonnetz2cs ((tonn tonnetz))
  :doc "Converts a Tonnetz into a chord-seq"
  :indoc '("A Tonnetz")
  :outdoc '("The chord-seq reprensenting the Tonnetz chords")
  (make-instance 'om::chord-seq :lmidic (mapcar (lambda (chr) (mapcar (lambda (note) (* 100 (+ (* 12 5) note))) chr)) (chords tonn))))

;; Transposes a Tonnetz given a direction
(om::defmethod! tonnetz-transpose ((tonn tonnetz) ngen)
   :doc "Transposes the Tonnetz path following an axis"
   :indoc '("A Tonnetz" "An integer : the index of the generator that translates")
   :outdoc '("The transposed Tonnetz")
   :initvals '(nil 0)
   :menuins '((1 (("1" 0) ("2" 1) ("3" 2) ("4" 3) ("5" 4) ("6" 5))))
   (make-instance 'tonnetz :gens (gens tonn) :chords (transpose-chords (chords tonn) (gen-funs tonn) ngen)))

(defun make-generators (n1 n2 n3)
  "Creates a list of generators given integer intervals"
  (list (interval 0 n1) (interval 0 n2) (interval 0 n3)))


