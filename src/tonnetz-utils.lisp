;; Defines a few functions which make us able to work on Tonnetze
;; 

;;;; (defpackage "Tonnetz"
;;;;     (:nicknames :tnetz)
;;;;     (:use "COMMON-LISP" "CL-USER" "OM-API"  "LISPWORKS" "HCL" "OM-LISP" "ACC" "OPENGL")
;;;;     (:import-from "CL-USER")
;;;;     (:export :addm12 :compose :interval :eq2 :traversal :pairs :computeMelody :computePath :stringNote :iota :tstandard :nol :transpose :acc-tonnetz :scanl :scanl1))
(in-package :tnetz)

;; Transpositions are here represented by functions rather than numbers,
;; so that we keep the algebraic group structure. These functions act on
;; the set of notes, which are simply numbers (from 0 to 11).
;; Paths in the Tonnetz are lists of positive integers, representing the
;; generators (to model a path, we take the position of the generator in 
;; the list of generators, and use it).

(defun addm12 (a b)
  "Modulo 12 addition, represents the addition on Z/12Z"
  (mod (+ a b) 12))

(defun iota (n &optional (start-at 0))
  "Builds a list of n elements starting at start-at"
  (labels ((tmpi (n sa acc)
             (if (<= n 0) acc (funcall #'tmpi (- n 1) (+ sa 1) (append acc (list sa))))))
    (funcall #'tmpi n start-at nil)
    ))

(defun compose-fun (f fa)
  "The operator on the group of transpositions"
  (lambda (a) (funcall f (funcall fa a))))

(defun interval (a b)
  "Computes a transposition from the distance between two pitches"
  (transpose-fun (- b a)))

(defun eq2 (f1 f2)
  "Predicate equal to true if the transpositions yield the same values, i.e. if they're equal."
  (equal (funcall f1 0) (funcall f2 0)))

;; 
(defun traversal (is ins)
  "Computes a transposition given the generators and a path"
  (reduce #'compose-fun (mapcar (lambda (n) (nth n is)) ins) :initial-value nol))

(defun tmpcompute (is i p)
  (let* ((ni (length is))
         (v (apply #'append (mapcar (lambda (x) (mapcar (lambda (b) (cons x b)) p)) (iota ni))))
         (l (remove-if (lambda (y) (not (eq2 i (traversal is y)))) v)))
    (if (eq2 i (transpose-fun 0))
        '()
        (if (null l)
            (tmpcompute is i v)
          l))))

(defun pairs (l)
  (if (or (null (cdr l)) (null l))
      '()
    (cons (list (car l) (cadr l)) (pairs (cdr l)))))

(defun computePathb (is i)
    (reverse (car (tmpcompute is i (list '())))))

(defun computePath (is ps)
  "Computes a path in a tonnetz, given the generators and a melody"
  (let ((ins (mapcar (lambda (l) (interval (car l) (cadr l))) (pairs ps))))
    (mapcar (lambda (x) (computePathb is x)) ins)))

;; A useful function
(defun scanl (f q ls)
  "Similar to a reduce, but successively folds over lists of increasing length"
  (if (null ls)
      (list q)
    (cons q (scanl f (funcall f q (car ls)) (cdr ls)))))

(defun scanl1 (f ls)
  "scanl without a starting argument"
  (if (null ls)
      '()
    (scanl f (car ls) (cdr ls))))

(defun computeMelody (gens start paths)
  "Computes a melody, given the generators, the starting note and a list of paths"
   (cons start (mapcar (lambda (i) (funcall i start)) 
                       (scanl1 #'compose-fun (mapcar (lambda (path) (traversal gens path)) paths)))))

(defun stringNote (n)
  "Transforms a note into a string, useful for user interfaces"
  ;(om::number-to-string n)
  (case n
    (0 "C")
    (1 "C#")
    (2 "D")
    (3 "D#")
    (4 "E")
    (5 "F")
    (6 "F#")
    (7 "G")
    (8 "G#")
    (9 "A")
    (10 "A#")
    (11 "B")))

;;;;;; Tonnetz transformations

;; Transposition
(defun transpose-chords (chords gens ngen)
  "Transposes a list of chords on an axis, given by the index of the generator"
  (mapcar (lambda (l) (mapcar (lambda (elt) (funcall (nth ngen gens) elt)) l)) chords))

(defun rotate-pos (ngen)
  (case ngen
    (0 1)
    (1 3)
    (2 5)
    (3 2)
    (4 0)
    (5 4)))

(defun rotate-pos-inverse (ngen)
  (case ngen
    (1 0)
    (3 1)
    (5 2)
    (2 3)
    (0 4)
    (4 5)))

;; Not finished yet
(defun rotate-chords (chords gens angle)
  "Rotation in a Tonnetz, angle is -1 or +1."
  (let* ((frotate (if (< angle 0) #'rotate-pos-inverse #'rotate-pos))
         (chs-gens (funcall #'frotate (rmapcar (lambda (i) (position i gens :test eq2)) (mapcar (lambda (chr) (computePath gens chr)) chords))))
         (ins (mapcar (lambda (p) (interval (car (last (car p))) (cadr p))) (pairs chords))) ;; Distances between chords
         (fstchr (mapcar (lambda (note) (funcall (nth note gens) (caar chords))) chs-gens))
         (finlst (list fstchr)))
    (loop for c in (cdr chs-gens)
          for i in ins
          
          )
  finlst
  ))

(defun permute (list)
  "Permutation function from Rosetta code. Returns all the permutations of a list."
  (if list
    (mapcan #'(lambda (x)
		(mapcar #'(lambda (y) (cons x y))
			(permute (remove x list))))
	    list)
    '(())))

(defun min-ch (lst faccess)
  (labels ((tmpmin-ch (lst acc)
             (if (null lst)
                 acc
               (tmpmin-ch (cdr lst) (if (< (funcall faccess (car lst)) (funcall faccess acc))
                                                  (car lst)
                                                acc)))))
    (tmpmin-ch lst (car lst))))
      
(defun most-compact (lst)
  "Selects the most compact chord in a list of chords
  The algorithm selects the list for which the sum of the length of the sublists is lower"
  (car (min-ch
        (mapcar (lambda (elt) (list elt (reduce #'+ (mapcar (lambda (e) (if (listp e) (- (length e) 1) 0)) elt)))) lst)
        #'cadr)
       ))

(defun most-compact1 (lst)
  "Uses '(chord path) lists as argument to keep track of the chords"
  (car (min-ch
        (mapcar (lambda (elt) (list elt (reduce #'+ (mapcar (lambda (e) (if (listp e) (- (length e) 1) 0)) (cadr elt))))) lst)
        #'cadr)))

(defun chord-distance (gens note chord1)
  "Returns the distance between a chord and a note, and the note of the chord for which the distance is minimal"
  (min-ch 
   (mapcar (lambda (elt)
             (list (car elt) (length (cadr elt))))
           (mapcar (lambda (nt) (list nt (if (= note nt) '() (car (computePath gens (list nt note)))))) chord1))
   #'cadr))

;;;;;;;
;; Related to the bridge between Tonnetze and ACCs

;;;; (defun lines-note (note gens)
;;;;   (let* ((ngens (length gens))
;;;;          (a (iota ngens (+ 12 (* ngens note)))))
;;;;     (mapcar (lambda (e g) (list e (list note (addm12 note g)))) a gens)))

;;;; ;; Generates an acc representing a Tonnetz, given its generators
;;;; (defun acc-tonnetz (generators)
;;;;   (let* ((notes (iota 12))
;;;;          (lines (apply #'append (mapcar (lambda (note) (lines-note note generators)) notes))))
;;;;     (make-complex (append (mapcar #'list notes) lines))))
