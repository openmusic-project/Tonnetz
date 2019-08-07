(in-package :om)

; Utilities

; Defines a few functions which make us able to work on Tonnetze
; Transpositions are here represented by functions rather than numbers,
; so that we keep the algebraic group structure. These functions act on
; the set of notes, which are simply numbers (from 0 to 11).
; paths in the Tonnetz are lists of positive integers, representing the
; generators (to model a path, we take the position of the generator in 
; the list of generators, and use it).

(defun netz-add-mod12 (a b)
;  "Modulo 12 addition, represents the addition in Z/12Z"
  (mod (+ a b) 12))

(defun iota (n &optional (startpoint 0))
;  "Builds a list of n elements beginning at startpoint."
  (labels ((tmpi (n sa acc)
             (if (<= n 0) acc (funcall #'tmpi (- n 1) (+ sa 1) (append acc (list sa))))))
    (funcall #'tmpi n startpoint nil)
    ))

(defun netz-transpose (a)
;  "Creates a function which transposes a given pitch."
  (lambda (b) (mod (+ a b) 12)))

(defun netz-inverse (i)
;  "Creates a function of the inverse of a given interval."
  (netz-transpose (- 12 (funcall i 0))))

; The null element of the transposition group.

(defvar el-null (netz-transpose 0))

; Generators of the standard riemannian Tone Network (Tonnetz): the minor and major thirds, and the perfect fifth.

(defvar tstandard (mapcar #'netz-transpose '(3 4 7)))
(defvar tstandardinv (append tstandard (mapcar #'netz-inverse tstandard)))

; Generators of non-riemannian, triadic Tone Networks (Tonnetze).

(defvar tk1110 (mapcar #'netz-transpose '(1 1 2)))
(defvar tk1110inv (append tk1110 (mapcar #'netz-inverse tk1110)))
(defvar tk129 (mapcar #'netz-transpose '(1 2 3)))
(defvar tk129inv (append tk129 (mapcar #'netz-inverse tk129)))
(defvar tk138 (mapcar #'netz-transpose '(1 3 4)))
(defvar tk138inv (append tk138 (mapcar #'netz-inverse tk138)))
(defvar tk147 (mapcar #'netz-transpose '(1 4 5)))
(defvar tk147inv (append tk147 (mapcar #'netz-inverse tk147)))
(defvar tk156 (mapcar #'netz-transpose '(1 5 6)))
(defvar tk156inv (append tk156 (mapcar #'netz-inverse tk156)))
(defvar tk237 (mapcar #'netz-transpose '(2 3 5)))
(defvar tk237inv (append tk237 (mapcar #'netz-inverse tk237)))
(defvar tk255 (mapcar #'netz-transpose '(2 5 7)))
(defvar tk255inv (append tk255 (mapcar #'netz-inverse tk255)))

(defun netz-compose (a b)
;  "Represents the operation of the transposition group."
  (lambda (x) (funcall a (funcall b x))))

(defun netz-interval (a b)
;  "Computes a transposition of the distance between two given pitches."
  (netz-transpose (- b a)))

(defun netz-equal (f1 f2)
;  "Computes a boolean of the predicate that both given transpositions yield the same values, i.e. are equal."
  (equal (funcall f1 0) (funcall f2 0)))

(defun netz-traverse (is ins)
;  "Computes a transposition, given the generators and a path."
  (reduce #'netz-compose (mapcar (lambda (n) (nth n is)) ins) :initial-value el-null))

(defun tmpcompute (is i p)
  (let* ((ni (length is))
         (v (apply #'append (mapcar (lambda (x) (mapcar (lambda (b) (cons x b)) p)) (iota ni))))
         (l (remove-if (lambda (y) (not (netz-equal i (netz-traverse is y)))) v)))
    (if (netz-equal i (netz-transpose 0))
        '()
        (if (null l)
            (tmpcompute is i v)
          l))))

(defun netz-pairs (l)
  (if (or (null (cdr l)) (null l))
      '()
    (cons (list (car l) (cadr l)) (netz-pairs (cdr l)))))

(defun compute-path (is i)
    (reverse (car (tmpcompute is i (list '())))))

(defun netz-computepath (is ps)
;  "Computes a path in a Tone Network, given the generators and a melody."
  (let ((ins (mapcar (lambda (l) (netz-interval (car l) (cadr l))) (netz-pairs ps))))
    (mapcar (lambda (x) (compute-path is x)) ins)))

(defun scanl (f q ls)
;  "Similar to a reduce, but successively folds over lists of increasing length."
  (if (null ls)
      (list q)
    (cons q (scanl f (funcall f q (car ls)) (cdr ls)))))

(defun scanl-minus (f ls)
;  "scanl without the start argument."
  (if (null ls)
      '()
    (scanl f (car ls) (cdr ls))))

(defun netz-computemelody (gens start paths)
;  "Computes a melody, given the generators, the starting note, and a list of paths."
   (cons start (mapcar (lambda (i) (funcall i start)) (scanl-minus #'netz-compose (mapcar (lambda (path) (netz-traverse gens path)) paths)))))

(defun netz-p2n (n)
;  "Maps a pitch onto a note — important for the UI."
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

; Tonnetz Transformations

; Transposition

(defun tonnetz-transpose-chords (chords gens n-gen)
;  "Transposes a list of chords along an axis, given by the index of the generator."
  (mapcar (lambda (l) (mapcar (lambda (el) (funcall (nth n-gen gens) el)) l)) chords))

(defun tonnetz-rotate-pos (n-gen)
  (case n-gen
    (0 1)
    (1 3)
    (2 5)
    (3 2)
    (4 0)
    (5 4)))

(defun tonnetz-rotate-pos-inverse (n-gen)
  (case n-gen
    (1 0)
    (3 1)
    (5 2)
    (2 3)
    (0 4)
    (4 5)))

; Not finished.
(defun tonnetz-rotate-chords (chords gens angle)
  "Rotation in a Tonnetz, angle is -1 or +1."
  (let* ((frotate (if (< angle 0) #'tonnetz-rotate-pos-inverse #'tonnetz-rotate-pos))
         (chs-gens (funcall #'frotate (rmapcar (lambda (i) (position i gens :test netz-equal)) (mapcar (lambda (chr) (netz-computepath gens chr)) chords))))
         (ins (mapcar (lambda (p) (netz-interval (car (last (car p))) (cadr p))) (netz-pairs chords))) ; Distances between chords
         (fstchr (mapcar (lambda (note) (funcall (nth note gens) (caar chords))) chs-gens))
         (result (list fstchr)))
    (loop for c in (cdr chs-gens)
          for i in ins
          
          )
  result
  ))

(defun netz-permute (list)
;  "Permutation function from Rosetta code. Returns all the permutations of a given list."
  (if list
    (mapcan #'(lambda (x)
    (mapcar #'(lambda (y) (cons x y))
      (netz-permute (remove x list))))
      list)
    '(())))

(defun min-ch (box access)
  (labels ((tmpmin-ch (box acc)
             (if (null box)
                 acc
               (tmpmin-ch (cdr box) (if (< (funcall access (car box)) (funcall access acc))
                                                  (car box)
                                                acc)))))
    (tmpmin-ch box (car box))))
      
(defun mostcompact (box)
;  "Selects the most compact chord in a list of chords. The algorithm selects the list for which the sum of the length of the sublists is lower."
  (car (min-ch
        (mapcar (lambda (el) (list el (reduce #'+ (mapcar (lambda (e) (if (listp e) (- (length e) 1) 0)) el)))) box)
        #'cadr)
       ))

(defun most-compact (box)
  (car (min-ch
        (mapcar (lambda (el) (list el (reduce #'+ (mapcar (lambda (e) (if (listp e) (- (length e) 1) 0)) (cadr el))))) box)
        #'cadr)))

(defun distance-note2chord (note chord gens)
;  "Returns the distance between a chord and a note, and the note of the chord for which the distance is minimal"
  (min-ch 
   (mapcar (lambda (el)
             (list (car el) (length (cadr el))))
           (mapcar (lambda (nt) (list nt (if (= note nt) '() (car (netz-computepath gens (list nt note)))))) chord))
   #'cadr))
