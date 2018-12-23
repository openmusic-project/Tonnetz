(defpackage "Tonnetz"
    (:nicknames :tnetz)
    (:use "COMMON-LISP" "CL-USER" "OM-API"  "LISPWORKS" "HCL" "OM-LISP" "OPENGL")
    (:import-from "CL-USER")
    (:export :tonnetz :addm12 :compose :interval :eq2 :traversal :pairs :computeMelody :computePath :stringNote :iota :tstandard :nol :transpose :acc-tonnetz :scanl :scanl1 :tstandardi :ttoneleadi :tonnetz2cs :chordseq2t :tonnetz-view :note-view)
)

(om::compile&load (om::om-relative-path '("src") "tonnetz-utils")) ;; Engine
(om::compile&load (om::om-relative-path '("src") "tonnetz")) ;; main object
(om::compile&load (om::om-relative-path '("src") "tonnetz-editor")) ;; Main file


(setf *tonnetz-lib* (om::find-library "Tonnetz"))

(defvar *Utils* (om::omng-make-new-package "Utils"))
(defvar *Tonnetz* (om::omng-make-new-package "Tonnetz"))

(om::AddPackage2pack *Utils* *tonnetz-lib*)
(om::AddPackage2pack *Tonnetz* *tonnetz-lib*)

(om::AddLispFun2Pack '(transpose inverse compose interval computePath computeMelody scanl scanl1 transpose-chords transchord) *Utils*)
(om::AddLispFun2Pack '(make-generators chordseq2t tonnetz2cs tonnetz-transpose) *Tonnetz*)
(om::addclass2pack '(tnetz:tonnetz) *Tonnetz*)