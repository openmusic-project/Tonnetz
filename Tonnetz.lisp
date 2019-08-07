(in-package :om)

(om::compile&load (om::om-relative-path '("sources") "tonnetz-editor"))
(om::compile&load (om::om-relative-path '("sources") "tonnetz-tiles"))
(om::compile&load (om::om-relative-path '("sources") "tonnetz-view"))
(om::compile&load (om::om-relative-path '("sources") "tonnetz-utils"))

(setf *tonnetz-lib* (om::find-library "Tonnetz"))

(defvar *Utils* (om::omng-make-new-package "Utils"))
(defvar *Tonnetz* (om::omng-make-new-package "Tonnetz"))

(om::AddPackage2pack *Utils* *tonnetz-lib*)
(om::AddPackage2pack *Tonnetz* *tonnetz-lib*)

(om::AddLispFun2Pack '(netz-transpose netz-inverse netz-compose netz-interval netz-computepath netz-computemelody scanl scanl-minus tonnetz-transpose-chords netz-convert2mc) *Utils*)
(om::AddGenFun2Pack '(cseq2netz netz2cseq transpose-tonnetz) *Tonnetz*)
(om::addclass2pack '(tonnetz) *Tonnetz*)