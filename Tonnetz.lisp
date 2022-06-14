(in-package :om)

;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

(defvar *tonnetz-files* nil)
(setf *tonnetz-files* (list
                       (om::compile&load (om::om-relative-path '("sources") "tonnetz-editor"))
                       (om::compile&load (om::om-relative-path '("sources") "tonnetz-tiles"))
                       (om::compile&load (om::om-relative-path '("sources") "tonnetz-view"))
                       (om::compile&load (om::om-relative-path '("sources") "tonnetz-utils"))
))

;Loading files 
;--------------------------------------------------
(mapc #'compile&load *tonnetz-files*)
;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------

(defvar *subpackages-karim-var* nil)
(setf *subpackages-Tonnetz-var*
      '(
        ("Tonnetz" nil nil (cseq2netz 
                            netz2cseq
                            transpose-tonnetz
                            ) nil)
        ("Utils" nil nil (netz-transpose 
                          netz-inverse 
                          netz-compose 
                          netz-interval 
                          netz-computepath 
                          netz-computemelody 
                          scanl 
                          scanl-minus 
                          tonnetz-transpose-chords 
                          netz-convert2mc
                          ) nil)
        (nil nil (tonnetz))
        ))
      

;--------------------------------------------------
;filling packages
;--------------------------------------------------

(om::fill-library *subpackages-Tonnetz-var*)

(om-print "
;;;============================================================
;;
;;
;;          $Revision: 1.12 $
;;     $Date: 2006/04/24 21:33:29 $
;;; (c) Ircam -  - 2022
;;;============================================================
")

;;; (gen-lib-reference (find-library "Tonnetz"))


;(setf *tonnetz-lib* (om::find-library "Tonnetz"))


