;; NOTES AND INFORMATION:
;; ------------------- ;;
;; 370 group code for pipes:
;;	35: pipe lines
;; 	18: on/off line and valve stop lines
;;	15: minimum distance and instrument lines
;;	9 : line under notes - some instruments lines too? D:
;;	25: equipment lines
;; xData App names:
;; 	CFD1: pipe lines
;; Complex GEVO P&IDs (pg number):
;; - 2, 9, 11, 15, 18, 22, 27, 36 - 40
;; ------------------- ;;

;; General error handler for throwing errors with a given message, msg.
(defun *error* ( msg )
  (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
    (princ (strcat "\nError: " msg))
  )
  (redraw) (princ)
)

; Global variable for the xdata with group code 1071
(setq *xdata-1071* 0)

;; for getting the next 1071 group code
(defun get1071 ( )
  (setq *xdata-1071* (1+ *xdata-1071*))
  (rtos *xdata-1071* 2)
)

; Global variable for the timer
(setq *g-timer-start-time* nil)

; Stores the current time when called, and then returns
; the time difference when called the next time. Time
; difference is returned in milliseconds.
; Usage: (timer)
(defun timer ( / rTimeDiff)
  ; Check to see if the function was previously called
  (if (= *g-timer-start-time* nil)
    (progn
      ; Stores the current milliseconds
      (setq *g-timer-start-time* (getvar "MILLISECS"))
      (princ)
    )
    (progn
      ; Calculates and returns the time difference
      (setq rTimeDiff (- (getvar "MILLISECS") *g-timer-start-time*)
            *g-timer-start-time* nil)
      rTimeDiff
    )
  )
)

;; given a predicate, pred, and a list, lst, filter the list down
;; based on the predicate
(defun filter ( pred lst )
  (cond ((not lst) '())
	(t (if (apply pred (list (car lst)))
	     (cons (car lst) (filter pred (cdr lst)))
	     (filter pred (cdr lst)))))
)

;; applies the function, func, sequentially starting at the right
;; on the list, lst, starting with the base value, base.
(defun foldr ( func base lst )
  (cond ((not lst) base)
	(t (apply func (list (car lst)
			     (foldr func base (cdr lst))))))
)

;; returns a list of information associated with a DXF code
;; arguments:
;;	key - DXF code
;;	elist - ent data list
(defun keyvals (key elist / item)
  (if (setq item (assoc key elist))
    (cons (cdr item) (keyvals key (cdr (member item elist))))
  )
)

;; given a point, returns the closest grid point
(defun gridpt ( pt )
  (mapcar 'roundsixteenth pt)
)

;; round 'n' to the nearest eighth
(defun roundsixteenth ( n )
  (roundm n 0.0625)
)

;; round 'n' to the nearest multiple of 'm'
(defun roundm ( n m )
  (* m (fix ((if (minusp n) - +) (/ n (float m)) 0.5)))
)

;; the list analog of the substr function
(defun sublst ( lst idx len )
  (cond ((null lst) null)
	((< 0 idx) (sublst (cdr lst) (1- idx) len))
	((null len) lst)
	((< 0 len) (cons (car lst) (subst (cdr lst) idx (1- len))))
  )
)

;; returns a list of the duplicate elements from the given list, lst
(defun dupeslst ( lst )
  (if lst
    (if (member (car lst) (cdr lst))
      (cons (car lst) (dupeslst (vl-remove (car lst) (cdr lst))))
      (dupeslst (vl-remove (car lst) (cdr lst)))
    )
  )
)

;; returns a list with duplicate elements in the given list, lst, removed
(defun uniquelst ( lst )
  (if lst (cons (car lst) (uniquelst (vl-remove (car lst) (cdr lst)))))
)

;; returns the list, lst, without any of the elements in the removal list, del
(defun removelst ( del lst )
  (if (and del lst)
    (removelst (cdr del) (vl-remove (car del) lst))
    lst
  )
)

;; removes the item at the nth index in a supplied list, lst
(defun removenth ( n lst / i )
  (setq i -1)
  (vl-remove-if '(lambda ( x ) (= (setq i (1+ i)) n)) lst)
)

;; substitutes the first occurrence of item, itm2, for item, itm1, in the list, lst
(defun substonce ( itm1 itm2 lst )
  (if lst
    (if (equal itm2 (car lst))
      (cons itm1 (cdr lst))
      (cons (car lst) (substonce itm1 itm2 (cdr lst)))
    )
  )
)

;; determines if two lists are arithmetically equal
(defun eqlst? (lst1 lst2)
  (apply 'and (mapcar '= lst1 lst2))
)

;; determines if two lines are along the same line
(defun sameline? ( lov1 lov2 / samex? samey? resultsx resultsy )
  (foreach pt1 lov1
    (foreach pt2 lov2
      (setq samex? (= (car pt1) (car pt2))
	    samey? (= (cadr pt1) (cadr pt2)))
      (setq resultsx (cons samex? resultsx)
	    resultsy (cons samey? resultsy))
    )
  )
  (or (apply 'and resultsx)
      (apply 'and resultsy))
)

;; given a list of lines that are on a vertical or horizontal line, return one line
(defun appendline ( lol / startpt endpt lov )
  (setq lov (apply 'append lol))
  (if (not (filter '(lambda ( x ) (/= x (caar lov))) (mapcar 'car lov))) ;; if all x's are equal
    (setq startpt (list (caar lov) (foldr '(lambda ( y1 y2 ) (max y1 y2)) 0 (mapcar 'cadr lov)))
	  endpt (list (caar lov) (foldr '(lambda ( y1 y2 ) (min y1 y2)) (cadar lov) (mapcar 'cadr lov)))) ;; set line to furthest pts
    (if (not (filter '(lambda ( y ) (/= y (cadar lov))) (mapcar 'cadr lov))) ;; if all y's are equal
      (setq startpt (list (foldr '(lambda ( x1 x2 ) (min x1 x2)) (caar lov) (mapcar 'car lov)) (cadar lov))
	    endpt (list (foldr '(lambda ( x1 x2 ) (max x1 x2)) 0 (mapcar 'car lov)) (cadar lov))) ;; set line to furthest pts
    )
  )
  (list startpt endpt)
)

;; determines if two lines have the same point at some specified distance, dist
(defun samepoint? ( lov1 lov2 dist / results )
  (foreach pt1 lov1
    (foreach pt2 lov2
      (setq results (cons (equal pt1 pt2 dist) results))
    )
  )
  (apply 'or results)
)

;; appends the list of lines lol at their shared corners
(defun appendcorner ( lol / main tocombine idxcomb idxmain )
  (setq main (car lol))
  (while (> (length lol) 1)
    (setq tocombine (cadr lol))
    (foreach pt main
      (if (member pt tocombine)
	(progn
	  (setq idxcomb (vl-position pt tocombine)
		idxmain (vl-position pt main))
	  (if (= idxmain 0)
	    (if (= idxcomb 0)
	      (setq main (append (reverse (cdr tocombine)) main))
	      (setq main (append tocombine (cdr main)))
	    )
	    (if (= idxcomb 0)
	      (setq main (append main (cdr tocombine)))
	      (setq main (append main (cdr (reverse tocombine))))
	    )
	  )
	)
      )
    )
    (setq lol (vl-remove tocombine lol))
  )
  main
)

;; finds the line pair of the vertex in a list of vertices
(defun findpair ( vtx lov / idx )
  (setq idx (vl-position vtx lov))
  (if (= (rem idx 2) 1)
    (nth (1- idx) lov)
    (nth (1+ idx) lov)
  )
)

;; finds the length between the given two points, lov
(defun linelength ( pt1 pt2 )
  (sqrt (+ (expt (- (car pt2) (car pt1)) 2) (expt (- (cadr pt2) (cadr pt1)) 2)))
)

;; returns the boundaries of a lwpolyline entitity in the format of '(xmin xmax ymin ymax).
(defun entboundary ( ent / lopt xmin xmax ymin ymax )
  (setq lopt (keyvals 10 (entget ent))
	xmin 36 xmax 0 ymin 24 ymax 0)
  (foreach pt lopt
    (setq xmin (min (car pt) xmin)
	  xmax (max (car pt) xmax)
	  ymin (min (cadr pt) ymin)
	  ymax (max (cadr pt) ymax))
  )
  (list xmin xmax ymin ymax)
)

;; given an entity, ent, returns the selection set of enities around it with the provided list of
;; specifications, lospec and boundary offsets, l, up, r, down.
(defun entconnect ( ent lospec l down r up / bounds botl topr )
  (setq bounds (entboundary ent)
	botl (list (- (car bounds) l) (- (caddr bounds) down))
	topr (list (+ (cadr bounds) r) (+ (cadddr bounds) up)))
;;;  (ssget "_X" (append lospec
;;;		      (list '(-4 . "<AND")
;;;			    	'(-4 . ">=,>=,*") (list 10 (- (car bounds) l) (- (caddr bounds) down) 0.0)
;;;			        '(-4 . "<=,<=,*") (list 10 (+ (cadr bounds) r) (+ (cadddr bounds) up) 0.0)
;;;			    '(-4 . "AND>"))))
;;;	                   ;; '(-4 . ">,>,*") (list 10 (+ (cadr bounds) r) (+ (cadddr bounds) up) 0.0)
  ;; (command "_ZOOM" "Window" botl topr)
  (ssget "_C" botl topr lospec)
  ;; (command "_ZOOM" "Previous" "")
)

;; given two objects, returns a list of their intersection points
(defun intersections ( obj1 obj2 mod / lst return )
    (if (and (vlax-method-applicable-p obj1 'intersectwith)
             (vlax-method-applicable-p obj2 'intersectwith)
             (setq lst (vlax-invoke obj1 'intersectwith obj2 mod)))
      (princ lst)
        (repeat (/ (length lst) 3)
            (setq return (cons (list (car lst) (cadr lst) (caddr lst)) return)
                  lst (cdddr lst))
        )
    )
    (reverse return)
)

;; Modifies the elist of the attributes of the given ent based on the group code, code,
;; and value, val
(defun editatts ( ent code val / att attlist )
  (setq att (entnext ent))
  (if (eq (cdr (assoc 0 (entget att))) "ATTRIB")
    (progn
      (setq attlist (entget att))
      (while (/= (cdr (assoc 0 attlist)) "SEQEND")
	(entmod (subst (cons code val) (assoc code attlist) attlist))
	(setq att (entnext att)
	      attlist (entget att))
      )
    )
  )
  (princ)
)
    

;; Set Attribute Values  -  Lee Mac
;; Sets attributes with tags found in the association list to their associated values.
;; ent - [ent] CadWorx block entity
;; lst - [lst] Association list of ((<tag> . <value>) ... )
;; Returns: nil
(defun setattvals ( ent lst / itm )
  (setq ent (vlax-ename->vla-object ent))
  (foreach att (vlax-invoke ent 'getattributes)
      (if (setq itm (assoc (vla-get-tagstring att) lst))
          (vla-put-textstring att (cdr itm))
      )
  )
)

;; Get Attribute Values  -  Lee Mac
;; Returns an association list of attributes present in the supplied block.
;; ent - [ent] CadWorx block entity
;; Returns: [lst] Association list of ((<tag> . <value>) ... )
(defun getattvals ( ent )
  (setq ent (vlax-ename->vla-object ent))
  (mapcar '(lambda ( att ) (cons (vla-get-tagstring att) (vla-get-textstring att)))
	  (vlax-invoke ent 'getattributes))
)

;; Get Attribute Value  -  Lee Mac
;; Returns the value held by the specified tag within the supplied block, if present.
;; blk - [vla] VLA Block Reference Object
;; tag - [str] Attribute TagString
;; Returns: [str] Attribute value, else nil if tag is not found.
(defun getattval ( ent tag )
  (setq ent (vlax-ename->vla-object ent)
	tag (strcase tag))
  (vl-some '(lambda ( att ) (if (= tag (strcase (vla-get-tagstring att))) (vla-get-textstring att))) (vlax-invoke ent 'getattributes))
)

;; sets up the xdata in VBA format using the given association list
(defun initxdata ( lst / datalst datatypelst i )
  (setq datatypelst (vlax-make-safearray vlax-vbInteger (cons 0 (1- (length lst))))
	datalst (vlax-make-safearray vlax-vbVariant (cons 0 (1- (length lst))))
	i 0)

  (foreach data lst
    (vlax-safearray-put-element datatypelst i (car data))
    (vlax-safearray-put-element datalst i (cdr data))
    (setq i (1+ i))
  )

  (list datatypelst datalst)
)
  

;; iterates through the entities of an AutoCAD drawing, starting with the curent
(defun iterateents (curent / lastent )
  (setq lastent (entlast))
  (if (not curent)
    (progn
      (setq curent (entnext))
      (printent curent)
    ) ;; start with the first entity if curent is not specified
    (if (eq curent lastent)
      (progn
      	(princ (entget lastent))
      	(princ "End of iteration")
      	(exit)
      ) ;; if curent is the last entity then quit the program after printing
      (printent curent) ;; print curent
    )
  )
  (princ)
)

;; helper for iterateents to print entity information and get user response
(defun printent ( ent / ans elist )
  (progn
    (if (assoc -2 (entget ent))
      (iterateents (entnext ent))
    )
    (setq elist (entget ent))
    (princ elist) ;; print entity list
    (if (assoc -1 elist) ;; if entity is visible
      (command "_ZOOM" "Object" ent "") ;; zoom into element
    )
    (redraw ent 3) ;; hightlight ent
    (initget "Yes No")
    (setq ans (cond ((getkword "\nWant the next ent? [Yes/No] <Yes>: "))
		    ("Yes")))
    (redraw ent 4)
    (cond ((eq ans "Yes") (iterateents (entnext ent)) ;; move onto nextent if yes (or nil)
	   (eq ans "No") (quit))) ;; quit if no
  )
)

;; iterates through and shows the user the entities in the given selection set, ss, starting at index, i.
(defun ssiterate ( ss i / ent elist ans )
  (progn
    (setq ent (ssname ss i)
	  elist (entget ent))
    (if (assoc -1 elist)
      (command "_ZOOM" "Object" ent "")
    )
    (princ elist)
    (redraw ent 3)
    (initget "Yes No")
    (setq ans (cond ((getkword "\nWant the next ent? [Yes/No] <Yes>: "))
		    ("Yes")))
    (redraw ent 4)
    (if (and (eq ans "Yes") (< (1+ i) (sslength ss)))
      (ssiterate ss (1+ i))
      (princ "End of iteration")
    )
  )
  (princ)
)

;; given a selection set, deletes all of the entities it contains
;; Arguments:
;;	ss: the selection set to be deleted
(defun ssentdel ( ss / i )
    (if ss
        (repeat (setq i (sslength ss))
	  (entdel (ssname ss (setq i (1- i))))
	)
    )
    (princ)
)

;; prints the elist of every entity in a given selection set
(defun ssprinc ( ss / i ent )
  (setq i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i)
	  i (1+ i))
    (princ (entget ent))
    (princ "\n")
  )
)

;; removes the second selection set from the first selection set
;; Usage: (ssentdel (ssdifference (ssget "_X") (ssget)))
(defun ssdifference ( ss1 ss2 / i ent )
  (setq i 0)
  (repeat (sslength ss2)
    (setq ent (ssname ss2 i)
	  i (1+ i))
    (if (ssmemb ent ss1)
      (setq ss1 (ssdel ent ss1))
    )
  )
  ss1
)

;; remove all the hatch elements in the drawing
(defun removehatches ( )
  (ssentdel (ssget "_X" '((0 . "HATCH") (2 . "SOLID"))))
  (princ)
)

;; properly formats the given variable list outputed by ctrl+alt+C
(defun varlist ( lst )
  (vl-princ-to-string (mapcar '(lambda (x) (strcase (vl-princ-to-string x) t)) lst))
)