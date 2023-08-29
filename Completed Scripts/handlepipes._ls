;; handles replacing the raw pipes with pipeline objects
;; functions:
;;	timer - util.lsp
;;	createpipe
(defun handlepipes ( )
  ;; (timer)
  
  (command "UNDO" "BEGIN")
  (foreach pipe (combinedplist (buildplist))
    (createpipe pipe)
  )
  (command "UNDO" "END")

  ;; (princ (strcat "\nElapsed time (ms): " (itoa (timer))))
  (princ)
)

;; given a list of vertices, lov, make a pipe object between those points
;; arguments:
;;	lov - list of vertices that make up the pipe
;; variables:
;;	polyline - the elist of the polyline to be created
;;	vtxs - the vertices that the polyline consists of
;;	pipe - the created AutoCAD pipe line
;; functions:
;;	initxdata - utils.lsp
(defun createpipe ( lov / polyline vtxs pipe )
  (if (<= (length lov) 1)
    (*error* (strcat "not enough vertices for polyline creation\n" (vl-princ-to-string lov)))
    ;; prompts an error if not given enough vertices
    (progn
      (setq polyline (append (list '(0 . "LWPOLYLINE") ;; entity type
			   	   '(100 . "AcDbEntity") ;; for AutoCAD entities
			   	   '(67 . 0)
				   '(410 . "Model")
			  	   '(8 . "PD-Minor-1") ;; layer
			      	   '(6 . "Continuous") ;; continuous lines
			   	   '(100 . "AcDbPolyline") ;; for AutoCAD polylines
			   	   (cons 90 (length lov)) ;; number of vertices
			   	   '(70 . 0) ;; open (not a closed polyline)
			   	   '(43 . 0.02)
			   	   '(38 . 0.0)
			   	   '(39 . 0.0))
			     (apply 'append
			       (foreach vtx lov
				 (setq vtxs (cons (list (cons 10 vtx) ;; vertex location
				       '(40 . 0.02)
				       '(41 . 0.02)
			       	       '(42 . 0.0)
				       '(91 . 0)) vtxs))
			       )
		     	     ) ;; add each vertex at a grid pt
		     	     (list (cons 210 '(0.0 0.0 1.0))) ;; add final element to elist
		   	)
      )
      (entmake polyline)
      (setq pipe (entlast))
      (initxdata (list '(1001 . "CFD1") '(1002 . "{") '(1000 . "") '(1000 . "") '(1000 . "") '(1000 . "1")
		       (cons 1071 (get1071)) '(1070 . 16) '(1070 . 301) (cons 1005 (cdr (assoc 5 (entget pipe))))
		       '(1040 . 0.0) '(1040 . 0.0) '(1040 . 0.0) '(1040 . 0.0) '(1040 . 0.0) '(1000 . "")
		       '(1070 . 0) '(1070 . 0) '(1070 . 0) '(1002 . "}")) pipe)
;;;      (setq reactor (list (list (cons 102 "{ACAD_REACTORS")
;;;				(assoc 330 polyline)
;;;				(cons 102 "}"))))
;;;      (entmod (append polyline reactor))
;;;      (pipexdata (entget (entlast)))
    )
  )
  (princ)
)
		    

;; creates the list of pipes, represented as a list of list of vertices that
;; make up the pipes to be created
;; variables:
;;	sspipe - the selection set of raw pipe entities
;;	i - counter to iterate through sspipe
;;	ent - the raw pipe entity
;;	lov - the list of vertices that make up the raw pipe entity
;;	j - counter to iterate through lopipes
;;	lopipes - the resulting list of pipes
;; functions:
;;	gridpt - utils.lsp
;;	keyvals - utils.lsp
;;	*error* - utils.lsp
(defun buildplist ( / sspipe i ent lov j lopipes )
  (setq sspipe (ssget "_X" '((0 . "LWPOLYLINE") (370 . 35) (-4 . "<NOT") (70 . 1) (-4 . "NOT>"))))
  (if sspipe
    (progn
      (setq i 0)
      (repeat (sslength sspipe)
	(setq ent (ssname sspipe i)
	      lov (mapcar 'gridpt (keyvals 10 (entget ent)))
	      i (1+ i))
	(if (> (length lov) 2)
	  (progn ;; handle if the polyline has more than 1 vertices
	    (setq j 1)
	    (repeat (1- (length lov))
	      (setq lopipes (cons (list (nth (1- j) lov) (nth j lov)) lopipes)
		    j (1+ j)) ;; breaks it into single line pipes
	    )
	  )
	  (setq lopipes (cons lov lopipes)) ;; otherwise add the single line (2 vertices)
	)
	(entdel ent) ;; deletes the raw entity
      )
    )
    (*error* "drawing does not contain any raw pipe entities")
  )
  lopipes
)

;; creates the list of pipes that have been combined between their gaps and at the corners
;; arguments:
;;	plist - the original pipe list that has not yet been combined
;; variables:
;;	tocombine - the list of pipes that can be combined
;;	locombined - the list of pipes after being combined
;;	lopipes - the resulting list of pipes after completing combinations
;; functions:
;;	removelst - utils.lsp
;;	gappedpipes
;;	combinegaps
;;	corneredpipes
;;	combinecorners
(defun combinedplist ( plist /  tocombine locombined lopipes )
  ;; combine the gaps
  (while (> (length plist) 0)
    (setq tocombine (gappedpipes (car plist) plist))
    (if (> (length tocombine) 1)
      (setq locombined (append (combinegaps tocombine) locombined))
      (setq locombined (append tocombine locombined))
    )
    (setq plist (removelst tocombine plist))
  )

  ;; combine the corners
  (while (> (length locombined) 0)
    (setq tocombine (corneredpipes (car locombined) locombined))
    (if (> (length tocombine) 1)
      (setq lopipes (append (combinecorners tocombine) lopipes))
      (setq lopipes (append tocombine lopipes))
    )
    (setq locombined (removelst tocombine locombined))
  )
  lopipes
)

;; combines the gaps of the gapped pipe list, plist, and returns the single combined pipe
;; arguments:
;;	plist - the list of pipes to be combined between their gaps
;; variables:
;;	i - the counter for iterating through the given plist
;;	fstlov - the first pipe in the plist
;;	sndlov - the second pipe in the plist
;;	locombined - the list of combined pipes
;; functions:
;;	samepoint? - utils.lsp
;;	appendline - utils.lsp
(defun combinegaps ( plist / i fstlov sndlov combined locombined )
  (setq i 0)
  (repeat (1- (length plist))
    (setq fstlov (nth i plist)
	  sndlov (nth (1+ i) plist)
	  i (1+ i))
    (if (samepoint? fstlov sndlov 0.7) ;; check if fst and snd are close to each other
      (if combined
	(setq combined (cons sndlov combined)) ;; add snd to combined (fst is already added)
	(setq combined (append (list sndlov fstlov) combined))) ;; start combined with fst and snd pipes
      (if combined
        (setq locombined (cons combined locombined) ;; add combined to list
	      combined (list sndlov)) ;; start new combined with sndlov
	(setq locombined (cons (list fstlov) locombined) ;; add fst to combined list
	      combined (list sndlov)) ;; start new combined with sndlov
      )
    )
  )
  (if combined
    (setq locombined (cons combined locombined)) ;; add last combined
  )
  (reverse (mapcar 'appendline locombined))
)

;; combines the corners of the cornered pipe list, plist, and return the single pipe
;; arguments:
;;	plist - the list of pipes to be combined at their matching corners
;; functions:
;;	appendcorner - utils.lsp
(defun combinecorners ( plist )
  (list (appendcorner plist))
)

;; returns a sorted list of the gapped pipes associated with the given lov in a plist
;; arguments:
;;	lov - the list of vertices that represents the pipe to be matched in the plist
;;	plist - the list of pipes to be tested if they are gapped with lov
;; variables:
;;	logaps - the list of pipes that are gaped with lov
;;	sameval - "x" if the pipes are gapped along the x axis, "y" otherwise
;; functions:
;;	sameline? - utils.lsp
(defun gappedpipes ( lov plist / logaps sameval )
  (setq logaps (list lov)
	sameval (if (apply '= (mapcar 'car lov)) "x" "y"))
  (foreach otherlov plist
    (if (and (not (equal lov otherlov)) (sameline? lov otherlov))
      (setq logaps (cons otherlov logaps))
    )
  )
  (if (equal sameval "x")
    (vl-sort logaps '(lambda ( pipe1 pipe2 ) (< (cadar pipe1) (cadar pipe2))))
    ;; sort lowest to highest y
    (vl-sort logaps '(lambda ( pipe1 pipe2 ) (< (caar pipe1) (caar pipe2))))
    ;; sort lowest to highest x
  )
)

;; returns a sorted list of the cornered pipes associated with the given lov in a plist
;; arguments:
;;	lov - the list of vertices that represents the pipe to be matched in the plist
;;	plist - the list of pipes to be tested if they are cornered with lov
;; variables:
;;	locorners - the list of pipes that are cornered with lov
;;	prevlen - the value that represents the length of locorners in the previous iteration
;;	i - the counter for iterating through locorners
;; functions:
;;	samepoint? - utils.lsp
(defun corneredpipes ( lov plist / locorners prevlen i )
  (setq locorners (list lov)
	plist (vl-remove lov plist)
	prevlen 0)
  (while (> (length locorners) prevlen) ;; while a new combination is found
    (setq i (- (length locorners) prevlen)
	  prevlen (length locorners))
    (repeat i ;; repeat for every new pipe
      (setq i (1- i) ;; set to an index
	    lov (nth i locorners)) ;; check each new pipe
      (foreach otherlov plist
	(if (and (not (equal lov otherlov)) (samepoint? lov otherlov 0.0)) ;; find a new combination
	  (setq locorners (cons otherlov locorners)
		plist (vl-remove otherlov plist))
	)
      )
    )
  )
  (reverse locorners) ;; set the given lov to the start of the list
)

;; extends the given pipe to the given pt based on which vertex is closest
;; arguments:
;;	pipe - the pipe to be extended
;;	pt - the pt to be exended to
;; variables:
;;	lov - the list of vertices of the given pipe
;;	closestpt - the closest point to the given point, pt, in the pipe entity
;; functions:
;;	keyvals - utils.lsp
(defun extendpipe ( pipe pt / lov closestpt )
  (setq lov (keyvals 10 (entget pipe))
	closestpt (car (vl-sort lov '(lambda ( a b ) (< (distance pt a) (distance pt b))))))
  (entmod (subst (cons 10 pt) (cons 10 closestpt) (entget pipe)))
  (redraw pipe)
  (princ)
)
  
  