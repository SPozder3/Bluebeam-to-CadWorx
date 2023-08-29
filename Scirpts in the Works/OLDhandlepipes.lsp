;; handles the pipes
(defun OLDhandlepipes ( / pipeslist pipes )
  (timer)
  
  (command "UNDO" "BEGIN")
  (setq pipeslist (OLDbuildpipeslist))
  (setq pipes (OLDcombinepipes pipeslist))
  ;;(princ (length pipes))
  (foreach pipe pipes
    (OLDcreatepipe pipe)
  )
  (command "UNDO" "END")
  
  (princ (strcat "\nElapsed time (ms): " (itoa (timer))))
  (princ)
)

;; Given a list of vertices, make a major primary pipe between those points
;; arguments:
;;	lov - list of vertices that make up the pipe
;; variables:
;;	polyline - the elist of the polyline to be created
;;	vtxs - the vertices that the polyline consists of
(defun OLDcreatepipe ( lov / *error* polyline vtxs )
  (defun *error* ( msg )
    (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
      (princ (strcat "\nError: " msg))
    )
    (redraw) (princ)
  )
  (if (<= (length lov) 1)
    (prompt "Error: not enough vertices for polyline creation") ;; prompts an error if not given enough vertices
    (progn
      (setq polyline (append (list '(0 . "LWPOLYLINE")
			   	   '(100 . "AcDbEntity")
			   	   '(67 . 0)
				   '(410 . "Model")
			  	   '(8 . "PD-Minor-1")
			      	   '(6 . "Continuous")
			   	   '(100 . "AcDbPolyline")
			   	   (cons 90 (length lov))
			   	   '(70 . 0)
			   	   '(43 . 0.02)
			   	   '(38 . 0.0)
			   	   '(39 . 0.0)) ;; beginning of elist
			     (apply 'append
			       (foreach vtx lov
				 (setq vtxs (cons (list (cons 10 vtx)
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
    )
  )
  (princ)
)

;; builds a pipes list based off of the lwpolylines in the initial DWG
(defun OLDbuildpipeslist ( / sspipe i ent lov j lopipes )
  (setq sspipe (ssget "_X" '((0 . "LWPOLYLINE") (370 . 35)))) ;; is a line with 2 vertices
  (if sspipe
    (progn
      (setq i 0)
      (repeat (sslength sspipe)
	(setq ent (ssname sspipe i)
	      lov (mapcar 'gridpt (keyvals 10 (entget ent)))
	      i (1+ i))
	(if (> (length lov) 2)
	  (progn
	    (setq j 1)
	    (repeat (1- (length lov))
	      (setq lopipes (cons (list (nth (1- j) lov) (nth j lov)) lopipes)
		    j (1+ j))
	    )
	  )
	  (setq lopipes (cons lov lopipes))
	)
	;; (entdel ent)
      )
    )
  )
  lopipes
)

(defun OLDcombinepipes ( lop )
  (OLDcombinecorners (OLDcombinegaps lop))
  ;;(combinecorners lop)
  ;;lop
)

(defun OLDcombinegaps ( lop )
  (OLDcombine lop 'OLDcombinegap 'appendline)
)

(defun OLDcombinecorners ( lop )
  (OLDcombine lop 'OLDcombinecorner 'appendcorner)
)

;; alters the list of pipes to combine the pipes based on the given combinefunc and appendfunc
(defun OLDcombine ( lop combinefunc appendfunc / combined )
  (setq combined (apply combinefunc (list (car lop) lop))) ;; find a combine for the first pipe
  (while (> (length combined) 1) ;; while a new combination is found
    (setq lop (removelst combined lop) ;; remove the new combination from the list
	  combined (apply appendfunc (list combined)) ;; append the combination
	  combined (cons combined (apply combinefunc (list combined lop)))) ;; search for new a combination
  ) ;; repeat if new combination is found
  
  (if lop
    (cons (apply appendfunc (list combined))
	  (combine (removelst combined lop) combinefunc appendfunc))
    (if combined
      (list (apply 'append combined))
    )
  ) ;; repeat for all lines until lop is empty
)

;; returns a list of pipes to combine between their gaps
(defun OLDcombinegap ( pipe lop )
  (filter '(lambda ( other ) (OLDsameline? pipe other)) lop)
)

;; returns a list of pipes to combine at their corners
(defun OLDcombinecorner ( pipe lop )
  (filter '(lambda ( other ) (OLDsamepoint? pipe other)) lop)
)

;; given two lines, lov1 and lov2, determine if they are the same line
(defun OLDsameline? ( lov1 lov2 / samex? samey? close? resultsx resultsy resultsclose )
  (foreach pt1 lov1
    (foreach pt2 lov2
      (setq samex? (= (car pt1)  (car pt2))
	    samey? (= (cadr pt1) (cadr pt2))
	    close? (equal pt1 pt2 0.5))
      (setq resultsx (cons samex? resultsx)
	    resultsy (cons samey? resultsy)
	    resultsclose (cons close? resultsclose))
    )
  )
  (and (or (apply 'and resultsx)
	   (apply 'and resultsy))
       (apply 'or resultsclose))
)

;; given two lines, determine if they have a shared point
(defun OLDsamepoint? ( lov1 lov2 / results )
  (foreach pt1 lov1
    (foreach pt2 lov2
      (setq results (cons (eqlst? pt1 pt2) results))
    )
  )
  (apply 'or results)
)