(defun handleinstruments ( / ssinstsyms ent i ssinstlines lwpoly j )
  ;; (timer)

  (command "UNDO" "BEGIN")
  (command "_ZOOM" "Window" '(0 0) '(36 24))
  (setq ssinstsyms (ssget "_X" '((0 . "CIRCLE") (370 . 15)))
	i 0)
  (repeat (sslength ssinstsyms)
    (setq ent (ssname ssinstsyms i)
	  ssinstlines (entconnect ent '((0 . "LWPOLYLINE") (370 . 15)) 0.25 0.25 0.25 0.25)
	  i (1+ i)
	  j 0)
    (createinst ent)
    (repeat (sslength ssinstlines)
      (setq lwpoly (ssname ssinstlines j)
	    j (1+ j))
      (createinstline (mapcar 'gridpt (keyvals 10 (entget lwpoly))))
    )
  )
  (ssentdel (entconnect ent '((-4 . "<OR") (8 . "PDF_Text") (8 . "PDF_Geometry") (-4 . "OR>")) 0.25 0.25 0.25 0.25))
  (command "_ZOOM" "Previous")
  (command "UNDO" "END")

  ;; (princ (strcat "\nElapsed time (ms): " (itoa (timer))))
  (princ)
)

(defun createinst ( ent / vtx bounds name pos loatt atd instblk xdata )
  (setq vtx (car (keyvals 10 (entget ent)))
	bounds (entboundary ent)
	name (instname vtx bounds)
	pos (instpos vtx name)
	loatt (instatts (entconnect ent '((0 . "MTEXT")) 0.25 0.25 0.25 0.25))
	atd (getvar 'attdia))
  (setvar 'attdia 0)
  (command-s "-layer" "s" "PD-Inst-1" "")
  (command "-insert" name pos 0.5 0.5 0)
  (setq instblk (entlast))
  (setattvals instblk (list (cons "TAG" (car loatt)) (cons "FUNCTION" (cadr loatt)) (cons "FAILMODE" (caddr loatt))))
  (setq xdata (initxdata (list '(1001 . "CFD1") '(1002 . "{") (cons 1000 (getattval instblk "FUNCTION")) '(1000 . "")
			       (cons 1000 (getattval instblk "TAG")) '(1000 . "") (cons 1071 (get1071)) '(1070 . 128) '(1070 . 302)
			       (cons 1005 (cdr (assoc 5 (entget instblk)))) '(1040 . 0.0) '(1040 . 0.0) '(1040 . 0.0)
			       '(1040 . 0.0) '(1040 . 0.0) '(1000 . "") '(1070 . 0) '(1070 . 0) '(1070 . 0) '(1002 . "}"))))
  (vla-SetXData (vlax-ename->vla-object instblk) (car xdata) (cadr xdata))
  (setvar 'attdia atd)
)

(defun createinstline ( lov /  polyline vtxs line xdata )
  (if (<= (length lov) 1)
    (*error* (strcat "not enough vertices for polyline creation\n" (vl-princ-to-string lov)))
    ;; prompts an error if not given enough vertices
    (progn
      (setq polyline (append (list '(0 . "LWPOLYLINE") ;; entity type
			   	   '(100 . "AcDbEntity") ;; for AutoCAD entities
			   	   '(67 . 0)
				   '(410 . "Model")
			  	   '(8 . "PD-Inst-1") ;; layer
			      	   '(6 . "INSTRUMENT") ;; instrument line type
			   	   '(100 . "AcDbPolyline") ;; for AutoCAD polylines
			   	   (cons 90 (length lov)) ;; number of vertices
			   	   '(70 . 0) ;; open (not a closed polyline)
			   	   '(43 . 0.0)
			   	   '(38 . 0.0)
			   	   '(39 . 0.0))
			     (apply 'append
			       (foreach vtx lov
				 (setq vtxs (cons (list (cons 10 vtx) ;; vertex location
				       '(40 . 0.0)
				       '(41 . 0.0)
			       	       '(42 . 0.0)
				       '(91 . 0)) vtxs))
			       )
		     	     ) ;; add each vertex at a grid pt
		     	     (list (cons 210 '(0.0 0.0 1.0))) ;; add final element to elist
		   	)
      )
      (entmake polyline)
      (setq line (entlast))
      (setq xdata (initxdata (list '(1001 . "CFD1") '(1002 . "{") '(1000 . "") '(1000 . "") '(1000 . "INSTRUMENT LINE") '(1000 . "0")
				   (cons 1071 (get1071)) '(1070 . 16) '(1070 . 350) (cons 1005 (cdr (assoc 5 (entget line))))
				   '(1040 . 0.0) '(1040 . 0.0) '(1040 . 0.0) '(1040 . 0.0) '(1040 . 0.0) '(1000 . "")
				   '(1070 . 0) '(1070 . 0) '(1070 . 0) '(1002 . "}"))))
      (vla-Setxdata (vlax-ename->vla-object line) (car xdata) (cadr xdata))
    )
  )
)

(defun instname ( vtx bounds )
  "SE_DI_LOCAL"
)

(defun instpos ( vtx name )
  (gridpt vtx)
)

(defun instatts ( ss / i att yval txtval loval )
  (setq i 0)
  (repeat (sslength ss)
    (setq att (ssname ss i)
	  yval (caddr (assoc 10 (entget att)))
	  txtval (cdr (assoc 1 (entget att)))
	  i (1+ i))
    (setq loval (cons (list txtval yval) loval))
  )
  (mapcar 'car (vl-sort loval '(lambda (y1 y2) (< (cadr y1) (cadr y2)))))
  ;; sort the attributes from highest y to lowest
)