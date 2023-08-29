;; handles replacing the raw valves with CADWorx valve objects
;; variables:
;;	ssvalves - the selection set of raw valve entities
;;	i - the counter used to iterate through ssvalves
;;	ent - the current raw entity
;;	name - the name of the CADWorx valve to be inserted
;;	pt - the point where the CADWorx valve will be inserted
;;	rot - the rotation angle of the inserted CADWorx valve
;; functions:
;;	timer - utils.lsp
;;	valvename
;;	valvept
;;	valverot
;;	createvalve
(defun handlevalves ( / ssvalves i ent name pt rot )
  ;; (timer)

  (command "UNDO" "BEGIN")
  (command "_ZOOM" "Window" '(0 0) '(36 24))
  (setq ssvalves (ssget "_X" '((370 . 18)))
	i 0)
	
  (repeat (sslength ssvalves)
    (setq ent (ssname ssvalves i)
	  name (valvename ent)
	  pt (valvept ent name)
	  rot (valverot ent name)
	  i (1+ i))
    (if (and name pt rot)
      (createvalve name pt rot)
    )
  )
  (command "_ZOOM" "Previous")
  (command "UNDO" "END")

  ;; (princ (strcat "\nElapsed time (ms): " (itoa (timer))))
  (princ)
)

;; given a name, point, and rotation angle, place a CADWorx valve
;; arguments:
;;	name - the block name of the valve
;;	pt - the point that the valve is placed at
;;	rot - the rotation of the valve in degrees (0 or -90)
;; variables:
;;	blkvals - an association list the associates a block name with its unique 1000 xdata value
;;	atd - the attdia system variable
;;	valve - the inserted valve
;;	x1000 - the 1000 xdata value
;;	x1005 - the 1005 xdata value representing the handle of the valve
;; functions:
;;	*error* - utils.lsp
;;	editatts - utils.lsp
;;	initxdata - utils.lsp
;;	get1071 - utils.lsp
(defun createvalve ( name pt rot / blkvals atd valve x1000 x1005 )
  ;; EXTEND: add the CADWorx block name and group code 1000 xdata value to the association list
  		;; (blkname x1000val
  (setq blkvals '(("ICES-VAL_BALL" . "65") ("SE_ICES-Val_GATE_F" . "70") ("ICES-VAL_GATE" . "72"))
	atd (getvar 'attdia))
  ;; END EXTEND
  (if (not (assoc name blkvals))
    (*error* (strcat "valve with name is not set up to be replaced"))
  )

  (setvar 'attdia 0) ; does not prompt the dialog box
  (if (/= (length pt) 2) ;; check that the point is valid
    (prompt "\nError: invalid point given when making valve")
    (progn
      (command-s "-layer" "s" "PD-Valves" "") ;; switch the layer to PD-Valves
      (command-s "-insert" name pt "0.25" "0.25" rot) ;; insert the Valve

      (setq valve (entlast)
	    x1000 (cdr (assoc name blkvals))
	    x1005 (cdr (assoc 5 (entget valve))))
      (editatts valve 8 "G-Text")
      (initxdata (list '(1001 . "CFD1") '(1002 . "{") '(1000 . "") '(1000 . "") '(1000 . "") (cons 1000 x1000)
		       (cons 1071 (get1071)) '(1070 . 128) '(1070 . 303) (cons 1005 x1005) '(1040 . 0.25)
		       '(1040 . 0.25) '(1040 . 0.0) '(1040 . 0.0) '(1040 . 0.0) '(1000 . "") '(1070 . 0)
		       '(1070 . 0) '(1070 . 0) '(1002 . "}")) valve)
    )
  )
  (setvar 'attdia atd)
  (princ)
)

;; creates the name of the valve block to be created
;; arguments:
;;	ent - the raw entity lwpolyline that was placed first by pdfimport
;; variables:
;;	bounds - the rectangular boundary that the link takes up
;;	area - the numerical area that the boundary takes up
;;	elist - the entity list of the ent
;;	ltype - the line type of the entity
;;	vtxct - the number of verticies of the entity
;;	closed? - whether or not the entity is closed
;;	flanged? - whether or not the valve is flanged
;;	name - the returned name
;; functions:
;;	entboundary - utils.lsp
;;	boundaryarea - utils.lsp
;;	getflanged?
(defun valvename ( ent / bounds area elist ltype vtxct closed? closelines flanged? name )
  (setq bounds (entboundary ent)
	area (boundaryarea bounds)
	elist (entget ent)
	ltype (cdr (assoc 0 elist))
	vtxct (cdr (assoc 90 elist))
	closed? (eq (cdr (assoc 70 elist)) 1)
	flanged? (getflanged? ent))
  ;; EXTEND: add the specifications that make the raw ent unique to its valve block name
  (if (and (equal area 0.035 0.001) (eq ltype "LWPOLYLINE") (eq vtxct 4))
    (progn
      (if closed? ;; Gate valves
	(progn 
	  (if flanged?
	    (setq name "SE_ICES-Val_GATE_F")
	  )
	  (if (not name)
	    (setq name "ICES-VAL_GATE")
	  )
	)
      )
    )
  )
  ;; EXTEND END
  name
)

;; determines if the entity has two lines close to it that would indicate that the valve is flanged
;; arguments:
;;	ent - the raw entity lwpolyline that was placed first by pdfimport
;; variables:
;;	sslines - the selection set of lwpolylines surrounding the ent
;;	i - the counter used to iterate through sslines
;;	line - the current line in sslines
;;	pts - the vertices that the lines uses
;;	result - the count of the surrounding flange lines
;; functions:
;;	entconenct - utils.lsp
;;	keyvals - utils.lsp
(defun getflanged? ( ent / sslines i line pts result )
  (setq sslines (entconnect ent '((0 . "LWPOLYLINE") (370 . 18) (90 . 2)) 0.2 0.2 0.2 0.2)
	i 0
	result 0)
  (if sslines
    (progn
      ;;(ssiterate sslines 0)
      (repeat (sslength sslines)
        (setq line (ssname sslines i)
	      pts (keyvals 10 (entget line))
	      i (1+ i))
        (if (equal (distance (car pts) (cadr pts)) 0.145 0.001)
          (setq result (1+ result))
        )
      )
    )
  )
  (= 2 result)
)     

;; creates the insertion point of the valve block
;; arguments:
;;	ent - the raw valve entity
;;	name - the name of the block to be inserted
;; variables:
;;	bounds - the rectangular boundary that the ent takes up
;;	pt - the returned insertion point
;; functions:
;;	entboundary - utils.lsp
;;	gridpt - utils.lsp
(defun valvept ( ent name / bounds pt )
  (setq bounds (entboundary ent))
  ;; EXTEND: Using the raw ent and name, add where the insertion point would be
  (if (or (eq name "ICES-VAL_GATE") (eq name "SE_ICES-Val_GATE_F"))
    (setq pt (gridpt (list (/ (+ (car bounds) (cadr bounds)) 2)
		           (/ (+ (caddr bounds) (cadddr bounds)) 2))))
  )
  ;; EXTEND END
  pt
)

;; creates the rotation value of the valve block
;; arguments:
;;	ent - the raw valve entity
;;	name - the name of the block to be inserted
;; variables:
;;	bounds - the rectangular boundary that the ent takes up
;;	rot - the returned rotation value
;; functions:
;;	entboundary - utils.lsp
(defun valverot ( ent name / bounds rot )
  (setq bounds (entboundary ent))
  ;; EXTEND: Using the raw ent and name, add what the rotation (in degrees) should be
  (if (or (eq name "ICES-VAL_GATE") (eq name "SE_ICES-Val_GATE_F"))
    (setq rot (if (> (- (cadr bounds) (car bounds)) (- (cadddr bounds) (caddr bounds))) 0 90))
  )
  ;; EXTEND END
  rot
)

