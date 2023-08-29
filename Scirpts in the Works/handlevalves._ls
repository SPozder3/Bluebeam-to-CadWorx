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

;; given a point, place a valve that is identical to using the CADWorx P&ID macros
;; arguments:
;;	blk: the block name of the valve
;;	pt : the point that the valve is placed at
;;	rot: the rotation of the valve in degrees (0 or -90)
(defun createvalve ( name pt rot / *error* blkvals atd valve x1000 scalefact x1005 )
  (defun *error* ( msg / *error* )
    (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
      (princ (strcat "\nError: " msg))
    )
    (redraw) (princ)
  ) ;; error handler

  ;; EXTEND
  		;; (blkname x1000val scalefactor
  (setq blkvals '(("ICES-VAL_BALL" . "65") ("SE_ICES-Val_GATE_F" . "70") ("ICES-VAL_GATE" . "72"))
	atd (getvar 'attdia))
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

(defun valvename ( ent / bounds area elist ltype vtxct closed? closelines flanged? name )
  (setq bounds (entboundary ent)
	area (boundaryarea bounds)
	elist (entget ent)
	ltype (cdr (assoc 0 elist))
	vtxct (cdr (assoc 90 elist))
	closed? (eq (cdr (assoc 70 elist)) 1)
	flanged? (getflanged? ent))
  ;; EXTEND
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
  name
)

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

(defun valvept ( ent name / bounds pt )
  (setq bounds (entboundary ent))
  ;; EXTEND
  (if (or (eq name "ICES-VAL_GATE") (eq name "SE_ICES-Val_GATE_F"))
    (setq pt (gridpt (list (/ (+ (car bounds) (cadr bounds)) 2)
		           (/ (+ (caddr bounds) (cadddr bounds)) 2))))
  )
  pt
)

(defun valverot ( ent name / bounds rot )
  (setq bounds (entboundary ent))
  ;; EXTEND
  (if (or (eq name "ICES-VAL_GATE") (eq name "SE_ICES-Val_GATE_F"))
    (setq rot (if (> (- (cadr bounds) (car bounds)) (- (cadddr bounds) (caddr bounds))) 0 90))
  )
  rot
)

