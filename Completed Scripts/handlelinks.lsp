;; handles replacing the raw links with link blocks
;; variables:
;;	sslinks - the selection set of raw link data
;;	i - counter used for parsing the selection set
;;	ent - an individual link in the sslinks selection set
;; functions:
;;	timer - utils.lsp
;;	createlink
(defun handlelinks ( / sslinks i ent )
  ;; (timer)
  
  (command "UNDO" "BEGIN")
  (command "_ZOOM" "Window" '(0 0) '(36 24))
  (setq sslinks (ssget "_X" '((0 . "LWPOLYLINE") (370 . 18)
			      (-4 . "<OR") (90 . 5) (90 . 6) (-4 . "OR>")
			      (70 . 1)))
	i 0)
  (repeat (sslength sslinks)
    (setq ent (ssname sslinks i)
	  i (1+ i))
    (createlink ent i)
  )
  (command "_ZOOM" "Previous")
  (command "UNDO" "END")

  ;; (princ (strcat "\nElapsed time (ms): " (itoa (timer))))
  (princ)
)

;; creates a link using the given raw entity (the outline of the link)
;; arguments:
;;	ent - the entity to be replaced
;;	i - the count for the number of links made
;; variables:
;;	lov - the list of vertices that makes up the link
;;	bounds - the rectangular boundary that the link takes up on the drawing
;;	name - the name (as a string) of the link to be created
;;	pos - the insertion point to be used for link creation
;;	lotag - the list of tags to be added to the link
;;	loatt - the list of attributes to be added to the link
;;	atd - the attdia variable (set to 0 during and reset after creation)
;;	link - the inserted link entity block
;;	x1000 - the xdata tag "(TEMP#)" where # is the link count, i
;;	x1005 - the xdata tag for the handle of the entity
;; functions:
;;	gridpt - utils.lsp
;;	keyvals - utils.lsp
;;	entboundary - utils.lsp
;;	entconnect - utils.lsp
;;	editatts - utils.lsp
;;	initxdata - utils.lsp
;;	get1071 - utils.lsp
;;	ssentdel - utils.lsp
;;	linkname
;;	linkpos
;;	linkatts
(defun createlink ( ent i / lov bounds name pos lotag loatt atd link x1000 x1005 )
  (setq lov (keyvals 10 (entget ent))
	bounds (entboundary ent)
	name (linkname ent lov)
	pos (linkpos ent name)
	lotag (linkatts (entconnect ent '((0 . "MTEXT")) 0 (- (caddr bounds) (cadddr bounds)) 0 0.25)) ;; gets tags 0.25 above the link
	loatt (linkatts (entconnect ent '((0 . "MTEXT")) 0 0 0 -0.1)) ;; -0.1 offset to not get the tag values
	atd (getvar 'attdia))
  (setvar 'attdia 0) ;; does not prompt the dialog box
  (command-s "-layer" "s" "PD-Links" "") ;; set layer
  (apply 'command (append (list "-insert" name pos 2.5 2.5 0)
			  (if (wcmatch name "*BIDIR*")
			    (append (list (caddr loatt) (car loatt))
				    lotag (list "")) ;; create bidirection block
			    (append lotag
			  	    (list "" (car loatt) (caddr loatt) (cadr loatt)))) ;; create from/to block
		  )
  )
  (setq link (entlast))
  (editatts link 8 "G-Text")
  (setq x1000 (strcat "(TEMP" (itoa i) ")")
	x1005 (cdr (assoc 5 (entget link))))
  (initxdata (list '(1001 . "CFD1") '(1002 . "{") '(1000 . "") '(1000 . "") (cons 1000 x1000) '(1000 . "")
		   (cons 1071 (get1071)) '(1070 . 256) '(1070 . 306) (cons 1005 x1005) '(1040 . 0.0) '(1040 . 0.0)
		   '(1040 . 0.0) '(1040 . 0.0) '(1040 . 0.0) '(1000 . "") '(1070 . 0) '(1070 . 0) '(1070 . 0)
		   '(1002 . "}")) link)
  (setvar 'attdia atd)
  (ssentdel (entconnect ent '((-4 . "<OR") (-4 . "<AND") (0 . "LWPOLYLINE") (370 . 18) (-4 . "AND>") (0 . "MTEXT") (-4 . "OR>"))
	                0 0 0 0.25))
)

;; creates the name of the link block to be created (ex: GEVO_LINK_LEFT_FROM_txttop)
;; arguments:
;;	ent - the raw entity polyline that makes up the link
;;	lov - list of vertices that make up the outline of the raw link entity
;; variables:
;;	lox - list of x values that make up the given lov
;;	bounds - the rectangular boundary that the link takes up
;;	xmin - the minimum x value that the link goes to
;;	xmax - the maximum x value that the link goes to
;;	sspipeleft - the selection set of pipes to the left of the link
;;	sspiperight - the selection set of pipes to the right of the link
;;	side - the side that the link should go on (L or R)
;;	dir - the direction that the link is assocuated with (from, to or bidirectional)
;; functions:
;;	entboundary - utils.lsp
;;	entconnect - utils.lsp
(defun linkname ( ent lov / lox bounds xmin xmax sspipeleft sspiperight side dir )
  (setq lox (mapcar 'car lov)
	bounds (entboundary ent)
	xmin (car bounds)   xmax (cadr bounds)
	sspipeleft (entconnect ent '((0 . "LWPOLYLINE") (-4 . "<OR") (-4 . "<XOR") (370 . 35) (8 . "PD-Minor-1") (-4 . "XOR>")
				                                     (-4 . "<XOR") (370 . 15) (8 . "PD-Inst-1") (-4 . "XOR>")
				                        (-4 . "OR>")) 0.1 0 -0.2 0) ;; pipe can be a process or instrument line (as raw or CADWorx data
	sspiperight (entconnect ent '((0 . "LWPOLYLINE") (-4 . "<OR") (-4 . "<XOR") (370 . 35) (8 . "PD-Minor-1") (-4 . "XOR>")
				                                      (-4 . "<XOR") (370 . 15) (8 . "PD-Inst-1") (-4 . "XOR>")
				                         (-4 . "OR>")) -0.2 0 0.1 0))
  (if (and sspiperight (not sspipeleft)) ;; set side
    (setq side "LEFT_")
    (if (and (not sspiperight) sspipeleft)
      (setq side "RIGHT_")
    )
  )
  (if (not side)
    (*error* "Could not find a pipe connected to the link")
  )
  (if (eq side "LEFT_") ;; set direction type
    (cond ((member xmin (cdr (member xmin lox))) (setq dir "FROM_"))
	  ((member xmax (cdr (member xmax lox))) (setq dir "TO_"))
	  ((and (member xmin lox) (member xmax lox)) (setq dir "BIDIR_")))
    (cond ((member xmin (cdr (member xmin lox))) (setq dir "TO_"))
	  ((member xmax (cdr (member xmax lox))) (setq dir "FROM_"))
	  ((and (member xmin lox) (member xmax lox)) (setq dir "BIDIR_")))
  )
  (cond ((eq dir "BIDIR_") ;; finish name
	(strcat "SE_LINK_" dir side "TXTTOP"))
	((or (eq dir "FROM_") (eq dir "TO_"))
	(strcat "GEVO_LINK_" side dir "txttop"))
  )
)

;; creates the insertion point of the link block
;; arguments:
;;	ent - the raw link entity
;;	name - the name that will be used for the link block
;; variables:
;;	linkbounds - the bounds that the raw link takes up
;;	linkxmin - smallest x value that the link extends to
;;	linkxmax - largest x value that the link extends to
;;	linkymin - smallest y value that the link extends to
;;	linkymax - largest y value that the link extends to
;;	sspipe - the selection set of the pipe(s) around the link
;;	pipe - the pipe closest to the link
;;	pipelov - the vertices that make up the pipe closest to the link
;;	linkpt - the link point where the pipe will connect
;;	pipebounds - the boundary that the pipe takes up
;; functions:
;;	entboundary - utils.lsp
;;	entconnect - utils.lsp
;;	roundsixteenth - utils.lsp
;;	*error* - utils.lsp
;;	extendpipe - handlepipes.lsp
(defun linkpos ( ent name / linkbounds linkxmin linkxmax linkymin linkymax sspipe pipe pipelov linkpt connectpt pipebounds )
  (setq linkbounds (entboundary ent)
	linkxmin (car linkbounds) linkxmax (cadr linkbounds)
	linkymin (caddr linkbounds) linkymax (cadddr linkbounds)
	sspipe (entconnect ent '((0 . "LWPOLYLINE") (-4 . "<OR") (-4 . "<XOR") (370 . 35) (8 . "PD-Minor-1") (-4 . "XOR>")
				                                 (-4 . "<XOR") (370 . 15) (8 . "PD-Inst-1") (-4 . "XOR>")
				                    (-4 . "OR>")) 0.1 0 0.1 0))
  (if sspipe
    (if (wcmatch name "*LEFT*")
      (progn
        (setq linkpt (list linkxmin (/ (+ linkymin linkymax) 2))
	      pipe (ssname sspipe 0)
	      pipelov (keyvals 10 (entget pipe))
	      connectpt (gridpt (car (vl-sort pipelov '(lambda ( a b ) (< (distance linkpt a) (distance linkpt b)))))))
        (if (< linkxmin 5.0)
	  (progn
	    (extendpipe pipe (list 2.75 (cadr connectpt)))
	    (list 0.25 (cadr connectpt))
	  )
	  (progn
	    (extendpipe pipe connectpt)
	    (list (- (car connectpt) 2.5) (cadr connectpt))
	  )
        )
      )
      (progn
        (setq linkpt (list linkxmax (/ (+ linkymin linkymax) 2))
	      pipe (ssname sspipe 0)
	      pipelov (keyvals 10 (entget pipe))
	      connectpt (gridpt (car (vl-sort pipelov '(lambda ( a b ) (< (distance linkpt a) (distance linkpt b)))))))
        (if (> linkxmax 32.0)
	  (progn
	    (extendpipe pipe (list 31.75 (cadr connectpt)))
	    (list 34.25 (cadr connectpt))
	  )
	  (progn
	    (extendpipe pipe connectpt)
	    (list (+ (car connectpt) 2.5) (cadr connectpt))
	  )
	)
      )
    )
    (*error* "Could not find a pipe to connect to the link")
  )
)

;; creates and formats a list of the attribute values for the link block
;; arguments:
;;	ss - the selection set of raw attribute MText that will be added to the link block
;; variables:
;;	i - the counter used to parse the selection set
;;	att - an individual attribute in the selection set
;;	yval - the y value of the attribute
;;	txtval - the text associatied with an attribute
;;	loval - the list of attribute values
(defun linkatts ( ss / i att yval txtval loval )
  (setq i 0)
  (repeat (sslength ss)
    (setq att
	  (ssname ss i)
	  yval (caddr (assoc 10 (entget att)))
	  txtval (cdr (assoc 1 (entget att)))
	  i (1+ i))
    (setq loval (cons (list txtval yval) loval))
  )
  (mapcar 'car (vl-sort loval '(lambda (y1 y2) (> (cadr y1) (cadr y2)))))
  ;; sort the attributes from highest y to lowest
)