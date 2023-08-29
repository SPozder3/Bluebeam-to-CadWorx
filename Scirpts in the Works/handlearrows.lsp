;; given a point, place an arrow that is identical to using the CADWorx P&ID macros
;; arguments:
;;	size: the size of the arrow (LARGE MEDIUM or SMALL)
;;	pt  : the point that the arrow is placed at
;;	rot : the rotation of the arrow in degrees (0 90 180 or -90)
(defun createarrow ( size pt rot / *error*  scale)
  (defun *error* ( msg / *error* )
    (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
      (princ (strcat "\nError: " msg))
    )
    (redraw) (princ)
  ) ;; error handler

  (if (not (eq size nil)) ;; check that the size is given
    (if (/= (length pt) 3) ;; check that the point is valid
      (prompt "\nError: invalid arrow point given")
      (progn
        (command "-layer" "s" "PD-Graph" "") ;; switch the layer to PD-Graph
	(cond ((eq "LARGE" size) (setq scale 0.1875))
	      ((eq "MEDIUM" size) (setq scale 0.125))
	      ((eq "SMALL" size) (setq scale 0.09375))
	      (t (prompt "\nError: invalid arrow size given")))
	(command "-insert" "ARROW_1" pt scale scale rot);; insert the arrow based on the size
      )
    )
  )
  (princ)
)

;; replaces the arrow symbols with large arrows
(defun replacearrows ( / ssarrow i ent elist vert rightvert )
  (setq ssarrow (ssget "X"
		       '((-4 . "<AND")
			   (0 . "SOLID")
			 (-4 . "AND>"))))
  (if ssarrow
    (progn
      (command "UNDO" "BEGIN")
      (setq i 0)
      (repeat (sslength ssarrow)
	(setq ent (ssname ssarrow i)
	      elist (entget ent)
	      vert (cdr (assoc 11 elist))
	      rightvert (cdr (assoc 12 elist))
	      i (1+ i))
	(createarrow (getarrowsize vert rightvert (cdr (assoc 10 elist))) vert (getarrowrot vert rightvert))
	(entdel ent)
      )
      (command "UNDO" "END")
    )
  )
  (princ)
)

;; helper for replacearrows: given the tip and right vertices, decide the rotation of the arrow
(defun getarrowrot ( tip right / x y )
  (setq x (< (car tip) (car right))
	y (< (cadr tip) (cadr right)))
  (cond ((and x y) 180) ;; facing left
	((and x (not y)) 90) ;; facing up
	((and (not x) y) -90) ;; facing down
	((and (not x) (not y)) 0)) ;; facing right
)

;; helper for replacearrows: given all three vertices of the arrow, decide its size
(defun getarrowsize ( tip right left / xdist ydist)
  (setq xdist (abs (- (car tip) (car right)))
	ydist (abs (- (cadr tip) (cadr right))))
  ;; this is too hard coded and needs a better solution
  (cond ((and (not (abouteq (car left) (car right)))
	      (not (abouteq (cadr left) (cadr right)))) nil) ;; not horiz or vertical
        ((or (> xdist 0.1147) (> ydist 0.1147)) "LARGE") ;; large arrow
	((or (> xdist 0.057) (> ydist 0.057)) "MEDIUM") ;; medium arrow
	(t nil)) ;; else
)