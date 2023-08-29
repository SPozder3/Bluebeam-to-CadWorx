;;---------------------=={ Entity List }==--------------------;;
;;                                                            ;;
;;  Displays the DXF Information for an entity, a variation   ;;
;;  of the program by Michael Puckett of the same name.       ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac - www.lee-mac.com                         ;;
;;------------------------------------------------------------;;
;;  Arguments: entity - entity (ename) / DXF list             ;;
;;------------------------------------------------------------;;

(defun elist ( entity / prin1x princx data xdata )

    (defun prin1x ( x i ) (repeat i (princ "  ")) (prin1 x))
    (defun princx ( x i ) (repeat i (princ "  ")) (princ x))

    (cond
        (   (or
                (and
                    (eq 'ENAME (type entity))
                    (setq data (entget entity))
                )
                (and
                    (listp entity)
                    (setq data entity)
                    (setq entity (cdr (assoc -1 data)))
                )
            )
            (princ "\n\n  (\n")
            (foreach pair data
                (prin1x pair 2)
                (princ "\n")
            )
            (if (setq xdata (assoc -3 (entget entity '("*"))))
                (progn
                    (princx "(" 2)
                    (prin1 (car xdata))
                    (princ "\n")
                    (foreach app (cdr xdata)
                        (princx "(" 3)
                        (princ "\n")
                        (foreach pair app (prin1x pair 4) (princ "\n"))
                        (princx ")" 3)
                        (princ "\n")
                    )
                    (princx ")" 2)
                    (princ "\n")
                )
            )
            (princ "  )")
            (if (= 1 (cdr (assoc 66 data)))
                (while
                    (progn
                        (elist (setq entity (entnext entity)))
                        (not (eq "SEQEND" (cdr (assoc 0 (entget entity)))))
                    )
                )
            )
        )
        (   (print entity)   )
    )
    (princ)
)

;; prints only the xdata elist of an entity to the command line
(defun xdlist ( entity )
  (defun prin1x ( x i ) (repeat i (princ "  ")) (prin1 x))
  (defun princx ( x i ) (repeat i (princ "  ")) (princ x))

  (princ "\n")
  (if (setq xdata (assoc -3 (entget entity '("*"))))
    (progn
      (princx "(" 2)
      (prin1 (car xdata))
      (princ "\n")
      (foreach app (cdr xdata)
        (princx "(" 3)
        (princ "\n")
        (foreach pair app (prin1x pair 4) (princ "\n"))
        (princx ")" 3)
        (princ "\n")
      )
      (princx ")" 2)
      (princ "\n")
    )
  )
  (princ)
)

;; prints the block name of the entity to the command line
(defun blkname ( entity )
  (princ (cdr (assoc 2 (entget entity))))
  (princ)
)

(defun c:ee  nil (elist (car (entsel))))
(defun c:eex nil (elist (car (nentsel))))
(defun c:xd  nil (xdlist (car (entsel))))
(defun c:bln nil (blkname (car (entsel))))

;; prints the elist of every entity in a given selection set using the elist util in elist.lsp
(defun sselist ( ss / i ent )
  (setq i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i)
	  i (1+ i))
    (elist ent)
    (if (assoc -1 (entget ent))
      (command "_ZOOM" "Object" ent "")
    )
    (redraw ent 3)
    (initget "Yes No")
    (setq ans (cond ((getkword "\nWant the next elist? [Yes/No] <Yes>: "))
		    ("Yes")))
    (redraw ent 4)
    (if (not (eq ans "Yes"))
      (progn
        (princ "End of iteration")
	(princ)
      )
    )
  )
  (princ)
)

;; prints the elist for every process line in the drawing
(defun elistpipes ( )
  (sselist (ssget "_X" '((0 . "LWPOLYLINE") (8 . "PD-Minor-1") (210 0.0 0.0 1.0))))
  (princ)
)