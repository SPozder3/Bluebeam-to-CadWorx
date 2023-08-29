(defun C:convertGEVOpd ( / os echo )
  ;; setup system variables
  (setq os (getvar 'osmode)
	echo (getvar 'cmdecho))
  (setvar 'osmode 20517) ;; turn osnap off
  (setvar 'cmdecho 0)

  ;; check if CADWorx is loaded
  (if (not (filter '(lambda ( app ) (wcmatch app "cadworx*")) (arx)))
    (progn
      (*error* "CADWorx not loaded")
      (quit)
    )
  )
  
  ;; create a new drawing with the SE P&ID template
  ; (createpd nil T)

  ;; import the raw entities from the GEVO pdf (containing the flattened markups)
  (importrawents nil nil '(-1.0625 -0.4375) 2.11764)

  ; (handleborder)

  (handlelinks)

  (handlepipes)

  ; (handleinstruments)

  (handlevalves)

  ; (handleequip)

  ; (handletext)

  ; (handletags)

  ; (handledimensions)

  ;; reset system variables
  (setvar 'osmode os)
  (setvar 'cmdecho echo)

  (princ)
)

(defun C:convertallGEVOpds ( )
    ;; setup system variables
  (setq os (getvar 'osmode)
	echo (getvar 'cmdecho))
  (setvar 'osmode 20517) ;; turn osnap off
  (setvar 'cmdecho 0)

  ;; check if CADWorx is loaded
  (if (not (filter '(lambda ( app ) (wcmatch app "cadworx*")) (arx)))
    (progn
      (*error* "CADWorx not loaded")
      (quit)
    )
  )
  
  (setq i 0)
  (repeat 40
    (setq i (1+ i))
    (importrawents "G:\\Engineering\\Discipline Information\\Piping\\Bluebeam-to-CW\\Gevo Project\\BB GEVO flattened.pdf" i '(-1.0625 -0.4375) 2.11764)
    (handlelinks)
    (handlepipes)
    (initget "Yes No")
    (setq ans (cond ((getkword "\nWant the next P&ID? [Yes/No] <Yes>: "))
		    ("Yes")))
    (if (eq ans "Yes")
      (ssentdel (ssget "_X"))
      (progn
        (princ "End of iteration")
	;; reset system variables
        (setvar 'osmode os)
        (setvar 'cmdecho echo)
	(quit)
      )
    )
  )
  (princ "End of iteration")

  ;; reset system variables
  (setvar 'osmode os)
  (setvar 'cmdecho echo)
  (princ)
)

  