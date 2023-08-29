;; creates a new P&ID based on the given template path and status
(defun createpd ( template status / ae-acad-object newdwg )
;; using ActiveX Commands
  (vl-load-com) ;; load ActiveX cmds
  (setq ae-acad-object (vlax-get-acad-object)) ;; set the AutoCAD app obj
  (if (not template)
    (setq newdwg (vla-add (vla-get-documents ae-acad-object) "s:/CAD_Standards/_SE_Custom/Piping/CADWorx/SE_P&ID/Template/SE_P&ID_24x36.dwt")) ;; use the default SE CAD PROCEDURE template
    (setq newdwg (vla-add (vla-get-documents ae-acad-object) template)) ;; use the given template
    ;; "s:/CAD_Standards/_SE_Custom/Piping/CADWorx/SE_P&ID/Template/SE_P&ID_24x36.dwt"
  )
  (if status
    (vla-put-activedocument ae-acad-object newdwg) ;; set new drawing as active document
    (princ "\nNew drawing created in different tab:")
  )
  (princ)
)

;; "C:\Users\sapozder\OneDrive - Samuel Engineering\Desktop\Bluebeam and AutoCAD Project\BB GEVO flattened.pdf" (only the path)
;; "C:/Users/sapozder/OneDrive - Samuel Engineering/Desktop/Bluebeam and AutoCAD Project/BB GEVO flattened.pdf" (this one works with commadn)
(defun importrawents ( file pgnum pt scale / atd fdia  )
  (setq atd (getvar 'attdia)
	fdia (getvar 'filedia))
  (setvar 'attdia 0)
  (setvar 'filedia 0)
  (if (not file)
    (setq file (getfiled "Select the GEVO flattened markups PDF file: " "g:/Engineering/Discipline Information/Piping/Bluebeam-to-CW/Gevo Project" "pdf" 8))
  )
  (if (not pgnum)
    (progn
      (initget 6)
      (setq pgnum (cond ((getint "Specify the page number <1>: "))
			("1")))
    )
  )
  (if (not pt)
    (setq pt (cond ((getpoint "Specify the insertion point <-1.0625,-0.4375>: "))
		   ("-1.0625,-0.4375")))
  )
  (if (not scale)
    (progn
      (initget 6)
      (setq scale (cond ((getreal "Specify the scale factor (> 1.00) <2.11764>: "))
			("2.11764")))
    )
  )
  (command "-pdfimport" "F" file pgnum pt scale "0")
  (setvar 'attdia atd)
  (setvar 'filedia fdia)
  (princ)
)