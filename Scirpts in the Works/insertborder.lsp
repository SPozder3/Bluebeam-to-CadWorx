;; inserts the SE borders to the P&ID
(defun insertborder ( )
  (command "_insert" "s:/CAD_Standards/Symbols/Blocks/SE_DSIZE_REF_ATT.dwg" '(0 0) "1" "1" "0")
  (command "_insert" "s:/CAD_Standards/Symbols/Blocks/SE_DSIZE_REV_ATT.dwg" '(0 0) "1" "1" "0")
  (command "_insert" "s:/CAD_Standards/Symbols/Blocks/SE_DSIZE_TB_ATT.dwg" '(0 0) "1" "1" "0")
)