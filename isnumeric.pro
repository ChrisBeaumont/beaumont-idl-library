;+
; PURPOSE:
;  This function determines whether a variable corresponds to a
;  numeric data type.
;
; CATEGORY:
;  Utilities.
;
; CALLING SEQUENCE:
;  result = isnumeric(expr)
;
; INPUTS:
;  expr: A variable
;
; OUTPUTS:
;  1 if expr is a numeric scalar or array. 0 otherwise.
;
; MODIFICATION HISTORY:
;  Sep 2009: Written by Chris Beaumont
;-
function isnumeric, expr

  compile_opt idl2
  on_error, 2

  type = size(expr, /type)

  ;- not numeric
  switch type of
     0:
     7:
     8:
     10:
     11: return, 0
  endswitch
  
  ;- numeric
  return, 1

end
    
