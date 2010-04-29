;+
; PURPOSE:
;  Returns the sign of x
;
; CATEGORY:
;  utilities
;
; INPUTS:
;  x: A number
; 
; OUTPUTS:
;  1 if x > 0, -1 if x < 0, 0 if x = 0
;
; MODIFICATION HISTORY:
;  June 2009: Written by Chris Beaumont
;  April 2010: Vectorized. cnb.
;-
function sign, x
  compile_opt idl2
  on_error, 2
  return, 0 + (x gt 0) - (x lt 0)
end
