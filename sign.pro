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
;-
function sign, x
  compile_opt idl2
  on_error, 2
  if x gt 0 then return, 1
  if x lt 0 then return, -1
  return, 0
end
