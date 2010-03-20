;+
; PURPOSE:
;  This function calculates the inverse hyperbolic sine of one or more
;  numbers. 
;
; CATEGORY:
;  Math
;
; CALLING SEQUENCE:
;  result = asinh(x)
;
; INPUTS:
;  x: One or more numbers
;
; OUTPUTS:
;  asinh(x)
;
; METHOD:
;  Uses the identity asinh(x) = ln(x + sqrt(x^2 + 1)),
;               and  asinh(-x) = -asinh(x)
;  cf. Numerical Recipes, 3rd ed (Press et al), ch 5.6 
;
; MODIFICATION HISTORY:
;  June 2009: Written by Chris Beaumont
;-
function asinh, x

  compile_opt idl2
  on_error, 2
  ;- check inputs
  if n_params() eq 0 then begin
     print, 'asinh calling sequence:'
     print, 'result = asinh(x)'
     return, !values.f_nan
  endif

  neg = where(x lt 0, nneg, complement = pos, ncomp = npos)
  result = x * 0
  
  if npos ne 0 then result[pos] = alog(x[pos] + sqrt(x[pos]^2 + 1))
  if nneg ne 0 then result[neg] = -alog(-x[neg] + sqrt(x[neg]^2 + 1))
  
  return, result

end
