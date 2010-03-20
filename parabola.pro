;+
; PURPOSE:
;  This function finds the unique parabola which fits three xy pairs.
;  The parabola is given by y = p + q * x + r * x^2
;
; CATEGORY:
;  utilities
;
; CALLING SEQUENCE:
;  coeff = parabola(x1, x2, x3, y1, y2, y3, [
;                   p = p, q = q, r = r]
;
; INPUTS:
;  x1: First  abscissa
;  x2: Second abscissa
;  x3: Third  abscissa
;  y1: First  ordinate
;  y2: Second ordinate
;  y3: Third  ordinate
;
; KEYWORD PARAMETERS:
;  p: Set to a variable to hold p
;  q: Set to a variable to hold q
;  r: Set to a variable to hodl r
;
; OUTPUTS:
;  An array [p,q,r] which describes the parabola
;  y = p + q * x + r * x^2
;
; MODIFICATION HISTORY:
;  June 2009: Written by Chris Beaumont
;-
function parabola, x1, x2, x3, y1, y2, y3, $ 
                   p = p, q = q, r = r
  compile_opt idl2
  on_error, 2

  if n_params() ne 6 then begin
     print, 'parabola calling sequence'
     print, 'result = parabola(x1, x2, x3, y1, y2, y3, '
     print, '                  [p = p, q = q, r = r]'
     print, 'returns [p,q,r], the parabola y = p + q * x + r * x^2'
     return, !values.f_nan
  endif

  ; y = p + q * x + r * x^2
  ; y2 - y1 = q(x2 - x1) + r(x2^2 - x1^2)
  ; y3 - y1 = q(x3 - x1) + r(x3^2 - x1^2)
  ; let G = (x2 - x1) / (x3 - x1)
  ; y2 - y1 - (y3 - y1) * G = 
  ;   r * (x2^2 - x1^2 - G * (x3^2 - x1^2))
  

  G = double(x2 - x1) / double(x3 - x1)
  lhs = y2 - y1 - double(y3 - y1) * G
  rhs = double(x2^2 - x1^2) - G * (x3^2 - x1^2)  ;- never 0
  r = lhs / rhs
  q = ((y3 - y1) - r * (x3^2 - x1^2)) / (x3 - x1)
  p = y1 - q * x1 - r * x1^2

  return, [p,q,r]
end
