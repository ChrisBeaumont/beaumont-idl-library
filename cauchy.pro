;+
; PURPOSE:
;  This function returns the probability density function of a Cauchy
;  distribution, with median mu and width gamma, evaluated at x.
;
; CATEGORY:
;  Statistics
;
; INPUTS:
;  x: The point at which to evaluate the Cauchy distribution
;  mu: The median of the distribution
;  gamma: The width of the distribution
;
; OUTPUTS:
;  PDF_cauchy(x ; mu, gamma)
;
; MODIFICATION HISTORY:
;  October 2009: Written by Chris Beaumont
;-
function cauchy, x, mu, gamma

  compile_opt idl2
  on_error, 2
  
  ;- check inputs
  if n_params() ne 3 then begin
     print, 'cauchy calling sequence:'
     print, 'result = cauchy(x, mu, gamma)'
     return, !values.f_nan
  endif

  result = !dpi * gamma * (1 + (x - mu)^2D/gamma^2D)
  return, 1D / result
end
