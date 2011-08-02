;+
; PURPOSE:
;  Evaluate the cumulative distribution function of a lognormal
;  distribution. 
;
;  The lognormal distribution PDF is defined as
;  P(x) = [sqrt(2 * pi) * sigma * x]^(-1) * 
;         exp[ -ln(x / a)^2 / (2 * sigma)^2]
;
; INPUTS:
;  x: The abcissa values at which to compute the CDF
;  a: The median of the distribution (see formula above)
;  sigma: The width of the distribution (see formula above)
;
; KEYWORD PARAMETERS:
;  param: An optional 2-element array, listing [a, sigma]. This is
;         provided for convenience, as some functions (e.g., ksone in
;         the IDL Astronomy user's library) prefer to pass
;         parameters this way. 
;
; OUTPUTS:
;  EDF(x)
;
; MODIFICATION HISTORY:
;  July 2011: Written by Chris Beaumont
;-
function lognormal_cdf, x, a, sigma, param = param

  npar = n_params()
  if npar ne 1 && npar ne 3 then begin
     print, 'calling sequence:'
     print, 'result = lognormal_cdf(x, a, sigma)'
     print, '   or'
     print, 'result = lognormal_cdf(x, param = [a, sigma])'
     return, !values.f_nan
  endif

  if npar eq 1 then begin
     if n_elements(param) ne 2 then $
        message, 'must supply a,sigma or param = [a,sigma]'
     a = param[0]
     sigma = param[1]
  endif else if n_elements(param) ne 0 then $
     message, 'Cannot supply both a,sigma and param'

  return, 0.5 * erfc(alog(a / x) / sqrt(2 * sigma^2))
end
