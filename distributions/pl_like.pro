;+
; PURPOSE:
;  This function calculates the log-likelihood of the data, under a
;  powerlaw model. The powerlaw distribution is given by
;   f(x) = (alpha - 1) (xmin)^(alpha - 1) x^(-alpha)
;
; CATEGORY:
;  Statistics
;
; CALLING SEQUENCE:
;  result = pl_like([params, derivs, data = data, alpha = alpha, xmin
;                   = xmin])
; OPTIONAL INPUTS:
;  params: A two element vector specifying [alpha, xmin].
;  derivs: A named variable to hold the partial derivative of the
;          log-likelihood with respect to alpha and xmin.
; 
; KEYWORD PARAMETERS:
;  data:  A vector of data values, assumed to be >= xmin
;  alpha: Another way to specify alpha. This takes precedence over any
;         variable stored in params[0].
;  xmin:  Another way to specify xmin. This takes precedence over any
;         variable stored in params[1].
;
; OUTPUTS:
;  Ln(Product( f(data_i) ) )
;
; RESTRICTIONS:
;  To maximize speed, the function does not check for values of data <
;  xmin. However, the output is useless when any data are < xmin.
;
; MODIFICATION HISTORY:
;  June 2009 Written by Chris Beaumont
;-
function pl_like, params, derivs, $
                  data = data, alpha = alpha, xmin = xmin
  compile_opt idl2
  on_error, 2
  
  ;- check input
  if n_params() eq 0 && ~keyword_set(alpha) then begin
     print, 'powerlaw_likelihood calling sequence:'
     print, 'result = pl_like(params, [alpha = alpha, xmin = xmin]'
     print, '           params: [alpha, xmin]'
     return, !values.f_nan
  endif

  ;- parse input
  if n_elements(params) eq 2 then begin
     a = params[0]
     x = params[1]
  endif else if keyword_set(alpha) && keyword_set(xmin) then begin
     a = alpha
     x = xmin
  endif else message, 'must specify params or alpha, xmin'

  ;- the log-likelihood
  c = (a - 1) * x^(a-1)
  result = total(alog(c * data^(-a))) 

  ;- the partials derivatives of the log-likelihood
  if arg_present(derivs) then begin
     derivs[0] = total( 1 / (a - 1) - alog(data / x) )
     derivs[1] = n_elements(data) * (a - 1) / x
  endif

  return, result
end
