;+
; PURPOSE:
;  This function computes the geometric mean of a set of numbers. The
;  geometric mean is defined as mean = (x1 * x2 * ... *
;  xn)^(1/n). This procedure works with the logarithms of x, which
;  prevents overflow/underflow issues when working with larger or
;  small numbers.
;
; INPUTS:
;  data: A vector of data. Must be non-negative and finite
;
; OUTPUTS:
;  The geometric mean of the data
;
; MODIFICATION HISTORY:
;  July 2010: Written by Chris Beaumont
;-
function gmean, data
  
  if n_params() ne 1 then begin
     print, 'calling sequence:'
     print, ' result = gmean(data)'
     return, !values.f_nan
  endif

  lo = min(data, /nan)
  
  if lo lt 0 || min(finite(data)) eq 0 then $
     message, 'Data must be finite and non-negative'
  if lo eq 0 then return, 0

  return, 10^mean(alog10(data))
end

pro test
  assert, gmean([1,1,1]) eq 1
  assert, gmean([0, 1, 2]) eq 0
  assert, gmean([2, 4, 1]) eq 2
end
