;+
; PURPOSE:
;  This function calculates the sum of very small numbers in a way that
;  avoids computer underflow errors. This is useful in situatioOBns like
;  likelihood maximization, where individual terms in the likelihood
;  can be very small. The program is vectorized to handle multiple
;  sums simultaneously.
;
; CATEGORY:
;  math
;
; INPUTS:
;  logs: The _natural log_ of the values to sum. NANs are
;        treated as missing data. A vector or 2d array. If it is a
;        vector, then the sum will be taken over all elements. If it
;        is a 2D array, then separate sums will be taken over each row
;        of data. 
; 
; OUTPUTS:
;  The _natural log_ of the sum of small numbers
;
; PROCEDURE:
;  Calculations are done on logarithms, since they are much larger and
;  not susceptible to underflow. The basic calculation is
;   Sum(x) = Xmax * Sum(x / xmax)
;  the numbers x / xmax can be calculated from the logs.
;
; MODIFICATION HISTORY:
;  December 2009: Written by Chris Beaumont
;-
function sumofsmalls, logs
  compile_opt idl2
  on_error, 2

  if n_params() ne 1 then begin
     print, 'calling sequence:'
     print, ' result = sumofsmalls(logs)'
     print, '  result = alog(sum(exp[logs]))'
  endif
  nd = size(logs, /n_dimen)
  if nd ne 1 && nd ne 2 then $
     message, 'input must be a 1 or 2D array'
  nrow = nd eq 1 ? 1 : n_elements(logs[0,*])
  ncol = nd eq 1 ? n_elements(logs) : n_elements(logs[*,0])

  x = max(logs, dimen = 1, /nan)
  xarr = rebin(1#x, ncol, nrow)
  result = x + alog(total(exp(logs - xarr),/nan, 1))
  return, result
end
