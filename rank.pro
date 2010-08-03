;+
; PURPOSE:
;  This function computes the rank of each element in a data set. The
;  rank 0 element is the smallest element, the rank 1 element is
;  second smallest, etc.
;
; INPUTS:
;  data: An array of numbers
;
; OUTPUTS:
;  An array the same size as data, giving the rank of each element.
;
; MODIFICATION HISTORY:
;  2010-08-03: Written by Chris Beaumont
;-
function rank, data
  if n_elements(data) eq 0 then begin
     print, 'calling sequence:'
     print, ' result = rank(data)'
     return, !values.f_nan
  endif

  s = sort(data)
  ind = lindgen(n_elements(data))
  result = ind*0
  result[s] = ind

  return, result
end

pro test
  x = [3, 5, 2, 6]
  print, rank(x)
end
