;+
; PURPOSE:
;  This function tests whether two arrays have identical contents.
;
; INPUTS:
;  array1: An array
;  array2: A second array
;
; OUTPUTS:
;  1 if the two arrays have identical contents. 0 otherwise.
;  Be careful of applying this test to floating point arrays, 
;  which may differ in trivial ways.
;
; MODIFICATION HISTORY:
;  2010-08-03: Written by Chris Beaumont
;-
function arreq, array1, array2
  if n_elements(array1) ne n_elements(array2) then return, 0
  return, min(array1 eq array2)
end

pro test
  x = indgen(30)
  y = indgen(30)
  assert, arreq(x, y)
  assert, arreq(size(x), size(y+1))
  assert, ~arreq(x, y+1)
end
