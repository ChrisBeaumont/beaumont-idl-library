;+
; PURPOSE:
;  This function compares 2 arrays, and returns 1 if all of their
;  values are equal.
;
; INPUTS:
;  array1: The first array
;  array2: The second array
;
; KEYWORD PARAMETERS:
;  thresh: For floating point arrays, machine precision can cause tiny
;          changes between numbers that, for practical purpose, you
;          consider to be equal. Set thresh to a positive number below
;          which two floating point numbers are considered equal
;
; OUTPUTS:
;  1 if the arrays are equal, 2 if not
;
; MODIFICATION HISTORY:
;  April 2010: Written by Chris Beaumont
;-
function arr_eq, array1, array2, thresh = thresh
  compile_opt idl2

  n1 = n_elements(array1)
  n2 = n_elements(array2)

  if keyword_set(thresh) then $
     return, (n1 eq 0 && n2 eq 0) || $
     n1 eq n2 && min(abs(array1 - array2) / thresh lt 1) eq 1 $
  else $
     return, (n1 eq 0 && n2 eq 0) || $
     n1 eq n2 && min(array1 eq array2) eq 1
end

pro test
  ;- test integers
  x = indgen(10)
  y = x & y[0] = 3
  
  pass = 'PASS'
  fail = 'FAIL'
  print, 'test 1: ' + (arr_eq(x, x) ? pass : fail)
  print, 'test 2: ' + (arr_eq(x, y) ? fail : pass)
  
  ;- test floats
  x = float(x)
  y = x & y[0] += 1e-4
  
  print, 'test 3: ' + (arr_eq(x,x) ? pass : fail)
  print, 'test 4: ' + (arr_eq(x,y) ? fail : pass)
  print, 'test 5: ' + (arr_eq(x,y,thresh=1e-3) ? pass : fail)
end
  
