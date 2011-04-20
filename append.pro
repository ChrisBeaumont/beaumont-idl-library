;+
; PURPOSE:
;  This function appends a second array to a first one. It's
;  only real convenience is that it handles the case when the first
;  array is undefined.
;
; INPUTS:
;  a: The first array. This need not be defined
;  b: The second array, which must be defined
;
; OUTPUTS:
;  The concatenation of a and b
;
; EXAMPLES:
;  a = [1,2,3]
;  b = [4,5]
;  print, append(a,b)
;    [1,2,3,4,5]
;  d = [6,7]
;  print, append(not_defined, d)
;    [6,7]
;
; MODIFICATION HISTORY:
;  April 2010: Written by Chris Beaumont
;-
function append, a, b
  isa = n_elements(a) ne 0
  isb = n_elements(b) ne 0
  if ~isa && ~isb then $
     message, 'One of the input vectors must exist!'

  if ~isa && isb then return, b
  if ~isb && isa then return, a
  
  if n_elements(a) eq 0 then return, b
  if n_elements(b) eq 0 then return, a
  return, [a, b]
end
