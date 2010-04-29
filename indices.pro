;+
; PURPOSE:
;  This procedure creates coordinate arrays for each dimension in an
;  input array. The coordinate arrays have the same shape as the
;  input, and the i'th pixel in those arrays denote that dimension's
;  coordinate value at the i'th location in the input
;
; INPUTS:
;  array: An array to generate indices for
;
; OUTPUTS:
;  x: The coordinate array for dimension 1
;  y: The coordinate array for dimension 2
;  z: The coordinate array for dimension 3
;  a: The coordinate array for dimension 4
;  b: The coordinate array for dimension 5
;  c: The coordinate array for dimension 6
;  d: The coordinate array for dimension 7
;  e: The coordinate array for dimension 8
;
; EXAMPLES:
;  array = findgen(3,3)
;  indices, array, x, y
;  print, x
;   0 1 2
;   0 1 2
;   0 1 2
;  print, y
;   0 0 0
;   1 1 1 
;   2 2 2
;
; MODIFICATION HISTORY:
;  April 2010: Written by Chris Beaumont
;-
pro indices, array, x, y, z, a, b, c, d, e

  ind = lindgen(n_elements(array))
  ind = array_indices(array, ind)
  sz = size(ind)
  nd = sz[1]

  if nd ge 1 then x = reshape(ind[0,*], array)
  if nd ge 2 then y = reshape(ind[1,*], array)
  if nd ge 3 then z = reshape(ind[2,*], array)
  if nd ge 4 then a = reshape(ind[3,*], array)
  if nd ge 5 then b = reshape(ind[4,*], array)
  if nd ge 6 then c = reshape(ind[5,*], array)
  if nd ge 7 then d = reshape(ind[6,*], array)
  if nd ge 8 then e = reshape(ind[7,*], array)
return
end

pro test
  
  a = findgen(3,3)
  indices, a, x, y
  print, x
  print, y
end
