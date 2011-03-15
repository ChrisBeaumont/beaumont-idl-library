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
; KEYWORD PARAMETERS:
;  center: Optionally, set to an array with ndim elements, where ndim
;  is the dimensionality of array. The output indices will be centered
;  around the coordinates of center.
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
;  July 29 2010: Now handles 1D arrays correctly. cnb.
;  Mar 15 2011: Added parameter checking and center keyword. cnb.
;-
pro indices, array, x, y, z, a, b, c, d, e, center = center
  if n_params() eq 0 then begin
     print, 'calling sequence'
     print, 'indices, array, [x, y, z, a, b, c, d, e, center = center]'
     return
  endif
  nd = size(array, /n_dim)
  if keyword_set(center) && n_elements(center) ne nd then $
     message, 'Number of elements in center must match dimension of array'

  if keyword_set(center) then begin
     sz = size(array)
     delta = center - (sz[1:nd] - 1) / 2.
  endif else delta = intarr(nd)

  ind = lindgen(n_elements(array))
  ind = array_indices(array, ind)
  if nd eq 1 then ind = reform(ind, 1, n_elements(ind))
  sz = size(ind)

  if nd ge 1 then x = reshape(ind[0,*], array) + delta[0]
  if nd ge 2 then y = reshape(ind[1,*], array) + delta[1]
  if nd ge 3 then z = reshape(ind[2,*], array) + delta[2]
  if nd ge 4 then a = reshape(ind[3,*], array) + delta[3]
  if nd ge 5 then b = reshape(ind[4,*], array) + delta[4]
  if nd ge 6 then c = reshape(ind[5,*], array) + delta[5]
  if nd ge 7 then d = reshape(ind[6,*], array) + delta[6]
  if nd ge 8 then e = reshape(ind[7,*], array) + delta[7]
return
end

pro test
  a = findgen(10)
  indices, a, x
  assert, array_equal(x, [0,1,2,3,4,5,6,7,8,9])

  a = findgen(3,3)
  indices, a, x, y
  assert, array_equal(x, [ [0,1,2],[0,1,2],[0,1,2] ])
  assert, array_equal(y, [ [0,0,0],[1,1,1],[2,2,2] ])

  indices, a, x, y, center = [0,0]
  assert, array_equal(x, [[-1,0,1], [-1,0,1], [-1,0,1]])
  assert, array_equal(y, [[-1,-1,-1],[0,0,0],[1,1,1]])

  indices, a, x, y, center = [5,0]
  assert, array_equal(x, [[4,5,6], [4,5,6], [4,5,6]])
  assert, array_equal(y, [[-1,-1,-1],[0,0,0],[1,1,1]])


  print, 'all tests passed'
end
