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
;  June 21 2011: Fixed bug when center=0. cnb.
;  June 23 2011: Optimized for speed and memory (10x speedup). cnb.
;-
pro indices, array, x, y, z, a, b, c, d, e, center = center
  compile_opt idl2
  on_error, 2

  if n_params() eq 0 then begin
     print, 'calling sequence'
     print, 'indices, array, [x, y, z, a, b, c, d, e, center = center]'
     return
  endif
  nd = size(array, /n_dim)
  sz = size(array)

  if nd eq 0 then $
     message, 'Input must be an array'

  if n_elements(center) ne 0 && n_elements(center) ne nd then $
     message, 'Number of elements in center must match dimension of array'

  do_center = n_elements(center) ne 0
  if do_center then begin
     delta = center - (sz[1:nd] - 1) / 2.
  endif 

  ind = lindgen(n_elements(array))

  if nd ge 1 then begin
     x = ind mod sz[1]
     if do_center then x += delta[0]
     x = reshape(x, array, /over)
  endif
  if nd ge 2 then begin
     ind /= sz[1]
     y =ind mod sz[2]
     if do_center then y += delta[1]
     y = reshape(y, array, /over)
  endif
  if nd ge 3 then begin
     ind /= sz[2]
     z = ind mod sz[3]
     if do_center then z += delta[2]
     z = reshape(z, array, /over)
  endif
  if nd ge 4 then begin
     ind /= sz[3]
     a = ind mod sz[4]
     if do_center then a += delta[3]
     a = reshape(a, array, /over)
  endif
  if nd ge 5 then begin
     ind /= sz[4]
     b = ind mod sz[5]
     if do_center then b += delta[4]
     b = reshape(b, array, /over)
  endif
  if nd ge 6 then begin
     ind /= sz[5]
     c = ind mod sz[6]
     if do_center then c += delta[5]
     c = reshape(c, array, /over)
  endif
  if nd ge 7 then begin
     ind /= sz[6]
     d = ind mod sz[7]
     if do_center then d += delta[6]
     d = reshape(d, array, /over)
  endif
  if nd ge 8 then begin
     ind /= sz[7]
     e = ind 
     if do_center then e += delta[7]
     e = reshape(e, array, /over)
  endif

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

  a = findgen(3)
  indices, a, x, center = 0
  assert, array_equal(x, [-1, 0, 1])

  print, 'all tests passed'
end
