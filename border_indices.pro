;+
; PURPOSE:
;  This function calculates and returns the (1D) indices corresponding
;  to the borders of an array. These indices can be conerted back into
;  n_dimensional indices via array_indices.
;
; INPUTS:
;  Array: An array of any size
;  width: A scalar or vector specifying the border width. If width is
;  a scalar, than an n-pixel border will be extraced along every
;  dimension. Otherwise, width must have the same number of elements
;  as array has dimensions. Each element specifies the border width
;  along the corresponding dimension.
;
; OUTPUTS:
;  The indices corresponding to the border
;
; EXAMPLE:
;  to trim a 10 pixel border off of an image
;  im = findgen(3,3)
;  print, im
;    0 1 2
;    3 4 5
;    6 7 8
;  border = border_indices(im, 1)
;  im[border] = 0
;  print, im
;   0 0 0
;   0 4 0
;   0 0 0
;
; SEE ALSO:
;  reshape, indices
;
; MODIFICATION HISTORY:
;  April 2010: Written by Chris Beaumont
;-
function border_indices, array, width

  ;- check inputs
  if n_params() ne 2 then begin
     print, 'calling sequence'
     print, ' result = border_indices(array, width)'
     return, !values.f_nan
  endif

  nd = size(array, /n_dim)
  sz = size(array)
  ndw = n_elements(width)

  if ndw eq 1 then width = replicate(width, nd) $
  else if ndw ne nd then message, $
     'width has incorrect number of elements'
  for i = 0, nd - 1, 1 do begin
     if 2 * width[i] gt sz[i+1] then message, $
        'requested border exceeds size of array'
  endfor


  ;- inds helps to index the array (whose shape is unknown)
  inds = lindgen(n_elements(array))
  inds = reshape(inds, array, /over)

  ;- loop through dimensions and extract borders
  for i = 0, nd - 1, 1 do begin

     prestar = (i eq 0) ? '' : strjoin(replicate('*, ', i))
     poststar = (i eq nd - 1) ? '' : strjoin(replicate(', *', nd - i - 1))

     cmd = 'sub = inds['+prestar+'0:width[i]-1'+poststar+']'
     void = execute(cmd)
     num = n_elements(sub) & sub = reform(sub, num, /over)
     result = append(result, sub)

     cmd = 'sub = inds['+prestar+'sz[i+1]-width[i]:*'+poststar+']'
     void = execute(cmd)
     num = n_elements(sub) & sub = reform(sub, num, /over)
     result = append(result, sub)

  endfor

  ;- eliminate all the duplicates, and order
  result = result[uniq(result, sort(result))]
  return, result
end

pro test
  ;- 1D case
  x = intarr(10)
  b = border_indices(x, 3)
  x[b] = 1
  assert, min(x[0:2]) eq 1 && min(x[7:9]) eq 1 && max(x[3:6]) eq 0

  ;- 2D case
  x = intarr(3, 3) + 1
  b = border_indices(x, 1)
  x[b] = 0
  assert, total(x) eq 1 && x[1, 1] eq 1

  ;- 3D case
  x = intarr(3, 3, 3) + 1
  b = border_indices(x, 1)
  x[b] = 0
  assert, total(x) eq 1 and x[1,1,1] eq 1

  ;- 8D case
  x = intarr(2, 2, 2, 2, 2, 2, 2, 2) + 1
  b = border_indices(x, 1)
  x[b] = 0
  assert, total(x) eq 0

end
