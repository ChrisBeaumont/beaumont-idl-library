;+
; PURPOSE:
;  This procedure copies a rectangular subregion of a rectangular
;  array to another one, handling all the crappy edge cases.
;
; INPUTS:
;  src: The source array (to be copied from)
;  x1: The lo x coordinate of the subregion to copy
;  y1: The lo y coordinate of the subregion to copy
;  dest: The destination array (to be copied to)
;  x2: The x coordinate in dest where x1 in src should be copied
;  y2: The y coordinate in dest where y1 in src should be copied
;  dx: The x size of the subregion to copy
;  dy: The y size of the subregion to copy
;
; KEYWORD_PARAMETERS:
;  add: Set to add the src region to dest
;  sub: Set to subtract the src region from dest
;  mult: Set to multiply the src region to dest
;  div: Set to divide the src region from dest
;
; BEHAVIOR:
;  Assuming both images are big enough, the program extracts a
;  dx-by-dy postage stamp from src at position (x1,y1), and inserts it
;  into position (x2,y2) of dest. If any part of the postae stamp
;  falls outside of either image, those pixels are quietly ignored.
;
; MODIFICATION HISTORY:
;  December 2010: Written by Chris Beaumont
;  December 13 2010: Changed implementation for speed. The lower and
;  upper bounds for each array are computed directly, instead of using
;  index arrays and WHERE. cnb.
;  Aug 24 2011: Fixed a bug in handling y trimming. cnb
;-
pro stamp, src, x1, y1, dest, x2, y2, dx, dy, $
           add = add, mult = mult, div = div, sub = sub

  if n_params() ne 8 then begin
     print, 'callng sequence'
     print, 'stamp, src, x1, y1, dest, x2, y2, dx, dy'
     return
  endif

  sz1 = size(src)
  sz2 = size(dest)

  if sz1[0] ne 2 || sz2[0] ne 2 then $
     message, 'src and dest must be 2D arrays'

  if ~is_scalar(x1) || ~is_scalar(y1) || $
     ~is_scalar(x2) || ~is_scalar(y2) || $
     ~is_scalar(dx) || ~is_scalar(dy) then $
        message, 'x1, x2, y1, y2, dx, and dy must be scalars'

  ;- calculate the "trim" -- the amount of the stamp that 
  ;- overhangs either src or dest.
  l_trim = ((-1) * (x1 < x2)) > 0
  b_trim = ((-1) * (y1 < y2)) > 0
  r_trim = ((x1 + dx - sz1[1]) > (x2 + dx - sz2[1])) > 0
  t_trim = ((y1 + dy - sz1[2]) > (y2 + dy - sz2[2])) > 0

  x1_0 = x1 + l_trim & x1_1 = x1 + dx - 1 - r_trim
  y1_0 = y1 + b_trim & y1_1 = y1 + dy - 1 - t_trim
  x2_0 = x2 + l_trim & x2_1 = x2 + dx - 1 - r_trim
  y2_0 = y2 + b_trim & y2_1 = y2 + dy - 1 - t_trim

  ;- requested stamp is completely off both images
  if (x1_1 lt x1_0) || (y1_1 lt y1_0) || $
     (x2_1 lt x2_0) || (y2_1 lt y2_0) then return

  if keyword_set(add) then begin
     dest[x2_0:x2_1, y2_0:y2_1] += src[x1_0:x1_1, y1_0:y1_1]
  endif else if keyword_set(sub) then begin
     dest[x2_0:x2_1, y2_0:y2_1] -= src[x1_0:x1_1, y1_0:y1_1]
  endif else if keyword_set(mult) then begin
     dest[x2_0:x2_1, y2_0:y2_1] *= src[x1_0:x1_1, y1_0:y1_1]
  endif else if keyword_set(div) then begin
     dest[x2_0:x2_1, y2_0:y2_1] /= src[x1_0:x1_1, y1_0:y1_1]
  endif else begin
     dest[x2_0:x2_1, y2_0:y2_1] = src[x1_0:x1_1, y1_0:y1_1]
  endelse

end

pro test

  dest = bytarr(5, 5)
  src = bytarr(2, 2) + 1


  ;- normal case, no edge issues
  stamp, src, 0, 0, dest, 2, 2, 2, 2
  assert, array_equal( dest, [[0,0,0,0,0], [0,0,0,0,0],$
                        [0,0,1,1,0], [0,0,1,1,0],$
                        [0,0,0,0,0]])

  ;- upper edge of dest
  dest *= 0
  stamp, src, 0, 0, dest, 4, 4, 2, 2
  assert, array_equal( dest, [[0,0,0,0,0],[0,0,0,0,0], $
                              [0,0,0,0,0],[0,0,0,0,0], $
                              [0,0,0,0,1]])

  ;- upper edge of src
  dest *= 0
  stamp, src, 1, 1, dest, 0, 0, 5, 5
  assert, array_equal( dest, [[1,0,0,0,0],[0,0,0,0,0], $
                              [0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]])

  ;- lower edge of src
  dest *= 0
  stamp, src, -1, -1, dest, 0, 0, 2, 2
  assert, array_equal( dest, [[0, 0, 0, 0, 0], [0, 1, 0, 0, 0],$
                              [0, 0, 0, 0, 0], [0, 0 ,0,0, 0], [0,0,0,0,0]])

  ;- lower edge of dest
  dest *= 0
  stamp, src, 0, 0, dest, -1, -1, 2, 2
  assert, array_equal( dest, [[1, 0, 0, 0, 0], [0, 0, 0, 0, 0],$
                              [0, 0, 0, 0, 0], [0, 0 ,0,0, 0], [0,0,0,0,0]])

  ;- no overlap
  dest *= 0
  stamp, src, 0, 0, dest, 13, 13, 1, 1
  assert, array_equal(dest, replicate(0, 5, 5))

  print, 'All tests passed'

end

  
  
