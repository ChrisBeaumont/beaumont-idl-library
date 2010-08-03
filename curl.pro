;+
; PURPOSE:
;  This procedure computes the curl of a 3d vector field sampled on a
;  uniform grid.
;
; INPUTS:
;  x: The x component of the vector field
;  y: The y component of the vector field
;  z: The z component of the vector field
;
; KEYWORD PARAMETERS:
;  order: Sets the precision of the lagrange interpolation when
;  computing partial derivatives. see pdiv.pro.
;
; OUTPUTS:
;  cx: The x component of the curl
;  cy: The y component of the curl
;  cz: The z component of the curl
;
; MODIFICATION HISTORY:
;  2010-08-03: Written by Chris Beaumont
;-
pro curl, x, y, z, cx, cy, cz, order = order
  ;- check inputs
  np = n_params()
  if np lt 3 then begin
     print, 'calling sequence:'
     print, ' curl, x, y, z, cx, cy, cz, [order = order]'
     return
  endif
  nx = n_elements(x) & ny = n_elements(y) & nz = n_elements(z)
  if nx ne ny || nx ne nz then $
     message, 'x, y, and z must be the same size'
  if size(x, /n_dim) ne 3 || size(y, /n_dim) ne 3 || $
     size(z, /n_dim) ne 3 then $
        message, 'x, y, and z must be 3D arrays'

  ;- compute the curl
  xy = pdiv(x, 2, order = order)
  xz = pdiv(x, 3, order = order)
  
  yx = pdiv(y, 1, order = order)
  yz = pdiv(y, 3, order = order)
  
  zx = pdiv(z, 1, order = order)
  zy = pdiv(z, 2, order = order)
  
  cx = zy - yz
  cy = xz - zx
  cz = yx - xy     
  return
end

pro test
  im = fltarr(10, 10, 10)
  indices, im, x, y, z

  ;-curl free field
  print, 'should be 0 0'
  curl, x, y, z, cx, cy, cz
  print, minmax(cx > cy > cz)

  ;- divergence free field.
  ;- cz = 1
  print, 'should be 0, 0 // 0, 0 // 1, 1'
  curl, -y, y, z, cx, cy, cz
  print, minmax(cx), minmax(cy), minmax(cz)
end
