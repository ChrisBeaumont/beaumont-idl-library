;+
; PURPOSE:
;  This function calculates the divergence of a 1, 2, or 3-dimensional
;  vector function sampled on a uniform grid.
;
; INPUTS:
;  x: The x component of the vector field.
;  y: The y component of the vector field. Optional
;  z: The z component of the vector field. Optional
;
; KEYWORD PARAMETERS:
;  order: The order of the lagrange interpolation polynomial to use
;  when calculating derivatives. See pdiv.pro for details. Defaults to 1.
;
; OUTPUTS:
;  An array of the same shape as data, giving the divergence at each
;  point.
;
; MODIFICATION HISTORY:
;  2010-08-03: Written by Chris Beaumont
;-
function div, x, y, z, order = order
  ;- check inputs
  if n_params() eq 0 then begin
     print, 'calling sequence: '
     print, ' result = div(x, [y, z, order = order])'
     return, !values.f_nan
  endif

  nx = n_elements(x) & ndx = size(x, /n_dim)
  ny = n_elements(y) & ndy = size(y, /n_dim)
  nz = n_elements(z) & ndz = size(z, /n_dim)
  if (ny ne 0 && (nx ne ny)) || (nz ne 0 && (nx ne nz)) then $
     message, 'Input arrays must have the same number of elements'
  if ny ne 0 && (ndx lt 2 || ndy lt 2) then $
     message, 'Dimensionality of input arrays must mach number of input arrays'
  if nz ne 0 && (ndx ne 3 || ndy ne 3 || ndz ne 3) then $
     message, 'Dimensionality of input arrays must mach number of input arrays'

  ;- compute the divergence
  result = x * 0.
  if nx ne 0 then result += pdiv(x, 1, order = order)
  if ny ne 0 then result += pdiv(y, 2, order = order)
  if nz ne 0 then result += pdiv(z, 3, order = order)
  return, result
end

pro test
  ;- divergence = 3
  im = fltarr(10, 10, 10)
  indices, im, x, y, z
  d = div(x, y, z)
  print, minmax(d)
  print, minmax(div(x+5, y, z))

  ;- divergence = 0
  print, minmax(div(y, x, x))
end
