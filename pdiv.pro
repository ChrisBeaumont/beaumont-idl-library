;+
; PURPOSE:
;  This function estimates the partial derivative of a
;  multi-dimensional function, sampled on a regular grid
;
; INPUTS:
;  data: An n-dimensional datacube, representing a function evenly
;        sampled on a grid.
;  dimension: The dimension (1-n_dimension(data)) over which to
;             calculate the partial derivative (df / d_dim). Defaults
;             to 1.
;
; KEYWORD PARAMETERS:
;  order: 1-3, indicating how to approximate the derivative. All
;         methods implicitly use lagrange interpolation to express
;         each data point as a point on a polynomial, and then
;         differentiate that polynomial. Order=1,2,3 correspond to a
;         (3,5,7) point interpolation scheme. Defaults to 1.
;
; OUTPUTS:
;  A grid the same size as data, giving the partial derivative along
;  dimension at each data point
;
; MODIFICATION HISTORY:
;  August 2010: Written by Chris Beaumont
;-
function pdiv, data, dimension, order = order
  compile_opt idl2

  ;- check inputs
  if n_params() eq 0 then begin
     print, 'result = pdiv(data, [dimension, order = order])'
     return, !values.f_nan
  endif

  nd = size(data, /n_dim) & sz = size(data, /dim)
  if n_elements(dimension) eq 0 then dimension = 1

  if nd lt dimension then $
     message, 'dimension must be <= dimensionality of data'
  if dimension le 0 then message, 'dimension must be >0'

  if ~keyword_set(order) then order = 1
  if 2 * order + 1 gt sz[dimension-1] then $
     message, 'Number of elements in array is too small for requested order'

  ;- coefficients of the derivative of the lagrange interpolation
  case order of 
     1: begin
        denom = 2.
        ;delta   -6 -5 -4 -3 -2 -1  0  1  2  3  4  5  6
        coeff = [[0, 0, 0, 0, 0,-1, 0, 1, 0, 0, 0, 0, 0], $  ;- center pix
                 [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $  ;- left+2 edge
                 [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $  ;- right-2 edge
                 [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $  ;- left+1 edge
                 [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $  ;- right-1 edge
                 [0, 0, 0, 0, 0, 0,-3, 4,-1, 0, 0, 0, 0], $  ;- left edge
                 [0, 0, 0, 0, 1,-4, 3, 0, 0, 0, 0, 0, 0]]    ;- right edge
     end
     2: begin
        denom = 12.
        ;-                              **0**
        coeff = [[0, 0, 0,   0,  1,  -8,   0,  8,  -1,  0,  0, 0, 0], $
                 [0, 0, 0,   0,  0,   0,   0,  0,   0,  0,  0, 0, 0], $
                 [0, 0, 0,   0,  0,   0,   0,  0,   0,  0,  0, 0, 0], $
                 [0, 0, 0,   0,  0,  -3, -10, 18,  -6,  1,  0, 0, 0], $
                 [0, 0, 0,  -1,  6, -18,  10,  3,   0,  0,  0, 0, 0], $
                 [0, 0, 0,   0,  0,   0, -25, 48, -36, 16, -3, 0, 0], $
                 [0, 0, 3, -16, 36, -48,  25,  0,   0,  0,  0, 0, 0]]
     end
     3: begin
        denom = 60.
        ;-                                       **0**
        coeff = [[0,    0,   0,   -1,   9,  -45,    0,  45,   -9,   1,    0,  0,   0], $ ;- cen
                 [0,    0,   0,    0,   2,  -24,  -35,  80,  -30,   8,   -1,  0,   0], $ ;-l+2
                 [0,    0,   1,   -8,  30,  -80,   35,  24,   -2,   0,    0,  0,   0], $ ; r-2
                 [0,    0,   0,    0,   0,  -10,  -77, 150, -100,  50,  -15,  2,   0], $ ;-l+1
                 [0,   -2,  15,  -50, 100, -150,   77,  10,    0,   0,    0,  0,   0], $ ;-r-1
                 [0,    0,   0,    0,   0,    0, -147, 360, -450, 400, -225, 72, -10], $ ;-left
                 [10, -72, 225, -400, 450, -360,  147,   0,    0,   0,    0,  0,   0]]   ;-right
     end
     else: message, 'order must be 1-3'
  end
  result = data * 0.

  ;- each pixel in the output uses coeffs from a certain 
  ;- row in coeff. Calculate this row
  rows = result * 0
  for i = 0, order - 1, 1 do begin
     ind = border_indices(data, i+1, dim = dimension, /low)
     rows[ind] >= 6 - 2 * i - 1
     ind = border_indices(data, i+1, dim = dimension, /up)
     rows[ind] >= 6 - 2 * i
  endfor

  ;- do the linear combination
  for i = 0, 12 do begin
     if max(abs(coeff[i, *])) eq 0 then continue
     delt = 6 - i
     shift = replicate(0, nd) & shift[dimension-1] = delt
     sh = shift(data, shift)
     result += sh * coeff[i, rows]
  endfor
  result /= denom

  return, result
end
        
pro test
  
  x = findgen(15)
  y = x * 3
  assert, max(abs(pdiv(y, 1, order = 1) - 3)) lt 1e-4
  assert, max(abs(pdiv(y, 1, order = 2) - 3)) lt 1e-4
  assert, max(abs(pdiv(y, 1, order = 3) - 3)) lt 1e-4

  x = rebin(x, 15, 15)
  y = x * 3
  assert, max(abs(pdiv(y, 1, order = 3) - 3)) lt 1e-4
  assert, max(abs(pdiv(y, 2, order = 3))) lt 1e-4

  x = findgen(15)
  x /= 10
  y = 3 * x^2 + x^3
  dydx = (6 * x + 3*x^2) / 10
  print, max(abs(pdiv(y, 1, order = 3) - dydx))
  assert, max(abs(pdiv(y, 1, order = 2) - dydx)) lt 1e-4
  assert, max(abs(pdiv(y, 1, order = 3) - dydx)) lt 1e-4
  
end
