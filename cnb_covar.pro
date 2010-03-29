;+
; PURPOSE:
;  This function computes the covariance matrix for a set of
;  multidimensional data
;
; INPUT:
;  data: An (ndimension) x (n point) data array.
;
; OUTPUT:
;  The covariance array. An (ndimension) x (ndimension) array.
;
; MODIFICATION HISTORY:
;  March 2010: Written by Chris Beaumont
;-
function cnb_covar, data
  compile_opt idl2
  on_error, 2
  ;- check input
  if n_params() ne 1 then begin
     print, 'calling sequence'
     print, 'result = cnb_covar(data)'
     return, !values.f_nan
  endif
  
  sz = size(data)
  if sz[0] ne 2 then $
     message, 'data must be a 2 dimensional array'

  npt = sz[2]
  ndim = sz[1]
  av = total(data, 2) / npt
  norm = data - rebin(av, ndim, npt)
  
  result = dblarr(ndim,  ndim)
  for i = 0, ndim - 1, 1 do begin
     for j = i, ndim - 1, 1 do begin
        result[i,j] = mean(norm[i,*] * norm[j,*])
        result[j,i] = result[i,j]
     endfor
  endfor
  return, result
end
