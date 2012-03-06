;+
; PURPOSE:
;  This function computes the covariance matrix for a set of
;  multidimensional data. It optionally returns the principal axes, of
;  the distribution, along with the variance along each principal
;  axis.
;
;  The principal axes and variances describe the orientation and size
;  of the "error ellipse" for multivariate gaussians.
;
; INPUT:
;  data: An (ndimension) x (n point) data array.
;
; KEYWORDS:
;  paxis: On output, will hold the principal axes of the distribution.
;  An (ndim x ndim) array. The ith row of this array lists the ith
;  principal axis.
;
;  pvar: On output, will hold the variance along each principal axis.
;  An (ndim) vector.
;
;  mean: On output, will hold the mean of the distribution.
;
;  weights: An option (npoint) array that will weight each data point
;
; OUTPUT:
;  The covariance array. An (ndimension) x (ndimension) array.
;
; MODIFICATION HISTORY:
;  March 2010: Written by Chris Beaumont
;  December 2010: Added paxis, pvar, and mean keywords. cnb.
;-
function cnb_covar, data, paxis = paxis, pvar = pvar, mean = mean, weights = weights
  compile_opt idl2
;  on_error, 2
  ;- check input
  if n_params() ne 1 then begin
     print, 'calling sequence'
     print, 'result = cnb_covar(data, [paxis = paxis, pvar = pvar, mean = mean])'
     return, !values.f_nan
  endif
  
  sz = size(data)
  if sz[0] ne 2 then $
     message, 'data must be a 2 dimensional array'

  npt = sz[2]
  ndim = sz[1]

  if n_elements(weights) eq 0 then begin
     w = data * 0 + 1. / npt
  endif else begin
     if n_elements(weights) ne npt then $
        message, 'weights must be an n-point array'
     w = weights / total(weights, /double, /nan)
     w = rebin(1#w, ndim, npt)
  endelse
  w2 = total(w[0,*] ^ 2., /nan)

  av = total(data * w, 2, /nan)
  mean = av
  norm = data - rebin(av, ndim, npt)
  
  result = dblarr(ndim,  ndim)
  for i = 0, ndim - 1, 1 do begin
     for j = i, ndim - 1, 1 do begin
        result[i,j] = total(w[0,*] * norm[i,*] * norm[j,*], /nan) / (1 - w2)
        result[j,i] = result[i,j]
     endfor
  endfor

  pvar = eigenql(result, eigenvec = paxis)
  return, result
end
