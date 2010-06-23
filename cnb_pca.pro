;+
; PURPOSE:
;  This procedure calculates the principal components of a
;  dataset. Many IDL routines do this, but I don't really
;  understand their idiosyncrasies. In particular, the builtin PCOMP
;  has weird outputs, and chokes whin n_dim >> 1, n_dim << n_data.
;  This procedure efficiently handles that case. 
;
;  This procedure is the driver for the PRICOM object class. That
;  class has methods for projecting new data on to principal
;  components, etc. 
;
; INPUTS:
;  data: An n_dim x n_data array of data points
; 
; OUTPUTS:
;  eval: The eigenvalues associated with each principal component. The
;  eigenvalues are proportional to the scatter of the data projected
;  onto the principal component.
;  evec: The principal components. 
;  
; KEYWORD PARAMETERS:
;  mean: The routine subtracts off the mean data vector before
;        performing the analysis. This keyword holds that mean
;
; MODIFICATION HISTORY:
;  June 11 2010: Written by Chris Beaumont
;-
pro cnb_pca, data, eval, evec, mean = mean
  compile_opt idl2
  on_error, 2

  ;- check inputs
  if n_params() ne 3 then begin
     print, 'Calling sequence:'
     print, ' cnb_pca, data, eval, evec, mean = mean'
     return
  endif
  if size(data, /n_dim) ne 2 then $
     message, 'data must be a 2d array'


  sz = size(data)
  ndim = sz[1] & ndata = sz[2]
  mean = total(data, 2) / ndata
  d = data - rebin(mean, ndim, ndata)

  ;- case 1: ndim > ndata (common)
  if ndim gt ndata then begin
     a = 1D / ndata * (d ## transpose(d))
     sz = size(a)
     assert, sz[1] eq ndata && sz[2] eq ndata
     
     eval = eigenql(a, /double, eigenvectors = evec)
     sz = size(evec)
     evec = transpose(transpose(d) ## transpose(evec))
  endif else begin
  ;- case 2: ndim < ndata (rare)
     c = transpose(d) ## d
     eval = eigenql(c, /double, eigenvectors = evec)
  endelse
   
  ;- there are only min(ndim, ndata) nonzero eigenvalues
  evec = evec[*, 0:(ndim < ndata) - 1]
  eval = eval[0:(ndim < ndata) - 1]

  ;- normalize the eigenvectors and eigenvalues
  sz = size(evec)
  evec /= rebin(1#sqrt(total(evec^2, 1)), sz[1], sz[2])
  eval /= total(eval)
end

pro test


  ;- first PCA should be in x direction
  ndata = 20
  ndim = 2
  data = randomn(seed, ndim, ndata)
  data[0,*] *= 30

  cnb_pca, data, eval, evec
  print, 'evec - should be along x'
  print, evec[*,0]

  ;- first 2 PCAs should be along dimension 1, then 3
  ndim = 3
  data = randomn(seed, ndim, ndata)
  data[0,*] *= 50
  data[2,*] *= 10
  cnb_pca, data, eval, evec
  print, 'evec 1 - should be along x'
  print, evec[*,0]
  print, 'evec 2 - should be along z'
  print, evec[*,1]

  ;- should still work when dim << data
  ndim = 1000
  ndata = 100
  data = randomn(seed, ndim, ndata)
  data[0,*] *= 1000
  cnb_pca, data, eval, evec
  print, 'evec1 (first 5 dims). Should be along x'
  print, evec[0:4, 0]


  ;- superposition of sinusoids
  ;- some instabilities for the later PCs
  print, '***** SINUSOIDS *****'
  npts = 1000
  nsins = 10
  ndata = 50
  x = arrgen(0, 2 * !pi, nstep = npts)
  sin = fltarr(npts, ndata)
  for i = 0, ndata - 1, 1 do begin
     for j = 0, nsins - 1, 1 do begin
        sin[*, i] += exp(-j) * randomn(seed) * sin(x * j)
     endfor
  endfor

  pca = obj_new('pricom', sin)
  pc = pca->get_pc()
  eval = pca->get_eval()
  print, eval
  help, pc
  for i = 0, ndata - 1, 1 do begin
     x = pca->project(sin[*,i], nterm = 4)
     plot, sin[*,i]
     oplot, x, color = fsc_color('red'), line=2
     stop
  endfor


end
