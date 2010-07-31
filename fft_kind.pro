;+
; PURPOSE:
;  This procedure calculates the vector wavenumber at each pixel in
;  an n-dim fft array. It's like indices.pro, except that the k's are
;  normalized and shifted correctly to match the default
;  structure of IDL's FFT command.
;
; INPUTS:
;  array: An n-dimensional array (presumably an fft of something)
;
; KEYWORD PARAMETERS:
;  t: The spacing between each pixel in the pre-FT space. Scalar or
;     vector of length equal to the dimensionality of array. The
;     k value at each pixel in FFT space is 2 pi n/(N * T), where n is
;     an integer, N is the number of pixels along a dimension, and T
;     is the spacing (see FFT documentation). Defaults to 1
;
; OUTPUTS:
;  k1: An array with the same shape as the input, giving the k1
;      coordinate at each pixel
;  k2-k8: Ditto, for the other dimensions
;
; MODIFICATION HISTORY:
;  2010-07-29: Created by Chris Beaumont
;-
pro fft_kind, array, x1, x2, x3, x4, x5, x6, x7, x8, t = t
  ;- check inputs
  if n_params() eq 0 then begin
     print, 'Calling sequence:'
     print, '  fft_kind, array, x1, [x2, x3, ...]'
     return
  endif

  sz = size(array,/dim) & nd = size(array,/n_dim)
  if ~keyword_set(t) then t = replicate(1., nd)
  if n_elements(t) eq 1 then t = replicate(t, nd)
  if n_elements(t) ne nd then $
     message, 't keyword must be a scalar, or equal to the number of dims in array'

  denom = sz * t / (2 * !pi)

  ;- create indices arrays
  indices, array, x1, x2, x3, x4, x5, x6, x7, x8
  offset_even = sz / 2 - 1
  offset_odd = sz / 2
  isEven = ((sz and 1) eq 0)
  offset = offset_even * isEven + offset_odd * (~isEven)
  
  ;- generate the shift command string
  cmd = ' '
  for i = 0, nd - 1, 1 do cmd+=','+strtrim(-offset[i],2)
;  print, cmd

  ;- subtract, normalize, and shift k indices
  for i = 0, nd - 1 do begin
     ist = strtrim(i+1,2)
     cmd1 = 'x'+ist+' -= '+strtrim(offset[i],2)
     cmd2 = 'x'+ist+' = shift(x'+ist+cmd+')'
     cmd3 = 'x'+ist+' /= '+strtrim(denom,2)
;     print, cmd1
;     print, cmd2
;     print, cmd3

     assert, execute(cmd1)
     assert, execute(cmd2)
  endfor
return
end

pro test
  x = fltarr(6)
  fft_kind, x, kx
  print, kx
  
  x = fltarr(5)
  fft_kind, x, kx
  print, kx

  x = fltarr(5, 4)
  fft_kind, x, kx, ky
  print, kx
  print,''
  print, ky
end
