;+
; PURPOSE:
;  This procedure swaps out NANs in an array with another value. It
;  can also undo this swapping
;
; INPUTS:
;  data: A data cube
;  swap: A value to substitute in place of nans within data
;
; KEYWORD PARAMETERS:
;  unswap: Reverse the procedure, replacing instances of swap with
;          nan. Note that any naturally occuring instances of SWAP
;          will be also replaced with nans.
;
; MODIFICATION HISTORY:
;  April 2010: Written by Chris Beaumont
;-
pro nanswap, data, swap, unswap = unswap

  if n_params() ne 2 then begin
     print, 'nanswap calling sequence'
     print, '   nanswap, data, swap, [/unswap]'
     return
  endif

  if n_elements(swap) ne 1 then $
     message, 'swap must be a scalar'

  if keyword_set(unswap) then begin
     hit = where(data eq swap, ct)
     if ct ne 0 then data[hit] = !values.f_nan
  endif else begin
     hit = where(~finite(data), ct)
     if ct ne 0 then data[hit] = swap
  endelse
  return
end
