;+
; PURPOSE:
;  This function is similar to (and an extension of) built-in
;  functions like indgen, findgen, etc. It creates an array given the
;  first element, last element, and step size. It can create arrays
;  which increase/decrease by constant amounts at each element, or
;  that increase/decrease by constant factors at each element.
;
; CATEGORY:
;  Utilities
;
; CALLING SEQUENCE:
;  result = ARRGEN(first, max, [step, /log, nstep = nstep])
;
; INPUTS:
;  first: The first element in the array. The data type of the result
;         will be the same as first
;  last: The (approximate) last element in the array. If there are not
;        an integer number of elements between first and last, then
;        one extra element will be added. In other words, either the
;        last element will have the value "last", or the last two
;        elements will bracket the value "last."
;  step: The difference between adjacent array elements. If /LOG is
;        not set, this must be a positive number, and specifies the
;        additive offset between adjacent elements. If /LOG is set,
;        this must be a number greater than one. It specifies the
;        multiplicative offset between adjacent elements.
;
; KEYWORD PARAMETERS:
;  LOG: If set, then the array values increase or decrease by a
;       multiplicative (instead of additive) constant at each step. In
;       otherwords, the logarithm of the elements are evenly spaced.
;
;  NSTEP: Set to specifiy the number of elements in the array, instead
;         of the step size. This overrides step.
;
; OUTPUTS:
;  The created array.
;
; EXAMPLE:
;  IDL> print, arrgen(1, 5, 1)
;           1 2 3 4 5
;
;  IDL> print, arrgen(1, 10, 4)
;           1           5           9          13
;
;  IDL> print, arrgen(5, 1, 2)
;           5           3           1
;
;  IDL> print, arrgen(1, 1000, 10, /log)
;       1      10     100    1000
;
;  IDL> print, arrgen(1.0D, 5, 1)
;       1.0000000   2.0000000   3.0000000   4.0000000   5.0000000
;
; MODIFICATION HISTORY:
;  July 2009: Written by Chris Beaumont
;  July 2009: Added nstep keyword
;-
function arrgen, first, last, step, log = log, nstep = nstep
  compile_opt idl2
  on_error, 2
  ;- check inputs
  if n_params() lt 2 then begin
     print, 'arrgen calling sequence:'
     print, 'result = arrgen(first, last, [step , /LOG, nstep = nstep])'
     return, !values.f_nan
  endif

  if last eq first then $
     message, 'last and first cannot be the same number'

  if n_elements(step) ne 0 && step le 0 then $
     message, 'step must positive'
  
  if ~keyword_set(nstep) && n_elements(step) eq 0 then $
     message, 'Must provide a stepsize or number of steps'

  if keyword_set(nstep) && nstep le 1 then $
     message, 'nstep must be >= 2'

  backwards = last lt first

  if keyword_set(log) then begin
     if keyword_set(nstep) then begin
        nstep = round(nstep)
        step = (last / first)^(1D / (nstep-1))
        result = first * step^(lindgen(nstep))
     endif else begin
        if step le 1 then $
           message, 'step size must be > 1 when /LOG is set'
        sz = ceil(abs(alog(1D * last / first)) / alog(step)) + 1
        ind = lindgen(sz) * (backwards ? -1 : 1)
        result = first * step^ind
     endelse
  endif else begin
     if keyword_set(nstep) then begin
        result = first + findgen(nstep) / (nstep - 1) * (last - first)
     endif else begin
        sz = ceil(1D * abs(last - first) / step) + 1
        ind = lindgen(sz) * (backwards ? -1 : 1)
        result = first + ind * step
     endelse
  endelse

  return, result

end
 
  
