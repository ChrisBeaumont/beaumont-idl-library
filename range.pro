;+
; PURPOSE:
;  Returns the range (max - min) of an array
;
; CATEGORY:
;  utilities
;
; CALLING SEQUENCE:
;  result = range(array)
;
; INPUTS:
;  array: Array to find the range of
;
; OUTPUTS:
;  the range of array
;
; MODIFICATION HISTORY:
;  Written by: Chris Beaumont, Mar 2009
;- 
function range, array
compile_opt idl2
on_error, 2

if n_params() ne 1 then begin
   print, 'range calling sequence:'
   print, 'result = range(array)'
   return, !values.f_nan
endif

if n_elements(array) eq 0 then message, 'array is not defined'

max = max(array, min = min, /nan)
return, max - min

end
