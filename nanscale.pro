;+
; NAME:
;  nanscale
;
; PURPOSE:
;  Byte scales numbers so that non-finite values are zero, and finite
;  values map from 1 - 255. Useful for image stretching.
; 
; CATEGORY:
;  Display utilities
;
; CALLING SEQUENCE: 
;  stretch = nanacale(input)
;
; INPUTS:
;  input: scalar or array of data
;
; OUTPUTS:
;  stretch: The stretched data
;
; MODIFICATION HISTORY:
;  Written by: Chris Beaumont, December 2008
;- 
function nanscale, input
compile_opt idl2
on_error, 2

if n_params() ne 1 then begin
   print, 'result = nanscale(input)'
   return, -1
endif

bad = where(~finite(input), ct)
range = minmax(input, /nan)

output = 1. + 254. * (input - range[0]) / (range[1] - range[0])
if ct ne 0 then output[bad] = 0

return, byte(output)
end
