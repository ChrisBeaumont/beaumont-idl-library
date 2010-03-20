;+
; PURPOSE:
;  This function wraps one or more values into the range [0,
;  range). If the values are positive, this function is identical to
;  the mod operator. Any negative values compute to (value mod range)
;  + range.
;
; CATEGORY:
;  Utilities
;
; CALLING SEQUENCE:
;  result = wrap(value, range)
;
; INPUTS:
;  value: One or more values
;  range: A positive scalar which sets the top of the wrapped
;  coordinate system.
;
; OUTPUTS:
;  Multiples of range are added to value to bring it in the range [0,
;  range)
;
; EXAMPLE:
;  Wrap 420 degrees into the range 0 - 360 degrees:
;  IDL> result = wrap(720, 360)
;  IDL> print, result
;        60
;  IDL> print, wrap(-1, 360)
;       359
;
; MODIFICATION HISTORY:
;  March 2009: Written by Chris Beaumont
;-
function wrap, value, range
compile_opt idl2
on_error, 2

if n_params() ne 2 then begin
   print, 'calling sequence:'
   print, 'result = snap(value, range)'
   return, !values.f_nan
endif

if size(range,/dimen) ne 0 || range le 0 then $
   message, 'range must be a positive scalar'

result = value mod range
neg = where(result lt 0, ct)
if ct ne 0 then $
   result[neg] += range
return, result
end
