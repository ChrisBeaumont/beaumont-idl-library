;+
; PURPOSE:
;  Convert linux time (the number of seconds since UTC Jan 1 1970) to
;  julian date (the number of days since Jan 1 4713 BC).
;
; CATEGORY:
;  utilities
;
; CALLING SEQUENCE:
;  result = linux2jd(linuxTime)
;
; INPUTS:
;  The linuxTime, in seconds
;
; OUTPUTS:
;  The julian date, in days
;
; MODIFICATION HISTORY:
;  March 2009 Written by Chris Beaumont
;  April 8 2009: Fixed bug which treated j2000 as j2001
;-
function linux2jd, linuxTime
compile_opt idl2
on_error, 2

;- check inputs
if n_params() ne 1 then begin
   print, 'linux2jd calling sequence:'
   print, ' jd = linux2jd(linux time)'
   return, !values.f_nan
endif

;- reference numbers
j2000 = 946684800D        ;-linux time at 2000
juldate, [2000,1,1], jd0  ;-reduced julian date at 2000
jd0 += 2400000D           ;-conversion to normal jd

return, jd0 + (linuxTime - j2000) / 86400
end
