;+
; PURPOSE:
;  This function converts a time length in seconds to a more readible
;  string version of that number in years, days, minutes, and seconds
;
; CATEGORY:
;  utilities
;
; CALLING SEQUENCE:
;  result = time2string(time, [/days])
;
; INPUTS:
;  time: A time length in seconds
;
; KEYWORD PARAMETERS:
;  days: If set, time is interpreted to be a length in days, not seconds.
;
; OUTPUTS:
;  That time, converted to a more sensible string
;
; EXAMPLE:
;  IDL> print, time2string(3605.5)
;    1 hour,  0 minutes and  5.5 seconds
;  IDL> print, time2string(!dpi * 1D8)
;    9 years, 351 days,  2 hours, 27 minutes and 45.4 seconds
;  IDL> print, time2string(1.5, /days)
;    1 day, 12 hours,  0 minutes and 0.0 seconds
;
; MODIFICATION HISTORY:
;  April 5 2009: Written by Chris Beaumont
;  April 7 2009: Added \days keyword
;-
function time2string, time, days = days
  compile_opt idl2
  on_error, 2

  ;- check inputs
  if n_params() ne 1 then begin
     print, 'time2string calling sequence: '
     print, ' result = time2string(duration, [/days])'
     return, 0
  endif

  if n_elements(time) ne 1 then $
     message, 'time must be a scalar value'

  ;- number of seconds
  tempTime = double(time)
  if keyword_set(days) then tempTime *= 86400D
  result = string(tempTime mod 60, format='(f4.1, " seconds")')
  
  ;- number of minutes
  tempTime = floor(tempTime / 60)
  if tempTime eq 0 then return, result
  unit = (tempTime mod 60) eq 1 ? ' minute and ' : ' minutes and '
  result = string(tempTime mod 60, format='(i2)') + unit + result

  ;- number of hours
  tempTime /= 60
  if tempTime eq 0 then return, result
  unit = (tempTime mod 24) eq 1 ? " hour, " : " hours, "
  result = string(tempTime mod 24, format = '(i2)') + unit + result
  
  ;- number of days
  tempTime /= 24
  if tempTime eq 0 then return, result
  unit = (tempTime mod 365) eq 1 ? " day, " : " days, "
  result = string(tempTime mod 365, format = '(i3)')+ unit + result
  
  ;- number of years
  tempTime /= 365
  if tempTime eq 0 then return, result
  unit = (tempTime eq 1) ? " year, " : " years, "
  result = strtrim(tempTime, 2) + unit + result
  
  return, result
end
