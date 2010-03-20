;+
; NAME:
;  KDISTERR
;
; DESCRIPTION:
;  This function calculates the two kinematic distances, with errors,
;  for a given object whose velocity is known to some precision.
;
; CATEGORY:
;  coordinate systems
;
; CALLING SEQUENCE:
;  result=KDISTERR( l, vlow, vhigh, [/RADIANS, /SILENT])
;
; INPUTS:
;  l: The galactic longitude of the source given in degrees (or
;     radians, if /RADIANS is set)
;  vlow: The lower bound for the radial velocity estimate, in km/s
;  vhigh: The uppper bound for the radial velocity estimate, in km/s
;
; KEYWORD PARAMETERS:
;  RADIANS: If set, l is given in radians
;  SILENT: If set, textual summary of the results are suppressed
;
; OUTPUTS:
;  A 4 element vector. The first and third elements give the near and
;  far kinematic distances in kpc, while the second and fourth give
;  error estimates.
;
; PROCEDURE:
;  This routine uses kdist.pro to do all of the work
;
; RESTRICTIONS:
;  This only works for objects with latitudes between 270 degrees and
;  90 degrees, for which there are two possible kinematic distances
;
; MODIFICATION HISTORY:
;  June 2008: Written by Chris Beaumont
;-
function kdisterr, l, vlow, vhigh, RADIANS = RADIANS, SILENT =SILENT
compile_opt idl2
on_error, 2

;- check inputs
if n_params() ne 3 then begin
   print, 'kdisterr calling sequence:'
   print, 'dist = kdisterr(l, vlow, vhigh, [/radians, /silent])'
   print, '       l: Galactic longitude in degrees by default'
   print,'     vlow: Lower velocity bound in km/s'
   print,'    vhigh: Upper velocity bound in km/s'
   print,' /radians: l in radians'
   print, 'dist: [dnear, dfar, derror] in kpc'
   return, -1
endif

if keyword_set(radians) then begin
    low=kdist(l,vlow,/radians)
    high=kdist(l,vhigh,/radians)
endif else begin
    low=kdist(l,vlow)
    high=kdist(l,vhigh)
endelse

result=fltarr(2,2)
result[0]=mean([low[0],high[0]])
result[1]=abs(low[0]-high[0])/2.
result[2]=mean([low[1],high[1]])
result[3]=abs(low[1]-high[1])/2.

if ~keyword_set(silent) then begin
    print,result[0],result[1],format='("Near Kinematic Distance: ", f4.1," +/- ",f3.1," kpc")'
    print,result[2],result[3],format='(" Far Kinematic Distance: ", f4.1," +/- ",f3.1," kpc")'
endif

return, result
end
