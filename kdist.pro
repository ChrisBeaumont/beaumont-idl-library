;+
; NAME:
;  KDIST
;
; PURPOSE:
;  This function calculates the near and far kinematic distances for a
;  given galactic longitude and radial velocity. The calculation uses
;  the Galactic rotaion model of Brand and Blitz 1993, A&A, 275 : 67.
;
; CATEGORY:
;  coordinate systems
;
; CALLING SEQUENCE:
;  result=KDIST( longitude, velocity, [/RADIANS])
;
; INPUTS:
;  longitude: Galactic Longitude. Currently must be in the range [-180,
;  180] in degrees.
;
;  velocity: Radial velocity in km/s
;
; KEYWORD PARAMETERS:
;  RADIANS: If set, input Longitude is in radians
;  DEBUG: If set, produce debugging plots / information
;
; OUTPUTS:
;  The two element vector [near_distance, far_distance] in kpc.
;
; RESTRICTIONS:
;  Currently only computes distances for objects in the inner galaxy.
;
; MODIFICATION HISTORY:
;  Written by: Chris Beaumont, June 2008.
;  June 23, 2008: Changed name from kinematic_distance to kdist. cnb
;  June 23, 2008: Fixed bug in modding l with 2 pi. cnb
;  July 17, 2008: Removed degrees keyword. Added radians. cnb
;  July 17, 2008: Changed things so that, if l is a variable,
;  it isn't modified
;  March 18, 2009: Changed theta_0 and v_sol to reflect arxiv
;  0902.3913
;  March 20, 2009: Changed minor typo in value of a1. Added /DEBUG keyword
;-
FUNCTION kdist, longitude, velocity, radians=radians, debug = debug
compile_opt idl2
on_error, 2

if n_params() ne 2 then begin
    message,'Calling Sequence: dist=kdist(l,v,[/radians])'
endif

if ~keyword_set(radians) then l=longitude/!radeg else l=longitude
l=(l mod (2*!dpi))
if(l lt 0) then l += 2 * !dpi

if (l ge !pi/2) && (l lt 3*!pi/2) then message, 'Error -- Longitude must be acute'

;- notation:
; v : rotation velocity
; vo: Solar rotation velocity
; vr: Radial velocity wrt LSR
; r : Galactocentric radius
; ro: Solar galactocentric radius

;rotation curve parameters from Brand and Blitz 1993
; v/vo = a1 * (R/Ro)^a2 + a3
; vr = sin(l) * vo * [ a1 * (r/ro)^(a2-1) + a3*(r/ro)^-1 - 1]

a1=1.0077D
a2=.0394D
a3=.0077D

;- the following are from arxiv 0902.3913 (VLBI parallax)
;- value taken from section 4, the best fit to this particular roation curve
ro=8.8
vo=275.
;ro = 8.5 ; - Brand Blitz value
;vo = 225

;determine r
r = (findgen(2000) + 1) / 2000. * 2 * ro
root=sin(l) * vo * (a1 * (r/ro)^(a2-1) + a3 * (r/ro)^(-1) - 1) - velocity
backup = root

;find the zero crossing
root*=shift(root,1)
root[0]=1

root[n_elements(root)-1]=1

hit=where(root lt 0, ct)
if keyword_set(debug) then begin
   plot, r, backup, xtitle = 'Galactocentric Distance (kpc)', $
         ytitle = 'Relative radial velocity (galaxy - object)', $
         charsize = 1.8, yra = [-20, 20]
endif

if ct ne 1 then begin
    message, 'Cannot determine galactocentric distance'
endif
r=r[hit[0]]

warnmsg = 'Warning: The galactocentric distance extrapolates the measured galactic roation curve'
if (r lt 2 || r gt 17) then $
   message, /continue, warnmsg

if keyword_set(debug) then begin
   oplot, r * [1,1], [-20, 20], color = fsc_color('crimson')
endif

rmin=ro*cos(l)
dr=sqrt(r^2-(ro*sin(l))^2)
if ~finite(dr) then $
   message, 'motion cannot be reporduced via galactic rotation'

return,[rmin-dr,rmin+dr]

end
