;+
; NAME:
;   GCROT
;
; PURPOSE:
;   This function computes, as a function of galactic coordinates, the
;   angle between a vector pointing towards the galactic north pole
;   and a vector pointing towards the North Celestial Pole. This can
;   be used to orient images aligned to ecliptic and galactic
;   coordinate systems.
;
; CATEGORY:
;  coordinate systems
;
; CALLING SEQUENCE:
;   result=GCROT(L,B,/DEGREE)
;
; INPUTS:
;   L:   Galactic longitude. Radians are assumed unless /DEGREE is set.
;   B:   Galactic latitude. Radians are assumed unless /DEGREE is set.
;
; KEYWORD PARAMETERS:
;   DEGREE: If set, the input and output are given in degrees.
;   
; OUTPUTS:
;   The clockwise rotation that aligns the galactic north pole with
;   the NCP (in J2000 coordinates). In radians by default.
;
; RESTRICTIONS:
;   This has not been tested for accuracy higher than roughly .5
;   degrees. I wouldn't use this to determine the orientation of spectra
;   slits or other high precision tasks without testing first.
;
; PROCEDURE:
;  Uses a spherical trig identity taken from wikipedia.org/wiki/Great-circle_distance
;  
; MODIFICATION HISTORY:
;   Written by:  Chris Beaumont, May 2008.
;   June 2008    Added /degree keyword. cnb
;-   
FUNCTION gcrot,l,b,degree=degree

;input checking
if n_params() lt 2 then begin
    message,'CALLING SEQUENCE: result=gcrot(L,B,[/DEGREE])',/continue
    return,0
endif

l=float(l)
b=float(b)

if keyword_set(degree) then begin
    l/=!radeg
    b/=!radeg
endif
    
;location of NCP
ncp_l=122.93194*!pi/180.
ncp_b=27.128405*!pi/180.

;next part refers to a spherical triangle's sides across points p (image), q (NGP), r(NCP)
p=!pi/2.-ncp_b
q=acos(cos(b)*cos(ncp_b)*cos(ncp_l-l)+sin(b)*sin(ncp_b)) ;wikipedia.org/wiki/Great-circle_distance
r=!pi/2.-b

result=acos((cos(p)-cos(q)*cos(r))/(sin(q)*sin(r)))

;Fix issues with arcos domain

hit=where(l ge ncp_l and l le ncp_l+!pi,ct)
if ct ne 0 then result[hit]*=-1.

if keyword_set(degree) then result*=!radeg

return,result

end
