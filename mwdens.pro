;+
; PURPOSE:
;  This function returns the number density of stars in the Milky Way,
;  as a function of galactic coordinates. It models a circularly
;  symmetric thin and thick disk. The parameters describing these
;  disks come from Bochanski et al. 2009.
;
; CATEGORY:
;  Astrophysics
;
; CALLING SEQUENCE:
;  result = mwdens(l, b, d)
;
; INPUTS:
;  l: The galactic longitude. Scalar or vector in DEGREES
;  b: The galatic latitude. Scalar or vector in DEGREES.
;  d: The distance from the sun. Scalar or vector in parsecs.
;
; OUTPUTS:
;  The number density of stars as a function of (l,b,d). This has been
;  approximately normalized, but the normalization factor may not be
;  very accurate (n = .048 stars pc^-3 at the sun)
;
; MODIFICATION HISTORY:
;  Sep 2009: Written by Chris Beaumont.
;  Oct 2009: Galaxy parameters updated. Calling
;            sequence changed. cnb.
;  Nov 2009: Added normalization constant. cnb.
;  Dec 2009: Removed galaxy truncation
;-
function mwdens, l,b,d, frac = frac

  compile_opt idl2
  on_error, 2

  ;- check inputs
  if n_params() ne 3 then begin
     print, 'mwdens calling sequence'
     print, ' result = mwdens(l, b, d)'
     print, '          d in parsecs'
     return, !values.f_nan
  endif

  nl = n_elements(l)
  nb = n_elements(b)
  nd = n_elements(d)
  if nl ne nb || nl ne nd then $
     message, 'l, b, and d must contain the same number of elements'

  ;- parameters describing the Galaxy
  ;- Table 6 from Bochanski et al. 2009.
  ;- Halo parameters from Reid 1993
  zthin = 300D
  rthin = 3100D
  zthick = 1300D
  rthick = 3100D
  fthick = .06D
  fhalo = .002D
  rhalo = 1000D
  n = 3D
  rsun = 8500D
  zsun = 15D
  rmax = 1D5
  
  ;- parameters describing the bulge. Different coordinate system!
  ;- taken from Dwek 95, using Besancon model parameters
  ;XXX bulge is wrong! must get coordinate rotations correct
  nbulge = 13.7
  x0 = 1590D
  y0 = 424D
  z0 = 424D
  rc = 2540D
  x = -d * cos(b * !dtor) * sin(l * !dtor)
  y = -rsun + d * cos(b * !dtor) * cos(l * !dtor)
  z = d * sin(b * !dtor)
  rs = (((x / x0)^2 + (y / y0)^2)^2 + (z / z0)^4)^(.25)
  r1 = sqrt(x^2 + y^2)
  bulge = exp(-0.5 * rs^2) * (r1 lt rc) + $
          exp(-0.5 * rs^2) * exp(-0.5 * ((r1 - rc) / .5)^2) * (r1 gt rc)
  bulge *= nbulge * 0

  ;- local density of stars near sun. Estimated by integrating
  ;- the Bochanski 2009 LF
  ;- divide by 1.2 b/c that is un-normalized density at d=0
  norm = .0488 / 1.2

  ;- convert (l,b,d) to (R, Z)
  z = zsun + d * sin(b * !dtor)
  x = rsun - d * cos(b * !dtor) * cos(l * !dtor)
  y = d * cos(b * !dtor) * sin(l * !dtor)
  r = sqrt(x^2 + y^2)
  r_sph = sqrt(x^2 + y^2 + z^2)

  ;- calculate the density
  pthin =  exp(-(r - rsun) / rthin) *  exp(-(abs(z) - zsun) / zthin)
  pthick = exp(-(r - rsun) / rthick) * exp(-(abs(z) - zsun) / zthick)
  phalo = fhalo * (rhalo^n + rsun^n) / (rhalo^n + r_sph^n)
  inside = r_sph lt rmax

  frac = transpose([[bulge], $
          [(1-fthick) * pthin * norm], $
          [fthick * pthick * norm], $
          [phalo * norm]])
  frac /= rebin(1 # total(frac, 1), 4, nl)
  
  return, bulge + norm * ((1 - fthick) * pthin + fthick * pthick + phalo); * $
          ;(r_sph lt rmax)
end
