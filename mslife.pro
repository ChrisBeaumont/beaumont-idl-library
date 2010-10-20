;+
; PURPOSE:
;  This function returns the approximate main sequence lifetime as a
;  function of stellar mass. It is valid only for stars >~ 1 Msolar
;
; CATEGORY:
;  astrophysics
; 
; INPUTS:
;  mass: Stellar masses, in units of a solar mass. Scalar or vector
;
; OUTPUTS:
;  lifetime(mass), in years
;
; PROCEDURE:
;  The equation used is t = 10^10 * M^-2.9
;
; MODIFICATION HISTORY:
;  November 2009: Written by Chris Beaumont
;-
function mslife, mass
  if n_params() ne 1 then begin
     print, 'calling sequence:'
     print, ' lifetime = mslife(mass)'
     print, ' lifetime in years. mass in solar masses'
     return, !values.f_nan
  endif

  return, 1d10 * mass^(-2.9)
end
