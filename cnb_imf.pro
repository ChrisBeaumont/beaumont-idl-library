;+
; PURPOSE:
;  This function both evaluates the IMF at given mass, and returns
;  masses drawn at random from the IMF. The functional form of the IMF
;  is given by dN/dM ~ M^-alpha, where
;    alpha = 0    for   0 < M / Msolar < .07 (Allen et al 2005) 
;          = 1.05 for .07 < M / Msolar < .5  (Kroupa et al 2002 ASPC..285...86K)
;          = 2.35 for  .5 < M / Msolar       (Ibid)
;
; CATEGORY:
;  Astrophysics
;
; CALLING SEQUENCE:
;  result = cnb_imf([mass, /noturnover, random = random, muench = muench)
;
; INPUTS:
;  mass: A set of masses at which to evaluate the IMF. Given in solar
;        masses. If provided, the program returns imf(masses) as a
;        scalar or vector. This function is not normalized
;
; KEYWORD PARAMETERS:
;  NOTURNOVER: If set, treat the full IMF as having alpha = 2.35. This
;  cannot be used in conjunction with the RANDOM keyword
;
;  RANDOM: Set this to a number (nstars) to draw nstars from the IMF
;  mass distribution. If this keyword is set, then the function
;  returns the masses of the nstars drawn from the distribution. 
;
;  MUENCH: Use the first three terms of the power law imf given in
;          equation 1 of Muench et al. 2002 (ApJ 573)
;  MALPHA0: Manually specify the high mass value for alpha.
;
; OUTPUTS:
;  If mass is set, it returns a vector of imf(mass). If random is set,
;  it returns a vector of masses drawn from the IMF distribution.
;
; MODIFICATION HISTORY:
;  April 2009: Written by Chris Beaumont
;  May 2009: Added RANDOM keyword. cnb.
;  June 2009: Added MUENCH keyword. cnb.
;-
function cnb_imf, mass, noturnover = noturnover, random = random, $
              muench = muench, malpha0 = malpha0

  compile_opt idl2

  ;- check inputs
  doMass = n_elements(mass) ne 0
  doRandom = keyword_set(random)
  noTurn = keyword_set(noturnover)
  
  if ~doMass && ~doRandom then begin
     print, 'imf calling sequence:'
     print, 'result = cnb_imf([mass, /noturnover, random=random, muench = muench])'
     return, !values.f_nan
  endif
  
  if doRandom && doMass then message,$
     'Cannot set RANDOM and provide masses'
  
  if doRandom && noTurn then message, $
     'Cannot set both RANDOM and NOTURNOVER'
  
  if keyword_set(noturnover) then return, ms^(-2.35D)
  
  ;-imf
  if keyword_set(muench) then begin
     alpha = [.27, 1.15, 2.21] * (-1)
     m = [.025, .12, .6]
     if keyword_set(malpha0) then alpha[2] = malpha0 * (-1)
  endif else begin
     alpha = [ 0, 1.05, 2.35] * (-1)
     m =     [.00, .07,  .5]
  endelse
  
  
;- pull objects from the imf distribution   
  if doRandom then begin
     common imf_common, seed
     masses = findgen(1d5)/1d3
     imf = cnb_imf(masses, muench = muench, malpha0 = malpha0)
     cdf = total(imf, /cumul) / total(imf)
     rand = randomu(seed, random)
     result = interpol(masses, cdf, rand) 
     return, result
  endif
  
;- evaluate the power law
  return, pwpl(m, alpha, x = mass)

end

