;+
; NAME:
;  NICER
;
; PURPOSE:
;  This function estimates the amount of reddening a star is seen
;  through, based on its observed colors. 
;
; INPUTS:
;  mag: An NBAND by NOBJECT array, or an NBAND vector. Each row of the
;       array represents an object, and the columns in that array list
;       the magnitudes of that object in NBAND different photometric
;       bands. If NBAND = 3, the colors are assumed to be (J,H,Ks). If
;       NBAND = 5, the colors are assumed to be (J,H,Ks,I1,I2). In
;       general, any set of magnitudes is allowed, as long as the
;       appropriate keyword variables are passed (see below).
;
;  dmag: The uncertainty on each magnitude measurement.
;
; KEYWORD PARAMETERS:
;  color: A vector of the mean color of (unreddened) stars. An NBAND-1
;         vector. Each entry refers to the mean color formed by taking
;         mag_i - mag_i+1. For example, if NBAND = 3, the assumed
;         magnitudes are JHK, and the colors are J-H and H-K. Default
;         values are provided for (JHK) and (JHK I1 I2) if NBAND = 3
;         or 5. Otherwise, this vector must be provided. 
;
;  covar: An NBAND-1 by NBAND-1 matrix, specifying the covariance of
;         colors of unreddened stars. This is provided for (JHK) and
;         (JHK I1 I2) if NBAND = 3 or 5. Otherwise, this matrix must
;         be provided.
;
;     ks: The reddening vector for each color. That is,
;         c_i(observed) = c_i(true) + Av * k. This is provided for
;         (JHK) and (JHK I1 I2) when NBAND = 3 or 5. Otherwise, this
;         vector must be provided.
;
; PROCEDURE:
;  Algorithm taken from Lombardi and Alves 2001. Equation references
;  in code refer to that paper. 
;  JHK extinction law taken from Lebofsky 1985
;  IRAC extinction law taken from Indebetouw, Mathis et al. 2005
;
; CATEGORY:
;  astrophysics
;
; MODIFICATION HISTORY:
;  Written by: Chris Beaumont July 2008
;  November 2008: cnb. Updated to incorporate IRAC bands 1 and 2
;  November 2009: cnb. Completely re-written to better generalize to
;                      different photometric bands. Calling sequence changed.
;- 
function nicer, mag, dmag, $
                     color = color, covar = covar, ks = ks

  compile_opt idl2

  ;- check inputs
  if n_params() ne 2 then begin
     print, 'NICER calling sequence: '
     print, 'Av = NICER(mag, dmag, [color = color, covar = covar, ks = ks)'
     return, !values.f_nan
  endif
  
  sz = size(mag)
  ndimen = sz[0]
  nband = sz[1]
  nobj = ndimen eq 1 ? 1 : sz[2]
  
  ;- make sure mag and dmag look good
  if nband lt 2 then message, 'must provide at least 2 colors'
  if ndimen gt 2 then $
     message, 'mag and dmag must be 2D matrices'

  sz = size(dmag)
  nd = sz[0]
  nb = sz[1]
  nob = nd eq 1 ? 1 : sz[2]
  
  if nb ne nband || nob ne nobj then $
     message, 'mag and dmag are not the same shape'
  
  ;- make sure color looks good
  if keyword_set(color) then begin
     sz = size(color)
     nd = sz[0]
     if nd ne 1 then message, 'color must be a 1D vector'
     if sz[1] ne nband-1 then message, 'color vector not the correct length'
  endif else begin
     ;- Default values for intrinsic colors
     ;- 2mass from j swift control field, IRAC from taurus field
     color = [.487419, .134643, .0556, .01707] ;j-h, h-ks, ks-i1, i1-i2
     if nband eq 3 then color = color[0:1] $ ;- assume mags are jhk
     else if nband eq 5 then color = color $ ;- assume mags are jhk i1 i2
     else message, 'Color vector not provided, and is required'
  endelse     

  ;- make sure covar looks good.
  if keyword_set(covar) then begin
     sz = size(covar)
     nd = sz[0]
     if nd ne 2 then message, 'covar must be a 2D matrix'
     if sz[1] ne (nband-1) || sz[2] ne (nband-1) then message, 'covar matrix not the correct size'
  endif else begin       
     ;- values for the intrinsic color scatter
     ;- the default values use a control field from taurus for irac
     ;- colors, and the field from js for 2mass colors
     covar = [ [0.0280821,    0.00293,      0.00595462, -0.00233027],$
               [0.00293,      0.02765,      0.00320190,  0.000177188],$
               [0.00595462,   0.00320190,   0.00595323,  0.000593445],$
               [-0.00233027,  0.000177188,  0.000593445, 0.0257013] ]

     if nband eq 3 then covar = covar[0:1, 0:1] $ ;- assume mags are jhk
     else if nband eq 5 then covar = covar      $ ;- assume mags are jhk12
     else message, 'covar vector is required'
  endelse
  
  ;- use default reddening vectors, if not provided.
  if ~keyword_set(ks) then begin
     ;- k=E/Av. (NOTE: E= (A1 - A2) by definition)
     ;- 2MASS values from Rieke and Lebofsky 1985
     ;- IRAC values from Indebetouw, Mathis et al. 2005 (Table 1, using
     ;- Ak/Av = .112 from Rieke and Lebofsky 1985)
     k = dblarr(4)
     k[0] = 0.106               ;- J-H
     k[1] = 0.063               ;- H-K
     k[2] = 0.049               ;- K-1
     k[3] = 0.0146              ;- 1-2
     if nband eq 3 then ks = k[0:1] $
     else if nband eq 5 then ks = k $
     else message, 'must supply reddening vectors'
  endif else if n_elements(ks) ne nband -1 then message, 'ks variable not the correct size'
     
  ;- calculate the color measurement covariance matrix (see eq 10)
  cerr = dblarr(nband-1, nband-1, nobj)
  for i = 0, nband - 2, 1 do begin
     if i ne 0 then cerr[i-1, i, *] = -dmag[i,*]^2
     cerr[i, i, *] = dmag[i,*]^2 + dmag[i+1, *]^2
     if i ne (nband - 2) then cerr[i+1, i, *] = -dmag[i+1, *]^2
  endfor
  cov = cerr + rebin(covar, nband -1, nband - 1, nobj)

  m_color = fltarr(nband - 1, nobj)
  for i = 0, nband - 2, 1 do m_color[i,*] = mag[i,*] - mag[i+1, *]

  ;- find Av and sigma_av from equations 12-13
  av = dblarr(nobj)
  sv = dblarr(nobj)
  for i = 0L, nobj - 1, 1 do begin
     ci = invert(cov[*,*,i], stat, /double)
     b = (ci ## ks) / ((ks ## (ci ## ks))[0])
     av[i] = total(b * (m_color[*,i] - color))
     sv[i] = sqrt(transpose(b) ## (cov[*,*,i] ## b))
  endfor
  
  ;- return the result
  return,transpose([[av],[sv]]) 
end
