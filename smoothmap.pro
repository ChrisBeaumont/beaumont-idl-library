;+
; WARNING:
;  This function has been deprecated, and replaced by SKYMAP_SMOOTH. I
;  recommend using that function instead of this one.
;
; NAME:
;  SMOOTHMAP
;
; PURPOSE:
;  This procuedure creates a smoothed map of some quantity sampled,
;  with uncertainty, at irregualr points across the sky. It uses a
;  gaussian smoothing filter to average nearby data points and improve
;  noise statistics. The algorithm is taken from Lombardi and Alves
;  2001. This implementation was written to generate Av maps from the
;  output of the NICER algorithm (also described in Lombardi and Alves
;  2001, and implemented in the procedure NICER.pro)
;
; CALLING SEQUENCE:
;  SMOOTHMAP, val, err, x, y, map, errmap, ctmap, head, 
;             [cdelt  = cdelt, fwhm = fwhm, out = out, /CLIP,
;             /GALACTIC, /CARTESIAN, /VERBOSE, /ITERATE]
;
; INPUTS:
;  val: The sampled values to smooth over
;  err: The uncertainties on the sampled errors (sqrt(variance))
;    x: The x position of the sampled values (treated as a spherical
;       coordinate in DEGREES)
;    y: The y position of the sampled values (treated as a spherical
;       coordiante in DEGREES)
;; 
; KEYWORD PARAMETERS:
;     cdelt: A scalar defining the pixel size of the map in
;            DEGREES. If not defined, cdelt will be set to fwhm / 2.
;      fwhm: The FWHM of the smoothing kernel, in DEGREES. If not
;            supplied, the fwhm will be set so that, on average, a 1 fwhm
;            diameter circle surrounds 50 datapoints.
;       out: Write a FITS file of the results. The output files will be
;            named out_map.fits, out_err.fits, and out_ct.fits.
;      CLIP: If set, the procedure will iterate and reject outlier data
;            points. 
;  GALACTIC: If set, the coordinates are assumed to be in galactic
;            coordinates. This does not affect the map calculation, but does
;            impact the header file. If not set, the FK5 system is
;            assumed.
;  CARTESIAN: If set, the input and output are in a cartesian
;             (non-spherical) coordinate system
;   VERBOSE: If set, information about the procedure is printed to the terminal.
;   ITERATE: Used internally for the sigma clipping process. Do not set
;            this manually.
; OPTIONAL OUTPUTS:
;     map: A variable to hold the smoothed map
;  errmap: A variable to hold the error (sqrt(variance)) on the smoothed map
;   ctmap: A variable to hold the map giving the number of sources
;          which went into the calculation of each pixel
;    head: A variable to hold the header of the map (A string array)
;
; RESTRICTIONS:
;  For pathological cases (sampled points straddle the poles or l=360
;  deg line), the map dimensions will be calculated incorrectly.
; 
; CATEGORY:
;  image processing
; 
; MODIFICATION HISTORY:
;  November 2008: Written by Chris Beaumont based on code by Jon
;                 Swift. Written to independently reproduce and 
;                 speed up that code.
;  December 1 2008: Fixed several bugs in error calculation. cnb.
;  December 4 2008: Fixed things so NANs in the input val variable 
;                   are ignored. cnb.
;  April 15 2009: Added CARTESIAN keyword   
;  March 2010: Deprecated and replaced by skymap_smooth. cnb.
;-
PRO smoothmap, val, err, x, y, map, errmap, ctmap, head, cdelt = cdelt, $
               fwhm = fwhm, out = out, clip = clip, galactic = galactic, $
               iterate = iterate, verbose = verbose, cartesian = cartesian

  compile_opt idl2
  ;on_error, 2

  dbl_radeg = 180D / (!dpi)
  dbl_dtor =  (!dpi) / 180D
  
  ;- if this is an iterative call, save the old maps
  if keyword_set(ITERATE) then begin
     oldmap = map
     olderr = errmap
     oldctmap = ctmap
  endif
  
  ;- Check for proper input
  if n_params() ne 8 then begin
     print, 'SMOOTHMAP calling sequence: '
     print, 'smoothmap, val, err, x, y, map, errmap, ctmap, head,'
     print, '           [cdelt = cdelt, fwhm = fwhm, out = out, clip = clip, '
     print, '            /galactic, /cartesian, /verbose]'
     return
  endif

  catch, theError
  if (theError ne 0) then begin
     catch,/cancel
     print,'Error raised in smoothmap. Aborting.'
     print, !error_state.msg
     if obj_valid(report) then obj_destroy, report
     return
  endif
  
  sz = n_elements(val)
  if n_elements(err) ne sz || n_elements(x) ne sz || n_elements(y) ne sz then $
     message, 'Input val, err, x, and y variables must have the same number of elements'
  
  ;- set up output map.
  if keyword_set(CARTESIAN) then begin
     ctype1 = 'X'
     ctype2 = 'Y'
  endif else if keyword_set(GALACTIC) then begin
     ctype1 = 'GLON-CAR'
     ctype2 = 'GLAT-CAR'
  endif else begin
     ctype1 = 'RA-CAR'
     ctype2 = 'DEC-CAR'
  endelse
  
  xra = minmax(x)
  yra = minmax(y)
  
  ;- All measurements in DEGREES
  crval1 = mean(xra)
  crval2 = mean(yra)
  
  ;- default to 50 sources per 1 fwhm diameter circle
  if ~keyword_set(fwhm) then begin
     objPerCell = 50
     area = (yra[1] - yra[0]) * (xra[1] - xra[0])
     if ~keyword_set(cartesian) then area *= cos(crval2 * dbl_dtor)
     fwhm = sqrt(area * objPerCell / (sz * !pi)) * 2.0D
  endif
  
  ;- default to 2 pixels per fwhm
  if ~keyword_set(cdelt) then begin
     cdelt1 = -fwhm / 2
     cdelt2 = fwhm / 2
     cdelt = cdelt2
  endif else begin
     cdelt1 = -abs(cdelt)
     cdelt2 =  abs(cdelt)
  endelse
  
  if keyword_set(cartesian) then begin
     naxis1 = ceil( range(xra) / cdelt2)
     naxis2 = ceil( range(yra) / cdelt2)
  endif else begin
     naxis1 = ceil((xra[1] - xra[0]) * cos(crval2 * dbl_dtor) / cdelt2)
     naxis2 = ceil((yra[1] - yra[0]) / cdelt2)
  endelse
  
  crpix1 = (naxis1 + 1) / 2.
  crpix2 = (naxis2 + 1) / 2.
  
  mkhdr, head, 4, [naxis1, naxis2]
  sxaddpar, head, 'CTYPE1', ctype1
  sxaddpar, head, 'CTYPE2', ctype2
  sxaddpar, head, 'CRVAL1', crval1, 'DEGREES'
  sxaddpar, head, 'CRVAL2', crval2, 'DEGREES'
  sxaddpar, head, 'CRPIX1', crpix1, '1-based'
  sxaddpar, head, 'CRPIX2', crpix2, '1-based'
  sxaddpar, head, 'CDELT1', cdelt1, 'DEGREES / PIXEL'
  sxaddpar, head, 'CDELT2', cdelt2, 'DEGREES / PIXEL'
  sxaddpar, head, 'BMAJ', fwhm, 'fwhm in DEGREES'
  sxaddpar, head, 'BMIN', fwhm, 'fwhm in DEGREES'
  
  map    = dblarr(naxis1, naxis2)
  errmap = dblarr(naxis1, naxis2)
  wmap   = dblarr(naxis1, naxis2)
  w2map  = dblarr(naxis1, naxis2)
  ctmap  = dblarr(naxis1, naxis2)
  
  if KEYWORD_SET(VERBOSE) && ~KEYWORD_SET(ITERATE) then begin
     print, format = "('Smoothing kernel fwhm (arcmin): ', e9.1)", fwhm * 60
     print, format = "('  Mapsize (pixels) / (degrees): ', i5, i5, ' / ', e9.1, e9.1)", $
            naxis1, naxis2, naxis1 * cdelt, naxis2 * cdelt
  endif
  
  ;- loop over the stars to calculate the maps
  ;- an npix by npix box around a source should cover >5sigma in angular distance
  sigma = fwhm / (2 * sqrt(2 * alog(2)))

  ;- worst case warping of x axis away from the equator
  warp = keyword_set(cartesian) ? 1 : min(cos(y * dbl_dtor)) 
  npix = ceil(5 * sigma / (cdelt2 * warp)) 
  pix = dindgen(npix) - (npix -1) / 2.
  
  stampxpix = rebin(pix, npix, npix)
  stampypix = rebin(1#pix, npix, npix)
  stamprad = sqrt(stampxpix^2 + stampypix^2)
  
  stampxsky = stampxpix * cdelt1
  stampysky = stampypix * cdelt2
  
  ;- pixel locations of each source. 0 indexed
  x_pix = (x - crval1) / cdelt1 + crpix1 - 1
  y_pix = (y - crval2) / cdelt2 + crpix2 - 1
  
  if keyword_set(VERBOSE) then begin
     print, ''
     print, 'Beginning map calculation on '+systime()
     report = obj_new('looplister', sz-1, 30)
  endif

  for i = 0, sz - 1, 1 do begin
     
     if ~finite(val[i]) || ~finite(err[i]) then continue
     if keyword_set(verbose) then report->report, i
     
     ;- if we are clipping, check to see if we should reject the point
     if KEYWORD_SET(ITERATE) then begin
        xloc = 0 > x_pix[i] < (naxis1 - 1)
        yloc = 0 > y_pix[i] < (naxis2 - 1)
        if abs((val[i] - oldmap[xloc,yloc]))^2 / $
           ((olderr[xloc,yloc]^2 + err[i]^2) > .01) gt 3 then continue
     endif
     
     ;- calculate the weights for a postage stamp around the ith source
     xstamp = 0 > round(stampxpix + x_pix[i]) < (naxis1 - 1)
     ystamp = 0 > round(stampypix + y_pix[i]) < (naxis2 - 1)
     if keyword_set(cartesian) then begin
        dis = stamprad
        weight = (err[i])^(-2) * exp(-(dis)^2 / (2 * sigma^2))
        weight *= (dis le 3 * sigma)
     endif else begin
        gcirc, 0, x[i] * dbl_dtor, y[i] * dbl_dtor, $
               (x[i] + stampxsky) * dbl_dtor, (y[i] + stampysky) * dbl_dtor, dis
        weight = (err[i])^(-2) * exp(-(dis * dbl_radeg)^2 / (2 * sigma^2)) 
        weight *= ((dis * dbl_radeg) le (3 * sigma)) ;- truncate
     endelse
     
     ;- update maps
     map[xstamp, ystamp]    += weight * val[i]
     wmap[xstamp, ystamp]   += weight
     ctmap[xstamp, ystamp]  += weight gt 0
     errmap[xstamp, ystamp] += err[i]^2 * weight^2
     w2map[xstamp, ystamp]  += weight^2   
     
  endfor
  
  if KEYWORD_SET(VERBOSE) then begin
     print, 'Map calculation finished on  '+systime()
     print, ''
     obj_destroy, report
  endif
  
  ;- flag bad regions based on counts
  bad = where(ctmap le 5, bct, $
              complement=good, nc = gct)
  
  if bct ne 0 then begin 
     map[bad] = !values.f_nan
     errmap[bad] = !values.f_infinity
     ctmap[bad] = !values.f_nan
  endif
  
  ;- normalize maps
  if gct ne 0 then begin
     map[good] /= wmap[good]
     errmap[good] = sqrt(errmap[good] / w2map[good])
  endif else message, 'no stars in the countmap'
  
  ;-flag out edges
  sz = size(map)
  map[0,*]    = !values.f_nan
  map[*,0]    = !values.f_nan
  errmap[0,*] = !values.f_nan
  errmap[*,0] = !values.f_nan
  
  map[sz[1]-1, *]    = !values.f_nan
  map[*, sz[2]-1]    = !values.f_nan
  errmap[sz[1]-1, *] = !values.f_nan
  errmap[*,sz[2]-1]  = !values.f_nan
  
  ;- if we are iteratively sigma clipping, we are done
  if KEYWORD_SET(ITERATE) then return
  
  ;- iterate and converge
  if KEYWORD_SET(CLIP) then begin
     if KEYWORD_SET(VERBOSE) then print, 'Sigma clipping results'
     
     maxiter = 10
     numiter = 0
     tol = 0.3
     tiny = .1
     
     ;- iterate
     while 1 do begin
        if numiter ge maxiter then message, 'SMOOTHMAP not converging'
        oldmap = map
        olderr = errmap
        
                                ;- recalculate with clipping
        smoothmap, val, err, x, y, map, errmap, ctmap, head, $
                   cdelt = cdelt, fwhm = fwhm, /CLIP, /ITER, $
                   galactic = keyword_set(galactic), verbose = keyword_set(verbose), $
                   cartesian = keyword_set(cartesian)
        
                                ;-check for convergence
        good = where( finite(map) and finite(oldmap), ct)
        if ct eq 0 then message, 'smoothed map has no finite data'
        resid = abs(oldmap[good] - map[good]) / (errmap[good] > TINY) 
        if mean(resid) lt TOL then break else numiter++
        
        if KEYWORD_SET(VERBOSE) then $
           print, min(resid) / tol, mean(resid) / tol, max(resid) / tol, $
                  format='("Normalized Sigma clipping Residuals (min / mean / max)", e9.2, " / ", e9.2, " / ", e9.2)'
     endwhile
     
     if KEYWORD_SET(VERBOSE) then print, 'Sigma clipping has converged'
  endif
  
  ;- write out files
  if keyword_set(out) then begin
     writefits, out+'_map.fits', map, head
     writefits, out+'_err.fits', errmap, head
     writefits, out+'_ct.fits', ctmap, head
  endif
  
end
