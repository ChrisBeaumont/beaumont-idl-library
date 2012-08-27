;+
; NAME:
;  DENSITYMAP
;
; PURPOSE:
;  This procedure creates a smoothed map of the suface density of
;  objects on the sky or in a plane.
;
; CATEGORY:
;  image processing, catalog processing
;
; CALLING SEQUENCE:
;  DENSITYMAP, x, y, num, map, errmap, head, [cdelt = cdelt, out =
;  out, /GALACTIC, VERBOSE = verbose, /ROBUST, /DEBUG, /CARTESIAN]
;
; INPUTS:
;    x: Vector of x-coordinate positions for a group of objects, in degrees
;    y: Corresponding y-coordinate positions for these
;       objects, in degrees.
;  num: The number of objects to use when smoothing. See PROCEDURE
;       section for details.
;  map: A named variable to hold the density map
; errmap: A named variable to hold the error map. This is just map /
;         sqrt(num - 1)
; head: A named variable to hold the fits header describing the map
;
; OUTPUTS:
;  map, errmap, and head are updated with the correct information.
;
; KEYWORD PARAMETERS:
;  cdelt: The pixel size of the smoothed map, in degrees /
;         pixel. Either a scalar or two element array. The default
;         pixel size is roughly 1/2 the mean input point spacings.
;  crval: The output map center. The default is the middle of the
;         input data. A two element array.
;  naxis:  The output map size, in pixels. The default is the smallest
;         necessary to overlap all input data, given the values for
;         cdelt and crval. A scalar or two element array.
;    out: A name used when writing out files. The map and error map
;         will be written to the file out_map.fits and out_err.fits
;  galactic: If set, then the input is assumed to be in galactic
;            coordinates.
;  verbose: If set, this procedure will print information as it
;           works. Set to a value 1-4 to control the amount of output
;           produced.
;  robust: If set, the distances between objects will be computed
;          using rigorous spherical trigonometry. This will be MUCH slower, as
;          this appoach is not vectorized well. The default behavior is to
;          treat the coordinates as cartesian, which is adequate for small
;          fields.
;   debug: Keyword which runs both robust and non-robust methods, and
;          reports for how many pixels the non-robust method gets the
;          wrong result.
;   cartesian: By default, the procedure assumes that the input
;          coordinates are on the sky. Set the CARTESIAN keyword to treat the
;          coordinates as on a flat plane.
;
; PROCEDURE:
;  The method is taken from section 3.5 of Gutermuth et al
;  2005ApJ...632..397G. For each pixel, the procedure finds
;  the distance r to the (num)'th closest object. The surface
;  density of that pixel is num / (pi r^2).
;
; EXAMPLE:
;  x = randomn(seed, 500)
;  y = randomn(seed, 500)
;  num = 10
;  DENSITYMAP, x, y, num, map, errmap, head, /verbose, /cartesian
;  tvscl, congrid(map, 500, 500)
;
; MODIFICATION HISTORY:
;  Written by: Chris Beaumont, December 2008
;  May 11 2009: Added CARTESIAN keyword. cnb.
;  June 15 2009: Updated things so that VERBIAGE handles text
;                output. cnb.
;  June 15 2009: Fixed some bugs related to the CARTESIAN
;                keyword. cnb.
;  June 15 2009: Enhancements to cdelt. Added crval and naxis
;                keywords. cnb.
;  Oct  8 2009:  Fixed a bug which led to integer overflow when
;                manipulating naxis1, naxis2 keywords. cnb
;-
PRO densitymap, x, y, num, map, errmap, head, $
                crval = crval, naxis = naxis, cdelt = cdelt, $
                galactic = galactic, cartesian = cartesian, $
                verbose = verbose, robust = robust, $
                debug = debug, out = out

compile_opt idl2
;on_error, 2

dbl_radeg = 180D / (!dpi)
dbl_dtor = (!dpi) / 180D

;-check for proper input
if n_params() ne 6 then begin
    print, 'DENSITYMAP calling sequence: '
    print, 'densitymap, x, y, n, map, errmap, head, [cdelt = cdelt, crval = crval, naxis = naxis,'
    print, '            /clip, /galactic, verbose = verbose, /debug, /robust, /cartesian, out = out]'
    return
endif

sz = n_elements(x)
if n_elements(y) ne sz  then message, 'x and y variables must be the same size'

;- output map coordinate type
if keyword_set(CARTESIAN) then begin
   ctype1 = 'X'
   ctype2= 'Y'
endif else if keyword_set(GALACTIC) then begin
    ctype1 = 'GLON-CAR'
    ctype2 = 'GLAT-CAR'
endif else begin
    ctype1 = 'RA---CAR'
    ctype2 = 'DEC--CAR'
endelse

;- output map center
xra = minmax(x)
yra = minmax(y)
if keyword_set(crval) then begin
   nval = n_elements(crval)
   if nval ne 2 then message, 'crval must be a 2 element vector'
   crval1 = crval[0]
   crval2 = crval[1]
endif else begin
   ;- All measurements in DEGREES
   crval1 = mean(x)
   crval2 = mean(y)
endelse

;- output map pixel scale
sphere_fudge =  keyword_set(cartesian) ? 1 : cos(crval2 * dbl_dtor)
if ~keyword_set(cdelt) then begin
   ;-default: ~10 pixels per input point
   area = (yra[1] - yra[0]) * (xra[1] - xra[0]) * sphere_fudge
   aper = sqrt(area * num / (sz * !pi))
   cdelt1 = -aper / 2
   cdelt2 = aper / 2
   cdelt = cdelt2
   if keyword_set(cartesian) then cdelt1 *= -1
endif else begin
   ndelt = n_elements(cdelt)
   if ndelt ne 1 && ndelt ne 2 then $
      message, 'cdelt must be a scalar or 2-element array'
   cdelt1 = ndelt eq 1 ? -abs(cdelt) : cdelt[0]
   cdelt2 = ndelt eq 1 ?  abs(cdelt) : cdelt[1]
   if ndelt eq 1 && keyword_set(cartesian) then cdelt1 *= -1 ;- no need to mirror flip
endelse

;- output map image size
if keyword_set(naxis) then begin
   numax = n_elements(naxis)
   if numax ne 1 && numax ne 2 then $
      message, 'naxis must be a scalar or 2-element array'
   naxis1 = long(numax eq 1 ? naxis : naxis[0])
   naxis2 = long(numax eq 1 ? naxis : naxis[1])
endif else begin
   ;- default: cover all the input points
   xp = abs((x - crval1) / cdelt1)
   yp = abs((y - crval2) / cdelt2)
   np = max([ceil(xp), ceil(yp)], /nan)
   naxis1 = long(2. * np)
   naxis2 = long(2. * np)
endelse

crpix1 = (naxis1 + 1) / 2.
crpix2 = (naxis2 + 1) / 2.

;-create image and header
mkhdr, head, 4, [naxis1, naxis2]
sxaddpar, head, 'CTYPE1', ctype1
sxaddpar, head, 'CTYPE2', ctype2
sxaddpar, head, 'CRVAL1', crval1, 'DEGREES'
sxaddpar, head, 'CRVAL2', crval2, 'DEGREES'
sxaddpar, head, 'CRPIX1', crpix1, '1-based'
sxaddpar, head, 'CRPIX2', crpix2, '1-based'
sxaddpar, head, 'CDELT1', cdelt1, 'DEGREES / PIXEL'
sxaddpar, head, 'CDELT2', cdelt2, 'DEGREES / PIXEL'
sxaddpar, head, 'BUNIT','Sources / square degree'

;-calculate nearest neighbors
imx = reform( rebin(     indgen(naxis1), naxis1, naxis2), naxis1 * naxis2, /over)
imy = reform( rebin( 1 # indgen(naxis2), naxis1, naxis2), naxis1 * naxis2, /over)
xyad, head, imx, imy, a, d
imx = a
imy = d
;imx = (imx + 1 - crpix1) * cdelt1 + crval1
;imy = (imy + 1 - crpix2) * cdelt2 + crval2

;- non-robust (but fast) calculation
if not (keyword_set(robust) && ~keyword_set(debug)) then begin
   verbiage, 'Calculating density map on '+systime(), 2, verbose
   neighbors = nearestN(transpose([[imx],[imy]]), transpose([[x],[y]]), $
                         num, verbose = verbose)

    ;- calculate distances
    if keyword_set(cartesian) then begin
       dis = sqrt((imx - x[neighbors])^2 + (imy - y[neighbors])^2)
    endif else begin
       gcirc, 2, imx, imy, x[neighbors], y[neighbors], dis
    endelse

endif

;-calculate the map robustly
if keyword_set(debug) || keyword_set(robust) then begin
   verbiage, 'Calculating map robustly on '+systime(), 2, verbose
   backup = lonarr(naxis1 * naxis2)

   ;- find nearest neighbors
   for i = 0L, naxis1 * naxis2 - 1, 1 do begin
       if keyword_set(cartesian) then begin
          dis2 = (imx[i] - x)^2 + (imy[i] - y)^2
       endif else gcirc, 2, imx[i], imy[i], x, y, dis2
        backup[i] = (sort(dis2))[num]
     endfor

   ;- are we debugging?
    if keyword_set(debug) then begin
        bad = where(neighbors ne backup, ct)
        verbiage, 'Number of failures in nearestN: '+strtrim(ct,2), 1, 1
     endif

    ;- nearest neighbors -> density
    if keyword_set(cartesian) then dis = sqrt((imx - x[backup])^2 + (imy - y[backup])^2) $
    else gcirc, 2, imx, imy, x[backup], y[backup], dis
endif

verbiage, 'Map calculation finished on '+systime(), 2, verbose

;- make map
if ~keyword_set(cartesian) then dis /= 3600D
map  = reform(1.0D * num / (!dpi * dis ^ 2), naxis1, naxis2)
errmap = map / sqrt(num - 1)

return

end
