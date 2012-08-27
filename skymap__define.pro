;+
; NAME:
;  SKYMAP
;
; PURPOSE:
;  This class creates a smoothed sky map from an irregularly spaced
;  set of measurements. The default smoothing kernel is a
;  gaussian. The class also implements sigma clipping.
;
; The smoothed map is evaluated according to
;  m(x,y) = sum( w_i v_i) / sum(w_i)
;
; where v_i is the ith sampled data point and w_i is a smoothing
; function. In this class, w_i is a gaussian centered on v_i, with a
; fwhm specified by the user.
;
; The variance map is given by
;  v(x,y) = sum(w_i^2 dv_i^2) / sum(w_i)^2
;
;  where dv_i^2 is the variance on measurement i
;
; METHODS:
;  weight: Implementation of the weighting kernel.
;  getMap: return the smoothed map
;  getIncluded: return a 1/0 bit vectors, listing which data points
;               were included/clipped.
;  writeFits: output the map to a fits file
;  makeMap: calculate the smoothed map
;  makeMapClip: Iteratively calculate map with sigma clipping
;  init: Create the object
;  cleanup: Destroy the object
;
; SUPERCLASSES:
;  none
;
; SUBCLASSES:
;  nicest: Overloads the weight method to implement Marco
;          Lombardi's NICEST algorithm.
;
;-

;+
; PURPOSE:
;  This function evaluates the gaussian smoothing kernel applied on
;  source "id" at location (x,y)
;
; INPUTS:
;  id: The index of the source to consider. scalar
;   x: The x location in sky coordinates, scalar or array
;   y: The y location in sky coordinates, scalar or array
;
; OUTPUTS:
;  w_id(x,y)
;
; MODIFICATION HISTORY:
;  December 2010: Written by Chris Beaumont
;  Aug 2011: Added support for euclidian (non-sky) maps. cnb.
;-
function skymap::weight, id, x, y
  dval = (*self.dval)[id]
  if self.euclidian then begin
     dis = sqrt(((*self.x)[id] - x)^2 + ((*self.y)[id] - y)^2)
  endif else begin
     gcirc, 2, (*self.x)[id], (*self.y)[id], x, y, dis
     dis /= 3600.
  endelse
  result = 1 / dval^2 * exp(-dis^2 / (2 * self.sigma^2)) * $
           (dis lt self.truncate)
  return, result
end

;+
; PURPOSE:
;  This function returns the map structure associated with the object.
;
; INPUTS:
;  none
;
; KEYWORD PARAMETERS:
;  variance: Set to hold the variance map
;
; OUTPUTS:
;  A map structure cotnaining 2 tags: a header and the image data
;-
function skymap::getMap, variance = variance
  if arg_present(variance) then variance = *self.emap
  return, *self.map
end


;+
; PURPOSE:
;  This function returns a 1/0 bit vector, listing whether each data
;  point was included/clipped when calculating the smoothed map.
;-
function skymap::getIncluded
  return, *self.included
end


;+
; PURPOSE:
;  Output the map data to a fits image
;
; INPUTS:
;  name: The file name to write to
;
; KEYWORD PARAMETERS:
;  An optional filename to write the variance data to.
;
;-
pro skymap::writeFits, name, varname = varname
  if n_params() ne 1 then begin
     print, 'calling sequence'
     print, 'skymap::writeFits, name, [varname = varname]'
     return
  endif
  if size(name, /tname) ne 'STRING' || ~(is_scalar(name)) then $
     message, 'Filename must be a scalar string'
  if keyword_set(varname) && (~is_scalar(varname) || $
                              size(varname, /tname) ne 'STRING') $
  then message, 'Varname must be a scalar string'

  writefits, name, (*self.map).map, (*self.map).head
  if keyword_set(varname) then $
     writefits, varname, (*self.emap).map, (*self.emap).head
end


;+
; PURPOSE:
;  This procedure iteratively computes the smooth map with sigma
;  clipping.
;
; DESCRIPTION:
;  makeMap is iteratively called and, at each iteration, each data
;  point v_i is compared to the map value map(x_i, y_i). If the two
;  values disagree by more than CLIP * sigma_map(x_i, y_i), the point
;  is rejected. The map is recalculated with valid points until
;  convergence.
;
; INPUTS:
;  clip: The outlier threshhold (see above). 3 is typical
;
; KEYWORD PARAMETERS:
;  lo: set to flag only the lo-valued outliers.
;  hi: set to flag only the hi-valued outliers.
;  tol: Specifiy a convergence tolerance. The clipping has converged
;       when fewer than TOL * n_object objects change their
;       inlier/outlier classification. Defaults to .01
;  max_reject: Set to the max fraction of objects that can be flagged
;              as outliers before failure. Defaults to .2
;  maxiter: The maximum number of clipping iterations before
;           failure. Defaults to 5.
;  miniter: The minimum number of clipping iterations. Default is 2.
;-
pro skymap::makeMapClip, clip, lo = lo, hi = hi, $
                         tol = tol, max_reject = max_reject, $
                         maxiter = maxiter, miniter = miniter

  if n_params() ne 1 then begin
     print, 'Calling sequence:'
     print, '   skymap->makeMapClip(clip, [/lo, /hi, '
     print, '           max_reject = max_reject, tol = tol'
     print, '           maxiter = maxiter, miniter = miniter]'
     return
  endif

  if keyword_set(lo) && keyword_set(hi) then $
     message, 'Cannot set both lo and hi'

  if ~is_scalar(clip) || clip le 0 then $
     message, 'Clip must be a positive scalar'

  MINITER = keyword_set(miniter) ? miniter : 2
  MAXITER = keyword_set(maxiter) ? maxiter : 5
  MAX_REJECT = keyword_set(max_reject) ? max_reject : .2
  CONVERGE_TOL = keyword_set(tol) ? tol : .01

  head = (*self.map).head
  if self.euclidian then begin
     x = *self.x
     y = *self.y
  endif else begin
     adxy, head, *self.x, *self.y, x, y
  endelse
  val = *self.val

  for i = 0, MAXITER - 1, 1 do begin
     if self.verbose then print, '      Sigma Clipping. Iteration '+strtrim(i+1,2)

     self->makeMap

     ;- flag outliers
     sigma = sqrt((*self.emap).map[x,y] + *self.dval^2)
     delta = (val - (*self.map).map[x, y]) / sigma
     if keyword_set(lo) then begin
        bad = (delta lt -1 * clip)
     endif else if keyword_set(hi) then begin
        bad = (delta gt clip)
     endif else begin
        bad = abs(delta) gt clip
     endelse
     good = ~bad

     if self.verbose then $
        print, total(bad), format='("      Number of rejects: ", i)'

     if total(bad) gt n_elements(x) * MAX_REJECT then begin
        message, 'Too many points rejected. Aborting', /con
        stop
        return
     endif

     ;- converged on a set of rejects
     if (i+1) ge MINITER && $
        total(good ne *self.included) lt CONVERGE_TOL * n_elements(x) then begin
        if self.verbose then print, '      CONVERGED.'
        return
     endif

     *self.included = good
  endfor

  message, 'Sigma clipping did not converge.', /con
end




;+
; PURPOSE:
;  This main procedure to create the smoothed map
;
; PROCEDURE:
;  This function uses weighted mean smoothing with a Gaussian
;  smoothing kernel.
;
;     V(x,y) = sum(w_i * val_i) / sum(w_i)
;  where
;     w_i = 1/dval_i^2 * exp[-((x-x_i)^2 + (y-y_i)^2) / 2 sigma^2]
;     w_i truncated to 0 at (x - x_i)^2 + (y - y_i)^2 > truncate^2
;
;  The variance map is given by
;   dV(x,y) = sum(w_i * dval_i^2) / sum(w_i)^2
;  In other words, it is the interpolated value of the variance, divided
;  by the "effective" number of points which contribute to each pixel.
;
;  The procedure only calculates weights out to the truncation radius
;  to speed things up. The procedure takes O(stamp_size * n_data)
;  operations, instead of O(map_size * n_data).
;;-
pro skymap::makeMap
  ;- initialize variables
  result = (*self.map).map * 0
  weight = result
  var = result
  head = (*self.map).head
  x = *self.x &  y = *self.y
  val = *self.val & dval = *self.dval
  doprint = self.verbose

  ;- lots of coordinates here:
  ;- (mx,my) map pixel coords
  ;- (ma,md) map sky coords
  ;- (dx,dy) data pixel coords
  ;- (da,dd) data sky coords
  ;- (sx,sy) stamp pixel coords
  ;- (sa,sd) stamp sky coords

  ;- map pixels to sky coords
  nx = sxpar(head, 'naxis1')
  ny = sxpar(head, 'naxis2')
  mx = rebin(findgen(nx), nx, ny, /sample)
  my = rebin(1#findgen(ny), nx, ny, /sample)
  if self.euclidian then begin
     ma = mx
     md = my
  endif else begin
     xyad, head, mx, my, ma, md
  endelse

  ;- data sky coords to pixels
  da = x
  dd = y
  if self.euclidian then begin
     dx = da
     dy = dd
  endif else begin
     adxy, head, da, dd, dx, dy
  endelse

  ;- a postage stamp
  ;- safely calculate minimum pixel size (may be variable)
  delt = (ma - shift(ma, 1,0)) > (md - shift(md, 0,1))
  delt[0,*] = !values.f_infinity & delt[*,0] = !values.f_infinity
  delt = min(delt) ;- degrees per pixel
  stampsz = ceil(2 * self.truncate / delt) + 1
  stampsz = stampsz < ( 2 * (nx > ny))

  ;- stamp pixel coords
  sx = rebin(indgen(stampsz) - stampsz / 2, stampsz, stampsz, /sample)
  sy = rebin(1#indgen(stampsz) - stampsz / 2, stampsz, stampsz, /sample)

  ;- loop over sources, vectorize on pixels
  nobj = n_elements(x)
  if doprint then begin
     print, nobj, stampsz, format='("Measurements: ", i0, "  Stamp size:  ", i0, " pixels")'
     pbar, /new
  endif

  for i = 0L, nobj - 1, 1 do begin
     if doprint && (i mod 1500) eq 0 then pbar, 1. * i / nobj
     if ~(*self.included)[i] then continue

     ;- extract postage stamp
     tx = floor(sx + dx[i])
     ty = floor(sy + dy[i])
     sa = ma[tx, ty] & sd = md[tx, ty]
     w = self->weight(i, sa, sd)

     stamp, w * val[i], 0, 0, $
            result, tx[0], ty[0], stampsz, stampsz, /add
     stamp, w, 0, 0, $
            weight, tx[0], ty[0], stampsz, stampsz, /add
     stamp, w^2 * dval[i]^2, 0, 0, $
            var, tx[0], ty[0], stampsz, stampsz, /add
  endfor
  if doprint then pbar, /close

  result /= weight
  emap = var / weight^2

  bad = where(weight eq 0, ct)
  if ct ne 0 then begin
     result[bad] = !values.f_nan
     emap[bad] = !values.f_nan
  endif

  (*self.map).map = result
  (*self.emap).map = emap
end

;+
; PURPOSE:
;  Initializes the object
;
; INPUTS:
;  map: A map structure created by map_init.pro
;  x: The x coordinates of the data
;  y: The y coordinates of the data
;  val: Value of the data
;  dval: 1-sigma errors on the data
;
; KEYWORD PARAMETERS:
;  fwhm: The fwhm of the smoothing kernel, in degrees. Defaults to
;        1/100 of the map size
;  truncate: The radius at which to truncate the smoothing. This can
;            considerably speed up execution time. See makeMap for
;            details.
;  verbose: Set to request extra textual output
;
; OUTPUTS:
;  1 if successful. 0 otherwise.
;-
function skymap::init, map, x, y, $
                       val, dval, fwhm = fwhm, $
                       truncate = truncate, $
                       verbose = verbose
  compile_opt idl2

  ;- check inputs
  if n_params() ne 5 then begin
     print, 'calling sequence'
     print, ' obj = obj_new("skymap", map, x, y, val, dval, '
     print, '                 [fwhm = fwhm, truncate = truncate, /verbose])'
     return, 0
  endif

  if size(map, /type) ne 8 then $
     message, 'map must be a structure. Use map_init.pro'
  tags = tag_names(map)
  if n_elements(tags) ne 2 || tags[0] ne 'MAP' || tags[1] ne 'HEAD' $
     then message, 'map must be a structure. Use map_init.pro'

  nobj = n_elements(x)
  if n_elements(y) ne nobj || n_elements(val) ne nobj || $
     n_elements(dval) ne nobj then $
        message, 'x, y, val, and dval not the same size'

  if ~keyword_set(fwhm) then $
     fwhm = sxpar(map.head, 'naxis2') * abs(sxpar(map.head, 'cdelt2')) / 100.
  sigma = fwhm / (2 * sqrt(2 * alog(2)))
  if ~keyword_set(truncate) then truncate = fwhm * 2

  sxaddpar, map.head, 'SIGMA', sigma, 'Smoothing kernel, DEGREES'

  self.x = ptr_new(x)
  self.included = ptr_new(byte(x) * 0B + 1B, /no_copy)
  self.y = ptr_new(y)
  self.val = ptr_new(val)
  self.dval = ptr_new(dval)

  self.map = ptr_new(map)
  self.emap = ptr_new(map)

  self.sigma = sigma
  self.truncate = truncate
  self.verbose = keyword_set(verbose)
  self.euclidian = sxpar(map.head, 'EUCLID') eq 1

  return, 1
end

pro skymap__cleanup
  ptr_free, [self.x, self.y, self.val, self,dval, $
             self.map, self.emap, self.weight]
end

pro skymap__define
  data = {skymap, x:ptr_new(), $
          y:ptr_new(), val:ptr_new(), dval:ptr_new(), $
          included:ptr_new(), $
          euclidian: 0, $
          map:ptr_new(), emap:ptr_new(), $
          truncate:0., sigma:0., verbose:0}
end


