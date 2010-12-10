;+
; NAME:
;  SKYMAP
;
; PURPOSE:
;  This class creates a smoothed sky map from an irregularly spaced
;  set of measurements. The default smoothing kernel is a gaussian.
;
; The smoothed map is evaluated according to
;  m(x,y) = sum( w_i v_i) / sum(w_i)
;
; where v_i is the ith sampled data point and w_i is a smoothing
; function. In this class, w_i is a gaussian centered on v_i, with a
; fwhm specified by the user. 
;
; The variance map is given by
;  v(x,y) = sum(w_i^2 v_i) / sum(w_i)^2
;
; METHODS:
;  weight: Implementation of the weighting kernel.
;  getMap: return the smoothed map
;  writeFits: output the map to a fits file
;  makeMap: calculate the smoothed map
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
;-
function skymap::weight, id, x, y
  dval = (*self.dval)[id]
  gcirc, 2, (*self.x)[id], (*self.y)[id], x, y, dis
  dis /= 3600.
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
  

pro skymap::makeMapClip, clip = clip, lo = lo, hi = hi

  MAXITER = 5
  MAX_REJECT = .2
  CONVERGE_TOL = .01

  a = *self.x & d = *self.y & val = *self.val
  head = (*self.map).head
  adxy, head, a, d, x, y

  for i = 0, MAXITER - 1, 1 do begin
     if self.verbose then print, '      Sigma Clipping. Iteration '+strtrim(i+1,2)

     self->makeMap     
     
     ;- flag outliers
     delta = ((*self.map).map[x, y] - val) / sqrt((*self.emap).map)
     if keyword_set(lo) then begin
        bad = (delta lt -1 * clip)
     endif else if keyword_set(hi) then begin
        bad = (delta gt clip)
     endif else begin
        bad = abs(delta) gt clip
     endelse
     good = ~bad

     if total(bad) gt n_elements(x) * MAX_REJECT then begin
        message, 'Too many points rejected. Aborting', /con
        return
     endif     

     if self.verbose then $
        print, total(bad), format='("      Number of rejects: ", i)'

     ;- converged on a set of rejects
     if total(good ne *self.included) lt CONVERGE_TOL * n_elements(x) then begin
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
  xyad, head, mx, my, ma, md

  ;- data sky coords to pixels
  da = x
  dd = y
  adxy, head, da, dd, dx, dy

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
     fwhm = sxpar(map.head, 'naxis2') * abs(sxpar('cdelt2')) / 100.
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
          map:ptr_new(), emap:ptr_new(), $
          truncate:0., sigma:0., verbose:0}
end


