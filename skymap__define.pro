;+
; NAME:
;  SKYMAP
;
; PURPOSE:
;  This class creates a smoothed sky map from an irregularly spaced
;  set of measurements. The default smoothing kernel is a gaussian.
;
; METHODS:
;  weight: Implementation of the weighting kernel.
;  getMap: return the smoothed map
;  writeFits: output the map to a fits file
;  makeMap: calculate the smoothed map
;  init: Create the object
;  cleanup: Destroy the object
;
;-

;+
; PURPOSE:
;  This function evaluates the weight/smoothing kernel applied on
;  source "id" at location (x,y)
;
; INPUTS:
;  id: The index of the source to consider
;   x: The x location in sky coordinates
;   y: The y location in sky coordinates
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
  weight2 = result
  var = result
  head = (*self.map).head
  x = *self.x &  y = *self.y
  val = *self.val & dval = *self.dval

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
  for i = 0L, nobj - 1, 1 do begin

     ;- extract postage stamp
     tx = floor(sx + dx[i])
     ty = floor(sy + dy[i])
     sa = ma[tx, ty] & sd = md[tx, ty]
     w = self->weight(i, sa, sd)
     
     stamp, w * val[i], 0, 0, $
            result, tx[0], ty[0], stampsz, stampsz, /add
     stamp, w, 0, 0, $
            weight, tx[0], ty[0], stampsz, stampsz, /add
     stamp, w * dval[i]^2, 0, 0, $
            var, tx[0], ty[0], stampsz, stampsz, /add
     stamp, w^2, 0, 0, $
            weight2, tx[0], ty[0], stampsz, stampsz, /add
  endfor
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
     print, '                 [fwhm = fwhm, truncate = truncate])'
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

  
  self.x = ptr_new(x)
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
          map:ptr_new(), emap:ptr_new(), $
          truncate:0., sigma:0., verbose:0}
end


