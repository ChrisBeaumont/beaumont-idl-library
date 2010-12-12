;+
; PURPOSE:
;  This procedure creates a smoothed map of some function on the sky,
;  sampled at discrete points. It relies on adxy and gcirc from the
;  IDL astronomy user's library to handle all of the hard
;  spherical geometry stuff. 
;
; INPUTS:
;  map: A blank map and fits header. Use map_init.pro to create
;       this structure. 
;  x: The x location (sky coordinates in degrees) of the data
;  y: The y location (sky coordinates in degrees) of the data
;  val: The value at each (x,y) point
;  dval: The 1-sigma error at each point. 
;
; KEYWORD PARAMETERS:
;  fwhm: The fwhm of the smoothing kernel, in degrees. Defaults to
;        1/100 of the map size
;  truncate: The radius at which to truncate the smoothing. This can
;            considerably speed up execution time. See PROCEDURE for
;            details. Defaults to 2 * fwhm.
;  emap: Set to a named variable to hold the estimated variance
;        map.
;
; OUTPUTS:
;  On output, the map.map array will be populated with the smoothed
;  map. Pixels which are further than (truncate) from the nearest
;  source are set to NAN.
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
;
; MODIFICATION HISTORY:
;  March 2010: Written by Chris Beaumont. Meant to replace
;  SMOOTHMAP.pro.
;  Dec 2010: Added /SAMPLE to REBIN calls. cnb.
;-     
pro skymap_smooth, map, x, y, val, dval, $
                   fwhm = fwhm, $
                   truncate = truncate, $
                   emap = emap, weight = weight
  compile_opt idl2
  
  ;- check inputs
  if n_params() ne 5 then begin
     print, 'calling sequence'
     print, '  skymap_smooth, map, x, y, val, dval, '
     print, '                 [fwhm = fwhm, truncate = truncate, '
     print, '                  emap = emap]'
     return
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

  ;- initialize variables
  result = map.map * 0
  weight = result
  weight2 = result
  var = result

  ;- map pixels to sky coords
  nx = sxpar(map.head, 'naxis1')
  ny = sxpar(map.head, 'naxis2')
  mx = rebin(findgen(nx), nx, ny, /sample)
  my = rebin(1#findgen(ny), nx, ny, /sample)
  xyad, map.head, mx, my, ma, md

  ;- data sky coords to pixels
  da = x
  dd = y
  adxy, map.head, da, dd, dx, dy

  ;- a postage stamp
  ;- safely calculate minimum pixel size (may be variable)
  delt = (ma - shift(ma, 1,0)) > (md - shift(md, 0,1))
  delt[0,*] = !values.f_infinity & delt[*,0] = !values.f_infinity
  delt = min(delt) ;- degrees per pixel
  stampsz = ceil(2 * truncate / delt) + 1
  stampsz = stampsz < ( 2 * (nx > ny))
  
  ;- stamp pixel coords
  sx = rebin(indgen(stampsz) - stampsz / 2, stampsz, stampsz, /sample)
  sy = rebin(1#indgen(stampsz) - stampsz / 2, stampsz, stampsz, /sample)
  
  ;- loop over sources, vectorize on pixels
  pbar, /new
  for i = 0, nobj - 1, 1 do begin
     if (i mod 20) eq 0 then pbar, 1. * i / nobj
     tx = floor(sx + dx[i])
     ty = floor(sy + dy[i])
     sa = ma[tx, ty] & sd = md[tx, ty]
     gcirc, 2, da[i], dd[i], sa, sd, dis
     dis /= 3600.
     w = 1/dval[i]^2 * exp(-dis^2 / (2 * sigma^2.)) * (dis lt truncate)

     ;- where do we put the postage stamp down?
     l = min(tx) > 0        & sl = l - min(tx)
     r = max(tx) < (nx - 1) & sr = stampsz - 1 + (r - max(tx)) 
     b = min(ty) > 0        & sb = b - min(ty)
     t = max(ty) < (ny - 1) & st = stampsz - 1 + (t - max(ty))
     if (r - l) < (t - b) le 1 then continue
     assert, r - l eq sr - sl
     assert, t - b eq st - sb
        
     ;- update the maps
     result[l:r, b:t] += w[sl:sr, sb:st] * val[i]
     weight[l:r, b:t] += w[sl:sr, sb:st]
     var[l:r, b:t] += w[sl:sr, sb:st] * dval[i]^2
     weight2[l:r, b:t] += w[sl:sr, sb:st]^2
  endfor
  pbar, /close

  result /= weight
  emap = var / weight^2
  bad = where(weight eq 0, ct)
  
  if ct ne 0 then begin
     result[bad] = !values.f_nan
     emap[bad] = !values.f_nan
  endif
  
  map.map = result
  return
end


; 
; PURPOSE:
;  Tests the skymap_smooth procedure by comparing results with the
;  skymap_smooth_slow procedure, which uses an easier-to-implement
;  algorithm. 
;
pro test


  ;- try to recover a known map
  file=FILEPATH('head.dat', SUBDIR=['examples', 'data']) 
  OPENR, UNIT, file, /GET_LUN 
  data = BYTARR(80, 100, 57, /NOZERO) 
  READU, UNIT, data 
  CLOSE, UNIT
  data = total(data, 3)
  data = congrid(data, 400, 500)
  sz = size(data)
  window, 0, xsize = sz[1] * 2, ysize = sz[2] * 2
  tvscl, data, 0

  ;- sampled data with noise
  npts = 5000
  x = randomu(seed, npts) * sz[1]
  y = randomu(seed, npts) * sz[2]
  sample = data[x,y] + randomn(seed, npts) * max(data)/1d4
  badim = data * 0
  badim[x,y] = sample
  tvscl, bytscl(badim), 1

  x /= sz[1] & y /= sz[2]
  map = map_init(center = [.5, .5], width = [1,1], pixwidth = [sz[1], sz[2]])
  fwhm = .005
  map2 = map
  skymap_smooth, map, x, y, sample, sample * 0 + max(data) / 1d4, $
                 fwhm = fwhm, truncate = fwhm * 2
  tvscl, map.map, 2, /nan
  map2 = map
  o = obj_new('skymap', map2, x,y,sample, sample * 0 + max(data)/1d4, $
              fwhm = fwhm, truncate = fwhm * 2, /verbose)
  o->makeMap

  tvscl, (o->getMap()).map, 3, /nan
  print, minmax((o->getMap()).map - map.map,/nan)

  return
  

  ;- test against skymap_smooth_slow 
  num = 1000
  x = randomn(seed, num) * 20
  y = randomn(seed, num) * 20
  val = randomn(seed, num)
  dval = val / 3

  map = map_init(center = [0,0], width=[30,30], $
                 pixwidth = [100, 100])

  map2 = map


  print, 'map'
  t0 = systime(/seconds)
  skymap_smooth_sigclip, map, x, y, val, dval, $
                 fwhm = .3, truncate = .9, emap = emap, clip = 3
  print, time2string(systime(/seconds) - t0)
  t0 = systime(/seconds)
  print, 'map2'
  skymap_smooth_slow, map2, x, y, val, dval, $
                 fwhm = .3, truncate = .9, emap = emap2

  print, time2string(systime(/seconds) - t0)
  lo = min(map.map, max=hi, /nan)
  tvblink, nanscale((map.map - lo) / (hi - lo)), $
           nanscale((map2.map - lo) / (hi - lo))
  print, minmax((map.map - map2.map) / map2.map,/nan)
end
