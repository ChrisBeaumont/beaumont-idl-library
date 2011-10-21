;+
; PURPOSE:
;  This class implements the NICEST algorithm to make smoothed
;  extinction maps based on on stellar reddening data. The relevatn
;  equations are
;  
;  (1)A = sum(w_n A_n) / sum(w_n) - beta sum(w_n sigma_n^2) / sum(w_n)
;  (2)beta = alpha * k * ln10
;
;  A_n, sigma_n are the individual measurements+errors
;  alpha is the slope of the number counts N(mag) ~ 10^alpha mag
;  k is the ratio A_lambda/A
;
;  w_n = w(x';x)10^[alpha * k * An], w(x';x) is a gaussian.
;
; SUPERCLASSES:
;  skymap
;
;-
function nicest::weight, id, x, y
  ;- gaussian filter
  super = self->skymap::weight(id, x, y)
  ;- av correction factor
  super *= 10^(self.alpha * self.k * (*self.val)[id])
  return, super
end

;+
; PURPOSE:
;  Creates a smoothed map, using the Nicest weighting scheme
;
; KEYWORD PARAMETERS:
;  nocorr: Set to exclude the second term in equation (1) above. This
;  speeds up the calculation by about a factor of 2. Since the
;  correction term is (to first order) constant, it only affects the
;  zero-point, and can be removed via comparison to a control field.
;-
pro nicest::mapeMap, nocorr = nocorr
  self->skymap::makeMap
  if keyword_set(nocorr) then return
  tmp = *self.val
  *self.val = *self.dval^2
  map = *self.map
  emap = *self.emap
  self->skymap::makeMap
  *self.map = map - *self.map * self.beta
  *self.emap = emap
  *self.val = tmp
end

;+
; PURPOSE:
;  Set the value of alpha, the slope of the number counts
;
; INPUTS:
;  alpha: The new value
;-
pro nicest::setAlpha, alpha
  self.alpha = alpha
end

;+
; PURPOSE:
;  Return the value of alpha
;
; OUTPUTS:
;  alpha
;-
function nicest::getAlpha
  return, self.alpha
end

function nicest::init, map, x, y, val, dval, $
                 fwhm = fwhm, truncate = truncate, $
                 verbose = verbose, alpha = alpha, k = k
                
  if n_params() eq 0 then begin
     print, 'calling sequence:'
     print, 'obj = obj_new("nicest", map, x, y, val, dval, '
     print, '               [fwhm = fwhm, truncate = truncate, '
     print, '                k = k, alpha = alpha, /verbose]'
     return, 0
  endif
            
  super = self->skymap::init(map, x, y, val, dval, $
                             fwhm = fwhm, truncate = truncate, $
                             verbose = verbose)
  if ~super then return, 0

  if n_elements(alpha) ne 0 then self.alpha = alpha
  if n_elements(k) ne 0 then self.k = k else self.k = 1

  return, 1
end

pro nicest__define
  nicest = {nicest, inherits skymap, $
           beta:0., $
           alpha: 0., $
           k: 0.}
end
           
