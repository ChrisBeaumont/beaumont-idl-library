;+
; PURPOSE:
;  This function computes centered Voigt profiles. It is a wrapper to the
;  builtin IDL VOIGT routine, which uses a somewhat confusing variable
;  convention. The Voigt function implemented here is a stright
;  convolution of a Gaussian with a Lorentzian.
;
; INPUTS:
;  x: The abcissa values. The profile will be centered on x=0. Scalar
;     or vector.
;  sigma: The width of the Gaussian part of the profile. Scalar or
;         vector
;  gamma: The width of the Lorentzian part of the profile. Scalar or
;         vector 
;
; OUTPUTS:
;  The voigt profile with the specified sigma, gamma, evaluated at x.
;
; MODIFIATION HISTORY:
;  October 2010: Written by Chris Beaumont
;-
function cnb_voigt, x, sigma, gamma
  compile_opt idl2
  on_error, 2

  ;- check inputs
  if n_params() ne 3 then begin
     print, 'calling sequence'
     print, 'result = cnb_voigt(x, sigma, gamma)'
  endif
  ;- inputs are scalars or arrays of the same size
  nx = n_elements(x) & ng = n_elements(gamma) & ns = n_elements(sigma)
  num = (nx > ng > ns)
  if (nx ne 1 && nx ne num) || (ng ne 1 && ng ne num) || $
     (ns ne 1 && ns ne num) then $
        message, 'x, gamma, and sigma have incompatible sizes'

  ;- change of varialbes into IDL's voigt convention
  delta = sqrt(2) * sigma
  u = x / delta
  a = gamma / delta
  result = 1 / (sqrt(!pi) * delta) * voigt(a, u)

  ;- the above approach doesn't work when sigma=0.
  ;- this is just the lorentz profile in this case
  TINY = 1e-7
  bad = where(sigma lt TINY, ct)
  if ct ne 0 && ns eq 1 then result = gamma / (!pi * (x^2 + gamma^2))
  if ct ne 0 && ns gt 1 then begin
     g = ng eq 1 ? replicate(gamma, ct) : gamma[bad]
     subx = nx eq 1 ? replicate(x, ct) : x[bad]
     result[bad] = g / (!pi * (subx^2 + g^2))
  endif

  return, result
end

pro test
  compile_opt idl2
  ;- reporduce wikipedia graphs
  nstep = 100
  x = arrgen(-10, 10, nstep = nstep)
  plot, x, cnb_voigt(x, 1.53, 0)
  oplot, x, cnb_voigt(x, 1.3, 0.5), color = fsc_color('blue')
  oplot, x, cnb_voigt(x, 1.0, 1.0), color = fsc_color('green')
  oplot, x, cnb_voigt(x, 0, 1.8), color = fsc_color('red')

  ;- repeat, but try vector gammas, sigmas
  oplot, x, cnb_voigt(x, replicate(1.3, nstep), 0.5), color = fsc_color('blue')
  oplot, x, cnb_voigt(x, 1.3, replicate(0.5, nstep)), color = fsc_color('blue')
  oplot, x, cnb_voigt(x, replicate(1.3, nstep), replicate(0.5, nstep)), color = fsc_color('blue')

end
