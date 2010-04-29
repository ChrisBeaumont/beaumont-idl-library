;+
; PURPOSE:
;  This function minimizes a multidimensional function based on
;  Powell's conjugate direction method, described in Numerical
;  Recipes 3rd ed., ch 10.7.1. It repeatedly minimizes the function
;  along a given 1D direction, and tries to choose new directions
;  which don't spoil previous minimizations.
;
;  The code is adapted from the pseudocode in ch 10.7.1 of Numerical
;  Recipes 3rd ed. However, the actual code was written independently
;  from the code in that book.
;
; CATEGORY:
;  Numerical Recipes
;
; CALLING SEQUENCE:
;  result = multimin(func, point, [fmin = fmin, tol = tol, _extra =
;                    extra, /verbose)
;
; INPUTS:
;  func: String name of a function to minimize. The function must have
;        a calling sequence like function func, x, _extra = extra. x must be
;        a n-element vector. Extra keywords passed to multimin will be
;        relayed to calls of func.
;  point: A seed point to start the minimization at. An n-element
;         array.
;
; KEYWORD PARAMETERS:
;  fmin: Set to a variable to hold the value of the function at the
;        minimum value.
;  tol:  The fractional accuracy of the minimum location. Default is
;        1d-4.
;  verbose: Produce extra output.
;  step: The rough scale of the function to minimize. This is the
;        length of the first step taken during each line
;        minimization. Default is 1. 
;
; OUTPUTS:
;  The location of the minimum
;
; MODIFICATION HISTORY:
;  June 2009: Written by Chris Beaumont.
;  April 2010: Tweaked parameter checking. cnb.
;- 
function multimin, func, point, $
                   fmin = fmin, $
                   tol = tol, _extra = extra, $
                   verbose = verbose, $
                   step = step
  
  ;- check inputs
  if n_params() ne 2 then begin
     print, 'multimin calling sequence:'
     print, 'result = multimin(func, point, [fmin = fmin,'
     print, '                  tol = tol, /verbose, _extra = extra'
     print, '         func: result = func(x, _extra = extra)'
     return, !values.f_nan
  endif

  if size(func,/type) ne 7 then $
     message, 'func must be a string'
  
  MAXITER = 100
  TINY = 1d-20

  ndim = n_elements(point)
  if ~keyword_set(tol) then tol = 1d-4

  dirs = dblarr(ndim, ndim)   ;- each row is a direction
  drops = dblarr(ndim)        ;- decrease in f(x) after each minimization
  for i = 0, ndim-1, 1 do dirs[i,i] = 1

  f0   = call_function(func, point, _extra = extra) ;- f(x) at beginning of cycle
  fmin = f0                                         ;- lowest f found yet
  p0   = point                                      ;- p at beginning of cycle
  p    = point                                      ;- current point
  
  for niter = 0, MAXITER - 1, 1 do begin
     maxDist = 0                ;- max movement in a cycle 
     maxDistInd = 0             ;- iteration of max movement  
     p0 = p
     f0 = fmin
     for i = 0, ndim - 1, 1 do begin
        d = dirs[*, i]
        min = linmin(func, p, d, step = step, dist = dist, _extra = extra, $
                     verbose = verbose, tol = tol)
        if ~finite(min) then begin
           verbiage, 'Line minimization failed. Aborting.', 1, verbose
           return, !values.f_nan
        endif

        assert, min(finite(p)) eq 1
        drops[i] = fmin - min
        fmin = min

        if (dist gt maxDist) then begin
           maxDist = dist
           maxDistInd = i
        endif
     endfor
   
     pe = p0 + 2 * (p - p0)
     fn = fmin
     fe = call_function(func, pe, _extra = extra)
     deltaf = max(drops)
     
     keep1 = fe ge f0
     keep2 = (2 * (f0 - 2 * fn + fe) * ((f0 - fn) - deltaf)^2) ge $
             (f0 - fe)^2 * deltaf
 
     ;- update the direction set
     if ~keep1 && ~keep2 then $
        dirs[*, maxDistInd] = (p - p0) / sqrt(total((p - p0)^2))
     
     ;- test for convergence
     if max(abs(p0 - p)) le max(tol * abs(p0 + p + TINY)) then begin
        verbiage, 'converged in '+strtrim(niter,2)+' iterations.', 2, verbose
        return, p
     endif
    
  endfor ;- MAXITERS

  verbiage, 'Did not converge after '+strtrim(maxiter,2)+' iterations', 1, verbose

  return, !values.f_nan
end
