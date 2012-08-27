;+
; NAME:
;  goldenMin
;
; PURPOSE:
;  This function finds a (local) minimum of a function, given three
;  points which bracket this minimum. The routine is lifted from the
;  routine 'golden' from Numerical Recipes
;
; CATEGORY:
;  Numerical recipes
;
; CALLING SEQUENCE:
;  result = goldenMin(func, xa, xb, xc, [tol = tol, /verbose, _extra =
;  extra])
;
; INPUTS:
;  func: The string name of a function to minimize. This function must
;  be of the form y = f(x). It must return a scalar.
;
;  xa: The first of three points bracketing the minimum
;  xb: The second point. f(xb) must be < than both f(xa) and f(xc),
;  and must lie between xa and xc
;  xc: The final point bracketing the minimum
;
; KEYWORD PARAMETERS:
;  tol: The desired fractional precision of the minimum
;  coordinate. Defaults to .001 if absent.
;
;  VERBOSE: Print extra information
;  _extra: Any extra keywords will be passed to FUNC
;
; OUTPUTS:
;  The value x for which f(x) is a mimum in the range [xa, xb].
;
; MODIFICATION HISTORY:
;  Written by: Chris Beaumont, Feb 2009
;  Added _extra keyword. cnb. June 2009
;-
function goldenmin, func, xa, xb, xc, $
                    tol = tol, verbose = verbose, _extra = extra, $
                    fmin= fmin
compile_opt idl2
;-on_error, 2

;-check arguments
if n_params() ne 4 then begin
   print, 'goldenMin calling sequence:'
   print, 'result = goldenMin(func, xa, xb, xc, [tol = tol, /verbose])'
   return, !values.f_nan
endif

if n_elements(tol) ne 1 then tol = 1d-3

if size(func,/type) ne 7 then $
   message, 'supplied func must be a string'

if sign(xb - xa) * sign(xc - xa) le 0 then $
   message, 'xb must lie between xa and xc'

;-use the golden ratio in picking midpoints
R = 0.61903399D
C =1d - R

fxa = call_function(func, xa, _extra = extra)
fxb = call_function(func, xb, _extra = extra)
fxc = call_function(func, xc, _extra = extra)
if fxb  ge fxa || fxb ge fxc then $
   message, 'func(xb) must be less than func(xa) and func(xc)'

x0 = xa
x3 = xc

if (abs(xc - xb) gt abs(xb - xa)) then begin
   x1 = xb
   x2 = xb + C * (xc - xb)
endif else begin
   x2 = xb
   x1 = xb - C * (xb - xa)
endelse

f1 = call_function(func, x1, _extra = extra)
f2 = call_function(func, x2, _extra = extra)
niter = 0
while (abs(x3 - x0) gt tol * (abs(x1) + abs(x2))) do begin
   niter++
   if (f2 lt f1) then begin
      x0 = x1
      x1 = x2
      x2 = R * x1 + C * x3
      f1 = f2
      f2 = call_function(func, x2, _extra = extra)
   endif else begin
      x3 = x2
      x2 = x1
      x1 = R * x2 + C * x0
      f2 = f1
      f1 = call_function(func, x1, _extra = extra)
   endelse
endwhile

if keyword_set(verbose) then print, 'goldenmin converged in '+strtrim(niter)+' iterations.'
fmin = min([f1, f2])
if (f1 lt f2) then return, x1
;else
return, x2

end
