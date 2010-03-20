;+
; PURPOSE:
;  A wrapper function used internally to linmin. Feeds a
;  multidimensional function to brent and bracket.
;
; INPUTS:
;  lambda: Position along the line
;
; KEYWORD PARAMETERS:
;  point: The starting point for the line
;  dir: A vector giving the direction of the line
;  linfunc: A user written function to minimize. Evaluated at(point +
;           lambda * dir)
;  _extra: Any extra keywords are passed to linfunc.
; 
; OUTPUTS:
;  linfunc(point + lambda * dir, _extra = extra) 
;-
function linmin_func, lambda, $
                      point = point, dir = dir, _extra = extra, $
                      linfunc = func
  return, call_function(func, point + dir * lambda, _extra = extra)
end

;+
; PURPOSE:
;  This function minimizes a multidimensional function along one
;  direction (not necessarily aligned with one of the axes). It is
;  used as a driver in multidimensonal minimization. It itself is a
;  driver of the functions 'bracket' and 'brent' to perform 1D
;  function minimization. The function name and basic logic is taken
;  from Numerical Recipes (Press et al. 2007)
;
; CATEGORY:
;  Numerical Recipes
;
; CALLING SEQUENCE:
;  result = linmin(func, point, dir, [step = step, dist = dist, _extra
;                  = extra])
;
; INPUTS:
;  func: The string name of a (multidimensional function). The function must
;        have the following calling sequence: 
;             result = func(x, _extra = extra)
;        x is an n element vector, the result is a scalar. Extra
;        keywords in linmin will be relayed to keywords of
;        func. 
;        WARNING- Do not declare the following keywords in
;        func: point, dir, fmin. These keywords are used internally to
;        linmin.
;  point: A reference point on the line to minimize func over. An
;         n-element array. This is updated with the new minimum upon
;         completion. 
;  dir:   The direction of the line to minimize. An n-element array. 
;  
; KEYWORD PARAMETERS:
;  step: The initial stepsize to take when bracketing the
;        minimum. This sets the size-scale of the function, and can be
;        used to help the linmin find the minimum closest to
;        point. Defaults to 1. 
;  dist: A keyword to hold the distance between point and the minimum.
;  _extra: Any extra keywords will be passed to func
;  verbose: Control the amount of textual output. Verbosity is handled
;  via the VERBIAGE program - see documentation for details.
;
; OUTPUTS:
;  The minimum of the function along the line specified by point and
;  dir.
;
; SIDE EFFECTS:
;  point is updated with the new minimum.
;
; MODIFICATION HISTORY:
;  June 2009: Written by Chris Beaumont
;-
function linmin, func, point, dir, step = step, dist = dist, _extra = extra, $
                 verbose = verbose
  compile_opt idl2
  
  ;- check inputs
  if n_params() ne 3 then begin
     print, 'linmin calling sequence:'
     print, 'result = linmin(func, point, dir, [step = step, dist = dist, _extra = extra]'
     return, !values.f_nan
  endif

  if ~keyword_set(step) then step = 1D
  
  ;- check for illegal use of reserved keywords
  if keyword_set(extra) then begin
     tags = tag_names(extra)
     bad = strcmp(tags, 'point',/fold) and $
           strcmp(tags, 'dir', /fold) and $
           strcmp(tags, 'fmin', /fold)
     if max(bad) ne 0 then $
        message, 'Attempt to override a reserved keyword (point, dir, fmin)'
  endif

  ;- bracket the function along dir
  bracket, 'linmin_func', 0, step, $
           ax, bx, cx, fa, fb, fc, $
           dir = dir, point = point, _extra = extra, linfunc = func, $
           verbose = verbose

  if ax eq bx || bx eq cx || fb ge fa || fb ge fc then begin
     ;assert, ax ne bx && bx ne cx && fb lt fa && fb lt fc
     v = string(ax, bx, cx, fa, fb, fc)
     verbiage, "bracket didn't work:", 1, verbose
     save, file='bracket_bug.sav'
     return, !values.f_nan
  endif

  if ~finite(ax) || ~finite(fa) then begin
     verbiage, "couldn't bracket a minimum", 1, verbose
     return, !values.f_nan
  endif

  ;- minimize the function along dir using brent's method
  result = brent('linmin_func', $
                 ax, bx, cx, $
                 fa, fb, fc, $
                 _extra = extra, point = point, dir = dir, fmin = fmin, $
                linfunc = func, $
                verbose = verbose)
  
  ;- update point to the new minimum
  point = point + result * dir
  dist = sqrt(total((dir * result)^2))
  return, fmin
end
