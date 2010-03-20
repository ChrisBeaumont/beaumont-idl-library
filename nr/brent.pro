;+
; PURPOSE:
;  This function minimizes a function of one variable. The function
;  requires 3 input abscissas which bracket at least one local minimum
;  (see, e.g. bracket.pro)
;
;  The algorithm is adapted from Numerical Recipes, but is
;  substantially modified in an attempt to simplify the
;  procedure. This may make the program slower, but I don't
;  think it should be any less stable.
;
;  The program attempts to improve upon goldenmin by using parabolic
;  extrapolation in the vicinity of the minimum. If the function is
;  smooth, then this should quickly converge. If it seems that
;  parabolic extrapolation isn't behaving well, then it
;  defaults to a goldenmin style partitioning of (abc) into golden
;  sections. This ensures convergence.
;
;  The  biggest change from the routine in Numerical Recipes is the
;  test for when to use parabolic extrapolation. The default state is
;  to use parabolic extrapolation if the extrapolated minimum is
;  interior to (a,c). However, if the range of (a,c) decreases by less
;  than a factor of 1.5 in any iteration, then the algorithm is
;  'punished', and parabolic extrapolation is not used for a
;  round. The value of 1.5 is used because the goldenmin style of
;  partitioning shrinks (a,c) by about 1.67 per iteration. For smooth
;  functions, typical shrinkages seem to be >2 per iteration. They can
;  be as high as several thousand for nearly parabolic functions.
;
; CATEGORY:
;  Numerical Recipes
;
; CALLING SEQUENCE:
;  result = brent(func, ax, bx, cx, 
;                 fax, fbx, fcx, [tol tol, 
;                 fmin = fmin, /verbose, _extra = extra, /plot, 
;                 golden = golden)
;
; INPUTS:
;  func: The name of a user written function to minimize. The function
;        must have a calling sequence like result = func(x, _extra =
;        extra). It may declare extra keywords, which are supplied to
;        brent. It must return a scalar.
;
;   ax: The first point bracketing the minimum
;   bx: The second point bracketing the minimum
;   cx: The third point bracketing the minimum
;   fax: func(ax)
;   fbx: func(bx)
;   fcx: func(cx)
;
; KEYWORD PARAMETERS:
;  tol: The requested fractional precision of xmin. Defaults to 1d-3
;  verbose: Set to produce textual output
;  fmin: Set to a variable to hold f(xmin)
;  _extra: extra kewyords to pass along to calls of func
;  plot: Set to plot the points as it plugs along.
;  golden: If set, turn off parabolic interpolation (i.e. basically
;  golden search)
;
; OUTPUTS:
;  xmin: The approximate location of a minimum within (abc)
;
;- PSEUDOCODE:
;   xmin = brent(func,  // name of function
;                a,b,c, // points bracketing minimum
;                tol)   // fractional accuarcy of xmin
;  
;     step1 = step2 = 0
;     While (TRUE) do:
;       if (c - a) lt abs(b * tol) then return b
;       u  <-- chooseCandidate(a,b,c, step2). // new candidate min
;       fu <-- func(u)
;       temp <-- b               
;       updateBracket(a,b,c,u)   // narrow the bracket
;     endwhile
;   END
;
; SEE ALSO:
;  bracket, goldenmin
;
; MODIFICATION HISTORY:
;  June 2009: Adapted from NR by Chris Beaumont
;-
function brent, func, ax, bx, cx, $
                fax, fbx, fcx, $
                tol = tol, $
                verbose = verbose, $
                fmin = fmin, $
                _extra = extra, $
                plot = plot, $
                golden = golden

  compile_opt idl2
;- check inputs;
  if n_params() ne 7 then begin
     print, 'brent calling sequence:'
     print, ' xmin = brent(func, ax, bx, cx, fax, fbx, fcx, '
     print, '              [tol = tol, /verbose, fmin = fmin, '
     print, '               golden = golden, _extra = extra, /plot])'
     return, !values.f_nan
  endif
  
  if size(func, /type) ne 7 then $
     message, 'func must be a string'
  
  if (bx - ax) * (cx - bx) le 0 then  $
     message, 'b must be between a and c'
  
  if fbx ge fax || fbx ge fcx then $
     message, 'fbx must be < fax, fcx'
  
  if ~keyword_set(tol) then tol = 1d-3
  
  a = (ax lt cx) ? ax : cx
  c = (ax gt cx) ? ax : cx
  fa = (ax lt cx) ? fax : fcx
  fc = (ax gt cx) ? fax : fcx
  b = bx
  fb = double(fbx)
  
  shrinkage = 0D
  punishParabola = 0

  ITMAX = 100
  GOLD  = .3819660
  EPS   = 1d-7

  if keyword_set(plot) then begin
     loadct, 13, /silent
     plot, [a,b,c], [fa, fb, fc], psym = symcat(16), color = fsc_color('white')
  endif
 
  for i = 0, ITMAX - 1, 1 do begin
     if keyword_set(plot) then oplot, [a,b,c],[fa,fb,fc], sym = 8, $
                                     color = (5 * i) mod 255
     
     t1 = (tol * abs(b) + EPS)
     
     ;- output
     verbiage, string(i,a,b,c,format = '("iter: ", i3, "'+$
                      '   x:  ", f, ", ", f, ", ", f)'), 3, verbose
     verbiage, string(fa, fb, fc, $
                      format='("         f(x):  ", f, ", ", f, ", ", f)'), $
               3, verbose
     verbiage, string(abs(c - a) / (2 * t1), format='("       bracket / tol: ", e9.1)'), $
               3, verbose
     
     ;- test for convergence
     if abs(c - a) lt 2 * t1 then begin
        verbiage, 'Converged in '+strtrim(i,2)+' iterations', 2, verbose
        verbiage, string(b, fb, format='("xmin: ", f, " f(xmin): ", f)'), 2, verbose
        fmin = fb
        return, b
     endif
     
    assert, (b gt a) && (c gt b)

     ;- calculate two trial points
     u1 = (c - b) gt (b - a) ? $
          b + GOLD * (c - b) : $
          b - GOLD * (b - a)
     par = parabola(a, b, c, fa, fb, fc)
     u2 = - par[1] / (2 * par[2])

          ;- special case: u2 is very close to b
     ;- this is probably a good thing - b is very close to the right answer.
     if abs(u2 - b) lt t1 then begin
        ;- to the right of b?
        if u2 gt b then begin
           if (c - b) lt t1 then u2 = b - t1 else u2 = b + t1
        endif else begin
           if (b - a) lt t1 then u2 = b + t1 else u2 = b - t1
        endelse
     endif
     
     verbiage, string(u2, format='("   Parabola x:", f)'), 3, verbose
     

     ;- is u2 acceptable?
     if ~keyword_set(golden) && ~punishParabola && $
        (u2 gt a) && (u2 lt c) then begin
        verbiage, '     parabola accepted.', 3, verbose
        u = u2 
     endif else begin
        u = u1
        punishParabola = 0
     endelse

     ;- evaluate function at u and update bracket
     fu = call_function(func, u, _extra = extra)
     
     shrinkage = c - a
     if u gt b then begin
        ; buc bracket
        if fu lt fb then begin
           fa = fb
           fb = fu
           a = b
           b = u
        ; abu bracket
        endif else begin
           fc = fu
           c = u
        endelse
     endif else begin
      ;-aub bracket
        if (fu lt fb) then begin
           fc = fb
           fb = fu
           c = b
           b = u
        ;-ubc bracket
        endif else begin
           fa = fu
           a = u
        endelse
     endelse
     
     shrinkage = shrinkage / (c - a)
     verbiage, string(shrinkage, format='("    shrinkage:", e9.1)'), 3, verbose
     ;- golden shrinkage is about 1.618. 
     ;- punish parabola for not shrinking enough.
     if shrinkage lt 1.5 then punishParabola = 1
     assert, shrinkage gt 1
  endfor 

  ;- failure
  verbiage, 'Did not converge to a minimum within '+strtrim(MAXITER, 2)+' iterations', 1, verbose
  return, !values.f_nan
  
end
