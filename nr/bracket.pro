;+
; PURPOSE:
;  This procedure attempts to bracket the minimum of a one dimensional
;  function. A minimum is bracketed by three points ax, bx, cx, if:
;    1) bx is between ax and cx
;    2) f(b) < f(a) and f(b) < f(c)
;  In this situation, provided the function is continuous, there is
;  guaranteed to be a local minimum in between a and c.
;
;  The procedure is fairly robust in finding _some_ minimum, provided
;  one exists. However, there is no guarantee that it will find the
;  minimum closest to the starting location. 
;
;  The algorithm is lifted from Numerical Recipes, with some attempt
;  to make the base cases a bit clearer
;
; CATEGORY:
;  Numerical Recipes
;
; CALLING SEQUENCE:
;  bracket, func, a, b, ax, bx, cx, fa, fb, fc, /verbose, _extra =
;  extra, struct = struct
;
; INPUTS:
;  func: A string giving the name of the 1D function to bracket. The
;        function must have the following calling sequence:
;           function func, x, _extra = extra
;        it may declare extra keywords, which can be supplied to
;        bracket and passed along to the function
;
;  a: The first trial abscissa
;  b: The second trial abscissa. The search for the minimum starts at
;     a and b, and proceeds downhill. Note that (a-b) should be
;     scaled, if possible, to match the scale of the wiggles in the
;     function. This will help in finding the nearest minimum
;
; OPTIONAL OUTPUTS:
;  ax: Named variable to hold the first abscissa of the bracket
;  bx: Named variable to hold the second abscissa of the bracket
;  cx: Named variable to hold the final abscissa of the bracket
;  fa: Named variable to hold f(ax)
;  fb: Named variable to hold f(bx)
;  fc: Named variable to hold f(cx)
;
; KEYWORD PARAMETERS:
;  _extra: Any extra keywords will be passed to FUNC
;  verbose: Set to an integer to control the amount of textual output
;  to produce (see VERBIAGE procedure for details) 
;  struct: Set to a variable to receive the result in a structure.
;
; PROCEDURE:
;  The procedure is adapted from Numerical Recipes, 3rd ed., page
;  491. It always keeps track of three x values (abc).  It repeatedly
;  steps downhill. At each iteration, it approximates the 3 most
;  recent points as a parabola, and extrapolates to the parabola's min
;  (if it exists). If parabolic extrapolation doesn't work (because
;  the points are nearly co-linear or concave down), it just takes a
;  moderately larger step than last time. It stops when the function
;  starts increasing again.
;
; MODIFICATION HISTORY:
;  June 2009: Written by Chris Beaumont
;  August 2010: Added struct keyword. cnb.
;-
pro bracket, func, a, b, $
             ax, bx, cx, $
             fa, fb, fc, $
             _extra = extra, $
             verbose = verbose

compile_opt idl2
;- on_error, 2

GOLD = 1.618034
TINY = 1d-20
GLIM = 100
MAXTRY = 100

;- check inputs
if n_params() lt 3 then begin
   print, 'bracket calling sequence:'
   print, 'bracket, func, a, b, ax, bx, cx, fa, fb, fc, [/verbose, _extra = extra]'
   return
endif

if size(func, /type) ne 7 then $
   message, 'func must be a string'

ax = a
bx = b
fa = call_function(func, a, _extra = extra)
fb = call_function(func, b, _extra = extra)
ntry = 0

;- define a->b as the downhill direction
if (fb gt fa) then begin
   swap, fb, fa
   swap, ax, bx
endif

;- guess at the minimum
cx = bx + GOLD * (bx - ax)
fc = call_function(func, cx, _extra = extra)
while(fb ge fc) do begin
   msg = string(ntry, ax, bx, cx, fa, fb, fc, $
                format='("Iteration: ", i4, 2(3x, "(", e0.3, 2x, e0.3, 2x, e0.3, ")"))')
   verbiage, msg, 3, verbose

   if (ntry++) ge MAXTRY then goto, failure
   ;- SUMMARY OF POSSIBILITES:
   ;- 1) ax, bx, cx are fit by a very steep parabola. Expected to be 
   ;-    numerically unstable. Parabola isn't used. (pitfall 1)
   ;- 2) ax, bx, cx are nearly colinear or concave down. Parabolic 
   ;-    extrapolation won't work. (pitfall 2)
   ;- 3) Parabola minimum isn't finite. Abort (pitfall 3)
   ;- 4) buc is a bracket (case 1a)
   ;- 5) abu is a bracket (case 1b)
   ;- 6) u between b and c, but not a good point.
   ;-    step beyond c and try again (case 1c)
   ;- 7) bcu is a bracket (case 2a)
   ;- 8) bcu is new abc (case 2b)
   ;- 9) f(b) eventually < f(c) (end of while statement).
   ;-    abc is the bracket.
   
   ;- INVARIANT: fb < fa at all times
   assert, fa ge fb

   ;- fit the parabola y = p + qx + rx^2 to
   ;- points a,b,c
   dummy = (bx - ax) / (cx - bx)
   lhs = (fb - fa) - (fc - fb) * dummy
   rhs = (bx^2 - ax^2) - (cx^2 - bx^2) * dummy
   
   ;- pitfall 1:
   ;- parabola fit has very large q value, and is untrustworthy
   ;- just step forward
   if (abs(rhs) lt TINY) then begin
      ux = cx + (cx - bx) * GOLD
      fu = call_function(func, ux, _extra = extra)
      shift, ux, cx, bx, ax
      shift, fu, fc, fb, fa
      continue
   endif
  
   r = lhs / rhs
   q = ((fb - fa) - r * (bx^2 - ax^2)) / (bx - ax)
   
   ;- pitfall 2: 
   ;  Parabola is nearly colinear or concave down,
   ;- so finding the turning point won't help. Just
   ;- take a step forward
   if (r lt TINY) then begin
      ux = cx + (cx - bx) * GOLD
      fu = call_function(func, ux, _extra = extra)
      shift, ux, cx, bx, ax
      shift, fu, fc, fb, fa
      continue
   endif

   ;- find the abscissa for the minimum of this parabola
   ;- limit the size of the extrapolation
   ux = - q / (2 * r)
   ulim = bx + GLIM * (cx - bx)
   if (ux - ulim) * (ulim - cx) gt 0 then ux = ulim
   fu = call_function(func, ux, _extra = extra)
     
   ;- pitfall 3: f(u) is undefined. Abort
   if ~finite(fu) then begin
      verbiage, 'Hit a non-finite function value. Aborting', 1, verbose
      nan = !values.f_nan
      ax = nan
      bx = nan
      cx = nan
      fa = nan
      fb = nan
      fc = nan
      struct={bracket, ax:ax, bx:bx, cx:cx, fa:fa, fb:fb, fc:fc}
      return
   endif

   ;- pitfall 4: ux is very near a, b, or c.
   if min(abs(ux - [ax,bx,cx])) lt $
      1d-4 * max(abs([ax,bx,cx]) + 1) then begin
      ux = cx + (cx - bx) * GOLD
      fu = call_function(func, ux, _extra = extra)
      shift, ux, cx, bx, ax
      shift, fu, fc, fb, fa
      continue
   endif

   ;- case 1: ux is between bx and cx
   if (ux - bx) * (cx - ux) gt 0 then begin
      
      ;- case 1a: buc brackes a minimum. success.
      if (fu lt fc) then begin
         fa = fb
         fb = fu
         ;fc = fc
         ax = bx
         bx = ux
         ;cx = cx
         struct={bracket, ax:ax, bx:bx, cx:cx, fa:fa, fb:fb, fc:fc}
         return
      ;- case 1b: abu brackets a minimum. success
      endif else if (fu gt fb) then begin
         fc = fu
         cx = ux
         struct={bracket, ax:ax, bx:bx, cx:cx, fa:fa, fb:fb, fc:fc}
         return
      endif
      ;- case 1c: u didn't bracket a minimum
      ux = cx + GOLD * (cx - bx)
      fu = call_function(func, ux, _extra = extra)
      shift, ux, cx, bx, ax
      shift, fu, fc, fb, fa
      continue

   ;- case 2: u is beyond c (it must be, I think)
   endif else if (ux - cx) * (cx - bx) gt 0 then begin

      ;- case 2a: bcu brackets a minimum. success.
      if (fu  gt fc) then begin
         shift, fu, fc, fb, fa
         shift, ux, cx, bx, ax
         struct={bracket, ax:ax, bx:bx, cx:cx, fa:fa, fb:fb, fc:fc}
         return
      
      ;- case 2b: fu < fc. abc -> bcu
      endif else begin
         shift, fu, fc, fb, fa
         shift, ux, cx, bx, ax
         continue
      endelse
      
   endif
   
   ;- I'm not sure how things ever get here, but just take
   ;- a step forward and continue
   ;- case 1c: u didn't bracket a minimum
   ux = cx + GOLD * (cx - bx)
   fu = call_function(func, ux, _extra = extra)
   shift, ux, cx, bx, ax
   shift, fu, fc, fb, fa
endwhile 
      
;- abc brackets a minimum. Don't need to do anything
struct={bracket, ax:ax, bx:bx, cx:cx, fa:fa, fb:fb, fc:fc}
return

failure:
verbiage, 'Failed to bracket the minimum in '+strtrim(maxtry)+' attempts.', 1, verbose
nan = !values.f_nan
ax = nan
bx = nan
cx = nan
fa = nan
fb = nan
fc = nan
      
end
