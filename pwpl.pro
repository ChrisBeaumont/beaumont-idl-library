;+
; PURPOSE:
;  This function evaluates, or returns samples drawn at random from, a
;  PieceWise Power Law distribution.
;
; CATEGORY:
;  Statistics
;
; CALLING SEQUENCE:
;  result = pwpl(xs, alphas, x = x, random = random)
;
; INPUTS:
;  xmins: A vector specifying the abscissas of the powerlaw boundaries in
;      increasing order. These are the lower bounds for each piecewise
;      segment. 
;  alphas: The power law exponents at each of the boundaries
;
; KEYWORD PARAMETERS:
;  x: A set of abscissas against which to evaluate the power law
;  random: Set to an integer n to return n samples drawn from this distribution
;
; OUTPUTS:
;  If x is set, the function returns f(x). If random is set, the
;  function returns a set of numbers  at random from dN/dx = f(x).
;  f(x) is given by the following equation
;   dN / dx = Const * x^alpha[i], for x[i] <= x < x[i+1]
;  If x < x[0], then dN / dx = 0
;
; SEE ALSO:
;  imf
;
; TODO:
;  Implement RANDOM keyword
;
; MODIFICATION HISTORY:
;  June 2009: Written by Chris Beaumont
;-
function pwpl, xmins, alphas, x = x, random = random
  compile_opt idl2
  ;-on_error, 2

  ;- check inputs
  if n_params() ne 2 then begin
     print, 'pwpl calling sequence:'
     print, 'result = pwpl(xmins, alphas x = x, random = random)'
     return, !values.f_nan
  endif

  sz = n_elements(xmins)
  if n_elements(alphas) ne sz then $
     message, 'xmins and alphas are not the same size'

  if keyword_set(random) then message, $
     'random keyword not yet implemented'
  if keyword_set(random) && keyword_set(x) then $
     message, 'cannot choose both x and random keywords'

  ;- make sure that xmin is in sorted order
  shift = shift(xmins, 1)
  shift[0] = 0
  if min(xmins - shift) lt 0 then $
     message, 'xmins are not listed in increasing order'

  if xmins[0] lt 0 then $
     message, 'xmins must be positive'

  ;- make sure that the piecewise power law is normalizeable 
 if alphas[0] le -1 || alphas[sz-1] ge -1 then $
     message, 'piecewise power law is not normalizeable'

  ;- determine scaling exponents for continuity
  cs = dblarr(sz) + 1
  for i = 1, sz-1, 1 do begin
     if xmins[i-1] eq 0 then cs[i] = cs[i-1] else $
        cs[i] = cs[i-1] * (xmins[i] / xmins[i-1])^(alphas[i-1])
  endfor

  ;- determine normalization factor
  norm = 0D
  for i = 0, sz-2, 1 do begin
     if xmins[i] eq 0 then begin
        norm += cs[i] * (xmins[i+1] - xmins[i])
     endif else begin
        norm += cs[i] / (alphas[i] + 1) * ((xmins[i+1]/xmins[i])^(alphas[i] + 1) - 1)
     endelse
  endfor

  norm -= cs[sz-1] / (alphas[sz-1] + 1)
  cs /= norm
  ;- if x keyword is supplied, then evaluate function
  if n_elements(x) ne 0 then begin
     result = x * 0
     for i = 0, sz - 1, 1 do begin
        hit = where(x ge xmins[i], ct)
        if ct eq 0 then continue
        result[hit] = cs[i] * (x[hit] / xmins[i])^(alphas[i])
     endfor
     return, result
  endif else begin
  ;- XXX add this 
     message, 'random sampling form pwpl not yet implemented'
  endelse

end
