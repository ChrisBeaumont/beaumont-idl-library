;+ 
; PURPOSE:
;  Plot which objects were flagged as outliers from detectoutlier.
;
; CATEGORY:
;  Display utilities.
;
; INPUTS:
;  x: Input x points
;  y: Input y points
;  result: A vector of [0,1] to denote [outlier/non-outlier]
;-
pro detectoutlier_plot, x, y, result
  good = where(result, ngood, complement = bad, ncomp = nbad)
  plot,[0],[0], /nodata, xra= minmax(x), yra = minmax(y)
  if ngood ne 0 then begin
     oplot, x[good], y[good], psym = 4, color = fsc_color('green')
  endif
  if nbad ne 0 then begin
     oplot, x[bad], y[bad], psym = 4, color = fsc_color('red')
  endif
end

;+
; PURPOSE:
;  This function attempts to identify outlier data points in 2D
;  space. It proceeds as follows:
;    1) Calculate the center position of all points
;    2) Remove the biggest outlier, and recalculate the center and
;    standard deviation.
;    3) If the outlier removed in step 2 deviates from the new center
;    by more than THRESH * stdev, go to step 1 and repeat. Otherwise,
;    re-insert the outlier flagged in step 2 as a legitimate data
;    point, and return.
;
; CATEGORY:
;  statistics
;
; CALLING SEQUENCE:
;  result = detectoutlier(x, y, [status, THRESH = thresh, /VERBOSE, /PLOT])
; 
; INPUT:
;  x: The x coordinates - a vector
;  y: The y coordinates - a vector
;  
; OUTPUT:
;  status: The status of the algorithm. Returns 0 for successful
;  completion, and 1 for failure. If status = 1, all points will be
;  flagged as legitimate.
;
;  A vector of 1 (good data) and 0 (outlier), flagging each input
;  (x,y)
;
; KEYWORD PARAMETERS:
;  THRESH: Use to manually set the threshhold for outlier
;          detection. The default is 3.5
;  VERBOSE: Output textual information
;  PLOT: Plot the points, color coded by whether they are outliers or not.
;
; MODIFICATION HISTORY:
;  March 2009 Written by Chris Beaumont
;-
function detectoutlier, x, y, status, THRESH = thresh, VERBOSE = verbose, PLOT = plot
compile_opt idl2
on_error, 2

;- check inputs
npar = n_params()
nx = n_elements(x)
ny = n_elements(y)
if npar lt 2 || npar gt 3 then begin
   print, 'detectoutlier calling sequence:'
   print, 'result = detectoutlier(x, y, [status, THRESH = thresh, /VERBOSE, /PLOT])'
   return, !values.f_nan
endif

if nx ne ny then message, 'Input x and y vectors must be the same length'
if nx lt 5 then message, 'x and y must contain at least 5 elements'

;- set up variables
if n_elements(THRESH) eq 0 then THRESH = 3.5
npts = nx
use = bytarr(npts) + 1

;- flag nans
bad = where(~ (finite(x) and finite(y)), bct)
if bct ne 0 then use[bad] = 0
default = use

best = where(use, complement = worst, nc = wct)
mx = mean(x[best],/nan)
my = mean(y[best],/nan)
d2 = (x-mx)^2 + (y-my)^2

;- iteratively test for rejection
while 1 do begin
;- find the biggest outlier not yet flagged as an outlier
   if (wct ne 0) then d2[worst] = 0
   top = max(d2, candidate)
   
;- see if this point is a significant outlier to the remaining
;  measurements
   use[candidate] = 0
   best = where(use, bct, complement=worst, nc = wct)
   
;-if we've rejected too many points, give up and flag
;-everything as good
   if bct le 2 then begin
      if keyword_set(verbose) then $
         message, 'Too many outliers rejected. Aborting', /continue
      status = 1
      return, default
   endif
   
   mx = mean(x[best],/nan)
   my = mean(y[best],/nan)
   
;-XXX use an ellipse
   xrms = stdev(x[best])
   yrms = stdev(y[best])
   dx = abs((x-mx)) / xrms
   dy = abs((y-my)) / yrms
   
   if (dx[candidate] > dy[candidate]) le THRESH then begin
   ;- not an outlier. reset the flag and break
      use[candidate] = 1
      break
   endif
endwhile

if keyword_set(verbose) then begin
   nrej  = total(~use)
   ntot = n_elements(use)
   ndigit = strtrim(floor(alog10(ntot)+1),2)
   print, nrej, ntot, $
        format = '("Success. Rejected ", i'+ndigit+', " of ", i'+ndigit+'," outliers")'
endif
status = 0
if keyword_set(plot) then detectoutlier_plot, x, y, use
return, use

end
