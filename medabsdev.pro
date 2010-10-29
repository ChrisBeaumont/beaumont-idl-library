;+
; NAME:
;  medabsdev
;
; PURPOSE:
;  This function returns a data set's median absolute deviation
;  from the median. That is, it returns
;    median( |data - median(data) | )
;  It is a proxy for the standard deviation, but is more resistent
;  against outliers. 
;
; CATEGORY:
;  Statistics
;
; CALLING SEQUENCE:
;  result = medabssdev(data, [/sigma])
;
; INPUTS:
;  data: An array of data
;
; KEYWORD PARAMETERS:
;  sigma: If set, divide the median absolute deviation by
;  inverseErf(0.5) * sqrt(2). This scales the MAD to an approximation
;  for sigma (assuming a Gaussian distribution)
;  median: On output, holds the median of the data
;
; OUTPUTS:
;  median( |data - median(data)| )
;
; NOTE:
;  For the gaussian distribution, 
;  medabsdev / sigma = inverseErf(0.5) * sqrt(2) = 0.67449
;
; EXAMPLES:
;  IDL> dist = randomn(seed, 50)
;  IDL> outlier = 1d7
;  IDL> data = [dist, outlier]
;  IDL> print, stdev(dist), stdev(data)
;       1.09757       1400280.1
;  IDL> print, medabsdev(dist), medabsdev(data)
;       0.597564      0.59756410
;
; MODIFICATION HISTORY:
;  Aug 2009: Written by Chris Beaumont
;  Sep 2009: Added /SIGMA keyword. cnb.
;  Oct 2009: Added input checking. cnb.
;  Oct 2010: Added median keyword. cnb.
;-
function medabsdev, data, sigma = sigma, median = median
  
  compile_opt idl2
  on_error, 2
  
  ;- check inputs
  if n_params() eq 0 then begin
     print, 'medabsdev calling sequence:'
     print, 'result = medabsdev(data, [/sigma])'
     return, !values.f_nan
  endif

  med = median(data) & median = med
  absdev = abs(data - med)
  result = median(absdev)
  if keyword_set(sigma) then result /= 0.6744897501960817D
  return, result
end
