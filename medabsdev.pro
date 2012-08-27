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
;  result = medabssdev(data, [/sigma], [/even])
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
;  even: If set and data contains an even number of points,
;  medians are computed as the average of the two middle numbers.
;  The returned values may not be an element of the original data.
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
;  Jul 2012: Added /even keyword. Julio Castro
;-
function medabsdev, data, sigma = sigma, median = median, even=even

  compile_opt idl2
  on_error, 2

  ;- check inputs
  if n_params() eq 0 then begin
     print, 'medabsdev calling sequence:'
     print, 'result = medabsdev(data, [/sigma, /even])'
     return, !values.f_nan
  endif

  med = median(data, even=keyword_set(even)) & median = med
  absdev = abs(data - med)
  result = median(absdev, even=keyword_set(even))

  if keyword_set(sigma) then result /= 0.6744897501960817D
  return, result
end

pro test

  assert, medabsdev([1, 2, 3]) eq 1
  assert, medabsdev([1, 1]) eq 0
  assert, medabsdev([1, 2, 3], /sigma) eq 1 / 0.6744897501960817D
  assert, medabsdev([1, 2], /even) eq 0.5
  assert, medabsdev([1, 2]) eq 1
  print, 'tests pass'
end
