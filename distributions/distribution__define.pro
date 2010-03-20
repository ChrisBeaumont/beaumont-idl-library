;+
; PURPOSE:
;  The distribution class is an interface for objects dealing with
;  probability distributions.
;
; CATEGORY:
;  Statistics
;
; NOTE:
;  Methods are not implemented in this interface, and trying to 
;  call any method on a raw distribution object will generate and
;  error message. Subclasses should override the methods CDF and PDF.
;
; MODIFICATION HISTORY:
;  Sep 2009: Written by Chris Beaumont.
;-


;+ 
; PURPOSE:
;  Return the probability density function for this distribution, as a
;  function of the variable x
;
; CATEGORY:
;  statistics
;
; INPUTS:
;  x: The input point. Scalar or array
;
; OUTPUTS:
;  The probability density at this point
;-
function distribution::pdf, x
  message, 'Method not implemented in the distribution interface'
  return, -1
end


;+
; PURPOSE:
;  Return the cumulative distribution funcion P(x' < x) for
;  this distribution. 
;
; CATEGORY:
;  statistics
;
; INPUTS:
;  x: The input point. Scalar.
;
; OUTPUTS:
;  The cumulative distribution P(x' < x)
; 
; NOTE:
;  The cdf function should only be used for 1-dimensional distribution
;  functions.
;-
function distribution::cdf, x
  message, 'Method not implemented in the distribution interface'
  return, -1
end


;+
; PURPOSE:
;  Define the data structure
;
; CATEGORY:
;  statistics
;-
pro distribution__define
  struct={distribution, dummy: 0}
end
