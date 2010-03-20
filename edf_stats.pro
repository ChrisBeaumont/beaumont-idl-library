;+
; PURPOSE:
;  This function calculates a variety of statistics to characterize
;  the discrepancy between 1D data and a model for the distribution
;  from which the data were drawn. Each statistic is based on the
;  empirical distribution function (i.e. the cdf of the data). Such
;  statistics can be used to evaluate whether a model distribution is
;  consistent with the data.
;
;  In what follows, Fn(x) is the empirical distribution function (edf)
;  and F(x) is the model cdf. Currently, four statistics are
;  implemented:
;
;  1) The Kolmogorov-Smirnov Statistic: max(|F(x) - Fn(x)|)
;  2) An Anderson-Darling style statistic:
;       Mean( (Fn(x) - F(x))^2 / (F(x) * (1 - F(x))) )
;  3) The Kuyper Statistic: max(F(x) - Fn(x)) + max(Fn(x) - F(x))
;  4) The mean absolute deviation: 
;       Mean( |Fn(x) - F(x)| )
;
;  Note that statistic 2 is designed to be more sensitive to
;  discrepancies at low and high values of x than is the KS stat
;
;  Note also that the Kuyper statistic is meant to be used for values
;  of x wrapped onto a circle. See Numerical Recipes.
;
; CATEGORY
;  Statistics
;
; CALLING SEQUENCE:
;  result = edf_stats(data, model, [ks = ks, ad = ad, ky = ky, mad = mad])
;         or
;  result = edf_stats(data, model, [/ks or /ad or /ky, /mad])
;
; INPUTS:
;  data: A vector of data values
;
;  model: The string name of a function which calculates the cdf of
;         the model distribution. The function must have the calling
;         sequence result = model(x, _extra = extra), and must be
;         written to handle x as a scalar or vector. Extra keywords to
;         edf_stats will be passed to this function.
; 
; KEYWORD PARAMETERS:
;  ks: If non zero or set to a named variable, will calculate and
;  return the ks statistic to that variable.
;  
;  ad: Same as above, for the Anderson-Darling statistic.
;
;  ky: Same as above, for the Kuyper statistic
;
;  mad: Same as above, for the mediat absolute deviation
;
; OUTPUTS:
;  If only one of ks, ad, ky, or mad are set, then the return value is
;  that particular statistic. Otherwise, the KS statistic is returned.
;
; SEE ALSO:
;  edf
;
; MODIFICATION HISTORY
;  June 2009: Written by Chris Beaumont
;  July 2009: Added mad statistic
;-
function edf_stats, data, model, ks = ks, ad = ad, ky = ky, mad = mad, $
                    _extra = extra
compile_opt idl2
;- on_error, 2

;- check inputs
if n_params() ne 2 then begin
   print, 'edf_stats calling sequence:'
   print, 'result = edf_stats(data, model, [/ks, /ad, /ky, /mad'
   print, '                   _extra = extra])'
   print, '   or '
   print, ' result = edf_stats(data, model, [ks = ks, '
   print, '                    ad = ad, ky = ky, mad = mad, _extra = extra])'
   return, !values.f_nan
endif

sz = n_elements(data)
if sz lt 2 then $
   message, 'data must contain at least two elements'
if size(model, /type) ne 7 then $
   message, 'model must be a string'

doKS  = keyword_set(ks)  || arg_present(ks)
doAD  = keyword_set(ad)  || arg_present(ad)
doKY  = keyword_set(ky)  || arg_present(ky)
doMAD = keyword_set(mad) || arg_present(mad)

;- get the edf
edf, data, data_x, edf
cdf = call_function(model, data_x, _extra = extra)
delta = edf - cdf

ks = max(abs(delta), /nan)
ky = max(delta, /nan) + max(-delta, /nan)
ad = delta^2 / (cdf * (1 - cdf))
ad = total(ad, /nan) / total(finite(ad))
mad = total(abs(delta), /nan) / total(finite(delta))

;- what should we return?
if doAD  && ~doKS && ~doKY  && ~doMAD then return, ad
if doKY  && ~doAD  && ~doKS && ~doMAD then return, ky
if doMAD  && ~doAD  && ~doKS && ~doKY then return, mad

return, ks

end
