;+
; PURPOSE:
;  This function computes the rank of each element in a data set. By default,
;   The rank 0 element is the smallest element, the rank 1 element is
;  second smallest, etc. If decrease keyword is set, the rank 0 element is
;   the largest element.
;
; INPUTS:
;  data: An array of numbers
;
; OUTPUTS:
;  An array the same size as data, giving the rank of each element.
;
; KEYWORDS:
;  decrease: set it if ranked in decrease order.
;
; MODIFICATION HISTORY:
;  2010-08-03: Written by Chris Beaumont
;- 2011-03-22: Updated by Jian Wu.
;              - Add decrease keyword
function rank, data,decrease=decrease
  if n_elements(data) eq 0 then begin
     print, 'calling sequence:'
     print, ' result = rank(data)'
     return, !values.f_nan
  endif

  if keyword_set(decrease) eq 1 then begin
    s = reverse(sort(data))
  endif else begin
    s = sort(data)
  endelse
  ind = lindgen(n_elements(data))
  result = ind*0
  result[s] = ind

  return, result
end
