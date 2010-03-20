;+
; PURPOSE:
;  This function returns the union of two vectors. The union is the
;  set of numbers present in either vector.
;
; CATEGORY:
;  Utilities
;
; CALLING SEQUENCE:
;   result = union(vec1, vec2)
;
; INPUTS:
;   vec1: A 1D vector of integers
;   vec2: A 1D vector of integers
;
; OUTPUTS:
;   A 1D vector of the integers present in either vec1 or vec2
;
; MODIFICATION HISTORY:
;  July 2009: Written by Chris Beaumont
;-
function union, vec1, vec2
  if n_params() ne 2 then begin
     print, 'union calling sequence:'
     print, '  result = union(vec1, vec2)'
     return, !values.f_nan
  endif

  ;- make sure vec1 and vec2 are integers
  type = size(vec1, /tname)
  case type of
     'BYTE':
     'INT':
     'LONG':
     'UINT':
     'ULONG':
     'LONG64':
     'ULONG64': break
     else: message, 'first vector not an integer data type'
  endcase
  if size(vec1, /n_dim) ne 1 then message, 'first vector not a 1D array'

  type = size(vec2, /tname)
  case type of
     'BYTE':
     'INT':
     'LONG':
     'UINT':
     'ULONG':
     'LONG64':
     'ULONG64': break
     else: message, 'second vector not an integer data type'
  endcase

  result = [vec1, vec2]
  return, result[uniq(result, sort(result))]
end
