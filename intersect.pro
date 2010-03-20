;+
; PURPOSE:
;  Intersect returns the intersection of two 1D, interger-valued
;  vectors. That is, given vec1 and vec2, intersect returns the values
;  common to both sets of data.
;
;  Intersect has three methods to find the intersection. One uses
;  histograms, and is optimized for dense vectors (i.e. the range of
;  values in vec1 and vec2 is much larger than the number of elements
;  in those vectors). The second method uses value_locate, and is more
;  efficient when the input vectors are sparse (the range of values
;  greatly exceeds the number of elements). 
;
;  The method to use can be chosen manually. If not, the program makes
;  a guess at which process is faster based on the data. Some
;  lightweight testing on my machine suggests that, for range /
;  n_elements >~ 30, the sparse method is faster.
;
; CATEGORY:
;  Utilities
;
; CALLING SEQUENCE:
;  result = intersect(vec1, vec2 [count = count, verbose = verbose, 
;                     /sparse, /dense)
;
; INPUTS:
;  vec1: A one-dimensional vector. It must be one of the integer data
;        types (byte, int, long, uint, ulong, ulong64, or long64)
;  vec2: A one-dimensional vector. It must be one of the integer data
;        types.
;
; KEYWORD PARAMETERS:
;  count: Set to a variable to hold the number of intersection elements.
;  verbose: Set to a number (1-3) to produce textual output with
;           varying levels of detail. See VERBOSITY for details.
;  sparse: Force intersect to find the inersection using the method
;          optimized for sparse data sets.
;  dense:  Force intersect to find the intersection using the method
;          optimized for dense data sets
;
; OUTPUTS:
;  The values common to both vec1 and vec2. If the two sets are
;  mutually exclusive, the routine returns NAN
;
;-
function intersect, vec1, vec2, $
                    count = count, $
                    sparse = sparse, dense = dense, $
                    verbose = verbose
  compile_opt idl2
  on_error, 2

  ;- check inputs
  if n_params() ne 2 then begin
     print, 'intersect calling sequence:'
     print, '  result = intersect(vec1, vec2, [verbose = verbose, /sparse, /dense])'
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
  if size(vec2, /n_dim) ne 1 then message, 'second vector not a 1D array'

  ;- determine which method to use
  lohi = minmax([minmax(vec1), minmax(vec2)])
  nelem = n_elements(vec1) + n_elements(vec2)
  density = 1D * nelem / (lohi[1] - lohi[0])
  if keyword_set(sparse) then goto, sparse
  if keyword_set(dense) then goto, dense

  ;- extrema of data
  if density lt 3d-1 then goto, sparse

 
  ;- method 1: histograms
  dense:
  verbiage, 'Using histogram method', 3, verbose
  h1 = histogram(vec1, min = lohi[0], loc = loc)
  h2 = histogram(vec2, min = lohi[0])
  int = where((h1 gt 0) and (h2 gt 0), count)
  return, count eq 0 ? !values.f_nan : loc[int]
  
                                ;- method 2: histograms + value_locate
  sparse:
  verbiage, 'Using value_locate method', 3, verbose

  ;- need to hand the special case of 1-element vectors
  if n_elements(vec1) eq 1 then begin
     hit = where(vec2 eq vec1[0], count)
     return, count eq 0 ? !values.f_nan : vec1
  endif
 
  sort = vec1[sort(vec1)]
  vals = value_locate(sort, vec2)
  int = where(sort[vals] eq vec2, count)
  if count eq 0 then return, !values.f_nan
  result = vec2[int]
  return, result[uniq(result, sort(result))] 

end

