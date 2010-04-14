;+
; PURPOSE:
;  This function is a wrapper to the builtin REFORM procedure. It
;  reforms an array to match the shape of template array.
;
; INPUTS:
;  array: The array to reshape. Can be any dimension.
;  template: The array whose shape you wish to match
;
; KEYWORD PARAMETERS:
;  over: If set, array is overwritten on output. This can save
;        significant time and memory with lines like
;        'a = reshape(a, template, /over)' if a is large
;
; OUTPUTS:
;  A reformed version of array whose shape matches that of template.
;
; SIDE EFFECTS:
;  The structure of array is changed if /over is set
;
; MODIFICATION HISTORY
;  April 2010: Written by Chris Beaumont
;-
function reshape, array, template, over = over
  compile_opt idl2
  on_error, 2

  ;- check inputs
  if n_params() ne 2 then begin 
     print, 'calling sequence'
     print, 'result = reshape(array, template, [/over])'
     return, !values.f_nan
  endif

  if n_elements(array) ne n_elements(template) then $
     message, 'reshape must not change number of elements in array'

  nd = size(template, /n_dim)
  sz = size(template)

  case nd of
     1 : return, reform(array, sz[1], over = keyword_set(over))
     2 : return, reform(array, sz[1], sz[2], over = keyword_set(over))
     3 : return, reform(array, sz[1], sz[2], sz[3], over = keyword_set(over))
     4 : return, reform(array, sz[1], sz[2], sz[3], $
                        sz[4], over = keyword_set(over))
     5 : return, reform(array, sz[1], sz[2], sz[3], $
                        sz[4], sz[5], over = keyword_set(over))
     6 : return, reform(array, sz[1], sz[2], sz[3], $
                        sz[4], sz[5], sz[6], over = keyword_set(over))
     7 : return, reform(array, sz[1], sz[2], sz[3], $
                        sz[4], sz[5], sz[6], sz[7], over = keyword_set(over))
     8 : return, reform(array, sz[1], sz[2], sz[3], sz[4], $
                        sz[5], sz[6], sz[7], sz[8], over = keyword_set(over))
  endcase
  ;- can't ever get here
  message, 'bug in reshape!'
end

pro test

  ;- test out input dimensions. Should all be array[256]
  print, 'Test 1: Should all be array[256]'
  template = fltarr(256)
  help, reshape(findgen(256), template)
  help, reshape(findgen(128, 2), template)
  help, reshape(findgen(64, 2, 2), template)
  help, reshape(findgen(32, 2, 2, 2), template)
  help, reshape(findgen(16, 2, 2, 2, 2), template)
  help, reshape(findgen(8, 2, 2, 2, 2, 2), template)
  help, reshape(findgen(4, 2, 2, 2, 2, 2, 2), template)
  help, reshape(findgen(2, 2, 2, 2, 2, 2, 2, 2), template)
  
  ;- test template dimensions
  print, 'Test 2: Should all be different'
  input = template
  help, reshape(input, findgen(256))
  help, reshape(input, findgen(128, 2))
  help, reshape(input, findgen(64, 2, 2))
  help, reshape(input, findgen(32, 2, 2, 2))
  help, reshape(input, findgen(16, 2, 2, 2, 2))
  help, reshape(input, findgen(8, 2, 2, 2, 2, 2))
  help, reshape(input, findgen(4, 2, 2, 2, 2, 2, 2))
  help, reshape(input, findgen(2, 2, 2, 2, 2, 2, 2, 2))

  ;- test /over keyword for memory saving
  num = 20000LL
  a1 = intarr(num^2)
  a2 = intarr(num, num)
  print, 'Test 3: /over keyword'
  print, 'Without /over'
  before = memory(/current)
  t0 = systime(/seconds)
  a2 = reshape(a2, a1)
  mem = (memory(/high) - before )
  t1 = systime(/seconds)
  print, 'Operation took '+time2string(t1 - t0)
  print, '    and used '+strtrim(mem,2)+' Bytes of memory'
  help, a2
  print, '****With /over'
  a2 = intarr(num, num)
  before = memory(/current)
  t0 = systime(/seconds)
  a2 = reshape(a2, a1,/over)
  help, a2
  after = memory(/high)
  mem = (memory(/high) - before)
  t1 = systime(/seconds)
  print, 'Operation took '+time2string(t1 - t0)
  print, '    and used '+strtrim(mem,2)+' Bytes of memory'
  print, 'With /over'

  
end
