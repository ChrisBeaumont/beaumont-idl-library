;+
; PURPOSE:
;  This function tests whether a variable's data type is one of
;  the integers (byte, short, long, etc).
;
; INPUTS:
;  data: A variable
;
; OUTPUTS:
;  1 if data is some kind of integer scalar or array
;
; MODIFICATION HISTORY:
;  2010-08-16: Written by Chris Beaumont
;-
function is_integer, data
  sz = size(data, /type)
  return, sz eq 1 || sz eq 2 || sz eq 3 || $
          sz eq 12 || sz eq 13 || sz eq 14 || sz eq 15
end

pro test

  assert, is_integer(3B)
  assert, is_integer(3S)
  assert, is_integer(3L)
  assert, is_integer(3LL)
  assert, is_integer(3US)
  assert, is_integer(3UL)
  assert, is_integer(3ULL)
  assert, ~is_integer(3.)
  assert, ~is_integer(3D)
  assert, ~is_integer({hi:3})
  assert, ~is_integer(obj_new())
  assert, ~is_integer(ptr_new())
  assert, ~is_integer(complex(3))
  assert, ~is_integer(dcomplex(3))
  assert, ~is_integer(undefined)
end
