;+
; PURPOSE:
;  The procedure swaps two variables
;
; CATEGORIES:
;  utilities
;
; CALLING SEQUENCE:
;  swap, a, b
;
; INPUTS:
;  a: The first variable
;  b: The second variable
;
; OUTPUTS:
;  b contains the old value of a, and vice versa
;
; MODIFICATION HISTORY:
;  June 2009: Written by Chris Beaumont
;-
pro swap, a, b
compile_opt idl2
on_error, 2

temp = a
a = b
b = temp
return
end
