;+
; PURPOSE:
;  Shift the values associated by up to 5 variables
;
; CATEGORIES:
;  utilities
;
; CALLING SEQUENCE:
;  shift, a, b, [c, d, e]
;
; INPUTS:
;  a: The first variable
;  b: The second variable
;
; OPTIONAL INPUTS:
;  c: The third variable
;  d: The fourth variable
;  e: The fifth variable
;
; OUTPUTS:
;  The value of a is shifted to b. The value of b is shifted to c, and
;  so on. The value of the final supplied variable is shifted to a.
;
; MODIFICATION HISTORY:
;  June 2009: Written by Chris Beaumont
;-
pro shift, a, b, c, d, e
compile_opt idl2
on_error, 2

narg = n_params()
case narg of 
   2: begin
      temp = b
      b = a
      a = temp
      return
   end
   3: begin
      temp = c
      c = b
      b = a
      a = temp
      return
   end
   4: begin
      temp = d
      d = c
      c = b
      b = a
      a = temp
      return
   end
   5: begin
      temp = e
      e = d
      d = c
      c = b
      b = a
      a = temp
      return
   end
endcase

return
end
