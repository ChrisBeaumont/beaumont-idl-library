;+
; PURPOSE:
;  This procedure is a simple routine to check assumptions during
;  programming. If, at any time, an assertion fails to be true, the
;  program halts with an error message.
;
; CATEGORY:
;  Utilities
;
; CALLING SEQUENCE:
;  ASSERT, assertion
;
; INPUTS:
;  assertion: A boolean expression believed to be true
;
; PROCEDURE:
;  The procedure evaluates whether the assertion is true or false. If
;  true, the program exists quietly. If false, the program issues an
;  error message and exits
;
; Modification History:
;  April 2009- Written by Chris Beaumont
;-
pro assert, assertion
compile_opt idl2
on_error, 2

if n_elements(assertion) ne 1 then begin
   message, 'assert argument is not a scalar'
endif

if assertion then return

help,/trace, output = out
message, 'Assertion failed at: '+out[1]

end
