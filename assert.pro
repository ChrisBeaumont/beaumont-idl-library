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
;  ASSERT, assertion [, msg]
;
; INPUTS:
;  assertion: A boolean expression believed to be true
;  msg: An optional string to print when the assertion fails
;
; PROCEDURE:
;  The procedure evaluates whether the assertion is true or false. If
;  true, the program exists quietly. If false, the program issues an
;  error message and exits
;
; Modification History:
;  April 2009- Written by Chris Beaumont
;  July 2011- Added msg input. cnb.
;-
pro assert, assertion, msg
compile_opt idl2
on_error, 2

if n_elements(assertion) ne 1 then begin
   message, 'assert argument is not a scalar'
endif

if assertion then return

help,/trace, output = out
if n_elements(msg) ne 0 then $
   message, /con, msg

message, 'Assertion failed at: '+out[1]

end
