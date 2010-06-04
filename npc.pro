;+
; PURPOSE:
;  This function returns the various ASCII non-printing
;  characters. See the "non-printing characters" IDL help page for
;  more information.
;
; INPUT:
;  name: A string naming the character to return. 
;
; KEYWORD PARAMETERS:
;  print: Set to print the possible names to the screen
;
; RETURNS:
;  The ASCII character associated with name
;
; MODIFICATION HISTORY:
;  June 2010: Written by Chris Beaumont
;-
function npc, name, print = print
  chars=['TAB', 'NEWLINE', 'BELL', 'BACKSPACE', 'HORIZONTAL TAB', 'LINEFEED', $
         'VERTICAL TAB', 'FORMFEED', 'CARRIAGE RETURN', 'ESCAPE']
  vals=[9B, 10B, 7B, 8B, 9B, 10B, 11B, 12B, 13B, 27B]

  if n_elements(name) eq 0 || keyword_set(print) then begin
     print, 'calling sequence'
     print, ' result = npc(name)'
     print, ' possible names:'
     print, chars
     return, !values.f_nan
  endif

  index = where(strupcase(name) eq chars, ct)

  if ct eq 0 then begin
     print, 'name not recognized. Possible names:'
     print, chars
     return, !values.f_nan
  endif
  return, string(vals[index[0]])
end
