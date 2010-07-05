;-
; DESCRIPTION:
;  This function flattens a string array into a scalar string. Each
;  row of the original array is copied into the new string, and
;  separated by newline characters.
;
; CATEGORY:
;  string processing
;
; INPUTS:
;  string: A string array
;
; OUTPUTS:
;  A scalar string, containing each row of string separated by newline
;  characters.
;
; KEYWORD PARAMETERS:
;  separator: Set to a scalar string to separate rows in the input
;  array by this, instead of newline characters.
;
; MODIFICATION HISTORY:
;  Jan 2010: Written by Chris Beaumont
;  July 2010: Added separator keyword. cnb
;-
function strflat, string, separator = separator
  nl = n_elements(separator) ne 0 ? separator : string(10B)
  sz = n_elements(string)
  result=''
  for i = 0, sz - 1, 1 do result += string[i]+nl
  return, result
end
