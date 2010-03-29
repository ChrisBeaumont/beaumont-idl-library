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
; MODIFICATION HISTORY:
;  Jan 2010: Written by Chris Beaumont
;-
function strflat, string
  nl = string(10B)
  sz = n_elements(string)
  result=''
  for i = 0, sz - 1, 1 do result += string[i]+nl
  return, result
end
