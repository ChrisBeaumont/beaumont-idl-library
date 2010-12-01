;+
; PURPOSE:
;  This function removes the last section from a string
;
; CATEGORY:
;  Utilities
;
; INPUTS:
;  string: A string scalar or array to truncate
;  search: A search string, containing the substring at which
;          to truncate the input strings
;
; OUTPUTS:
;  A string scalar or array, containing the portion of the input
;  strings occuring before the search string.
;
; EXAMPLE:
;  IDL> print, strtrun('test.png', '.png')
;  test
;  IDL> print, strtrun('test.png', 'notfound')
;  test.png
;  IDL> print, strtrun(['1.png','2.jpg'], '.png')
;  1 2.jpg
;
; MODIFICATION HISTORY:
;  Jan 2010: Written by Chris Beaumont
;-
function strtrun, string, search

  compile_opt idl2  
  on_error, 2

  hit = strpos(string, search, /reverse_search)
  good = where(hit ne -1, ct)
  result = string
  for i = 0, ct - 1, 1 do begin
     result[good[i]] = strmid(result[good[i]], 0, hit[i])
  endfor
  return, result
end
