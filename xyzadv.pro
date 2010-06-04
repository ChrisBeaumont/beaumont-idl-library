;+
; PURPOSE:
;  This procedure is an extenion of (and a wrapper for) the xyad
;  program in the astronomy user's library. It converts (x,y,z)
;  pixel coordinates to (a,d,v) sky / velocity coordinates for a 3D
;  data cube, based on the fits header.
;
; INPUTS:
;  head: A fits header. String array
;     x: x pixel coordinates. scalar or vector
;     y: y pixel coordinates. scalar or vector
;     z: z pixel coordinates. scalar or vector
; 
; OUTPUTS:
;  a: x sky coordinate (e.g. ra or lon).
;  d: y sky coordinate (e.g. dec or lat).
;  v: z sky coordinate (e.g. vel or freq).
;
; PROCEDURE:
;  This procedure calls extast3 from the Beaumont IDL library and xyad
;  from the IDL Astronomy User's Library.
;
; MODIFICATION HISTORY:
;  March 2010: Written by Chris Beaumont
;  May 2010: Fixed a bug in the call to xy2ad. cnb.
;-  
pro xyzadv, head, x, y, z, a, d, v

  ;- check inputs
  if n_params() ne 7 then begin
     print, 'calling sequence'
     print, 'xyzadv, head, x, y, z, a, d, v'
     return
  endif

  if n_elements(head) eq 0 || size(head, /tname) ne 'STRING' then $
     message, 'head must be a string array'
  sz = n_elements(x)
  if sz eq 0 then message, 'x,y,z must be non-empty'
  if n_elements(y) ne sz || n_elements(z) ne sz then $
     message, 'x, y, z must have the same number of elements'

  extast3, head, struct
  xy2ad, x, y, *struct.extast, a, d

  v = struct.crval3 + (z + 1 - struct.crpix3) * struct.cdelt3
  extast3, head, struct, /delete
  return
end
  
