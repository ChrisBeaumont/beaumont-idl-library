;+
; PURPOSE:
;  This procedure converts from (a,d,v) sky coordinates to (x,y,z)
;  pixel coordinates in a data cube. It is a wrapper to ad2xy in the
;  IDL Astronomy User's Library, which does not handle 3D data.
;
; INPUTS:
;  head: A fits header. String array
;  a: List of sky x coordinates (alpha, lon). Scalar or vector
;  d: List of sky y coordinates (dec, lat). Scalar or vector
;  v: List of sky z coordinates (velocity, freq). Scalar or vector.
; 
; OUTPUTS:
;  x: Pixel x coordinates. These are zero-indexed, IDL style.
;  y: Pixel y coordinates
;  z: Pizel z coordinates
;
; PROCEDURE:
;  This function calls ad2xy, and hence can handle any wcs stuff that
;  that procedure can. The only restriction is that the fits header
;  have a simple description for transforming from v->z. That is,
;  CDELT3/CD3_3, CRPIX3, CRVAL3, keywords should completely define the
;  transformation. This is almost always the case. 
;
; MODIFICATION HISTORY:
;  March 2010: Written by Chris Beaumont
;-
pro advxyz, head, a, d, v, x, y, z
  compile_opt idl2

  ;- check inputs
  if n_params() ne 7 then begin
     print, 'calling sequence'
     print, 'advxyz, head, a, d, v, x, y, z'
     return
  endif

  if n_elements(head) eq 0 || $
     size(head, /tname) ne 'STRING' || size(head, /n_dimen) $
  then message, 'HEAD must be a string array'
  sz = n_elements(a)
  if sz eq 0 || n_elements(d) ne sz || n_elements(v) ne sz then $
     message, 'a,d,v must have the same number of elements'
  if ~arg_present(x) || ~arg_present(y) || ~arg_present(z) then $
     message, /info, 'Warning: output variables not provided'

  ;- do the work
  extast3, head, struct
  ad2xy, a, d, struct.extast, x, y

  z = struct.crpix3 + (v - struct.crval3) / struct.cdelt3 - 1
  extast3, head, struct, /delete
  return
end
  
