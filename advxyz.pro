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
; KEYWORD PARAMETERS:
;  ortho: Set this keyword if the third axis of the input coordinates
;         is orthogonal to the first two; that is, if adv describe a
;         cube, where a and d are constant across the cube. When this
;         is true, the computation can be performed more quickly.
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
;  May 2010: Fixed a bug in call to ad2xy. cnb.
;  Sep 2010: Fixed a bug when testing whether head is a string
;            array. cnb.
;  June 2011: Optimized for the case where a,d,v describe a cube with
;             a,d constant along slices. Added ortho keyword. cnb.
;-
pro advxyz, head, a, d, v, x, y, z, ortho = ortho
  compile_opt idl2

  ;- check inputs
  if n_params() ne 7 then begin
     print, 'calling sequence'
     print, 'advxyz, head, a, d, v, x, y, z'
     return
  endif

  if n_elements(head) eq 0 || $
     size(head, /tname) ne 'STRING' || (size(head, /n_dimen) ne 1) $
  then message, 'HEAD must be a string array'
  sz = n_elements(a)
  if sz eq 0 || n_elements(d) ne sz || n_elements(v) ne sz then $
     message, 'a,d,v must have the same number of elements'
  if ~arg_present(x) || ~arg_present(y) || ~arg_present(z) then $
     message, /info, 'Warning: output variables not provided'

  ;- do the work
  extast3, head, struct

  ;- special case: xyz describe a cube, with xy identical
  ;- at each slice
  sz = size(v)
  if keyword_set(ortho) || $
     (sz[0] eq 3 && array_equal(max(a, min=lo, dim=3), lo) && $
     array_equal(max(d, min=lo, dim=3), lo)) then begin
     aa = reform(a[*,*,0])
     dd = reform(d[*,*,0])
     ad2xy, aa, dd, *struct.extast, x, y
     x = rebin(x, sz[1], sz[2], sz[3])
     y = rebin(y, sz[1], sz[2], sz[3])
  endif else $       
     ad2xy, a, d, *struct.extast, x, y

  z = struct.crpix3 + (v - struct.crval3) / struct.cdelt3 - 1
  extast3, head, struct, /delete
  return
end
  
