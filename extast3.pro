;+
; PURPOSE:
;  This program is a wrapper procedure to the extast routine in the
;  IDL astronomy user's library. extast3 is designed to handle
;  data cubes, which extast cannot handle. This procedure is used by
;  sky2pix to convert between sky and pixel coordinates in data cubes.
;
; CATEGORY:
;  Astrometry
;
; CALLING SEQUENCE:
;  extast3, hdr, result, [/delete]
;
; INPUTS:
;  hdr: A string array, containing a fits header. The header must
;  describe a 2 or 3 dimensional fits file. If the fits file is 3D,
;  all information related to 2d position on the sky must be contained
;  within the first 2 dimensions of the fits file (in otherwords, the
;  fits axes cannot be rotated such that spatial information lies
;  along the third axis. This would be pretty non-standard, however).
;
; OUTPUTS:
;  result: A structure containing the following fields:
;    extast: A pointer to the result of calling extast on hdr. If hdr
;    describes a data cube, extast is 'tricked' into thinking hdr
;    actually describes a 2d images. Extast contains all of the 2D sky
;    astrometry data.
;    cdelt3
;    crpix3
;    crval3
;
; KEYWORD PARAMETERS:
;  delete: Since result contains a pointer, these should be properly
;  de-allocated at the end of use. Set the delete keyword to free the
;  extast pointer and delete the result variable
;
; SEE ALSO:
;  sky2pix
;
; MODIFICATION HISTORY:
;  June 2009: Written by Chris Beaumont
;  March 2010: Added input checking. cnb.
;-
pro extast3, hdr, result, delete = delete
  compile_opt idl2
  on_error, 2

  ;- check inputs
  if n_params() eq 0 then begin
     print, 'calling sequence'
     print, 'extast3, hdr, result, [/delete]'
     return
  endif

  if n_elements(hdr) eq 0 || size(hdr,/tname) ne 'STRING' $
     then message, 'hdr must be a string array'
  
  if keyword_set(delete) then begin
     if n_elements(delete) eq 0 then return
     names = tag_names(result)
     hit = where(names eq 'EXTAST', ct)
     if ct eq 1 then ptr_free, result.extast
     ;- delete the result variable
     junk = temporary(result)
     return
  endif
  
  result = {extast3}

  naxis = sxpar(hdr, 'naxis', count = ct)
  if ct ne 1 then $
     message, 'cannot find naxis keyword in supplied header'

  ;- easy case: no third dimension
  if naxis eq 2 then begin
     extast, hdr, ast
     result = {extast3, extast: ptr_new(ast), $
               crval3 : 0, crpix3 : 0, cdelt3 : 0}
  ;- hard case; must parse third imension info
  endif else begin
     temp = hdr
     sxdelpar, temp, 'naxis3'
     sxaddpar, temp, 'naxis', 2
     extast, temp, ast
    
     cdelt3 = sxpar(hdr, 'cdelt3', count = ct)
     if ct eq 0 then begin
        cdelt3 = sxpar(hdr, 'cd3_3', count = ct)
        if ct eq 0 then message, 'cannot find cdelt3 or cd3_3 in header'
     endif
     
     crval3 = sxpar(hdr, 'crval3', count = ct)
     if ct eq 0 then message, 'cannot find crval3 in header'
     crpix3 = sxpar(hdr, 'crpix3', count = ct)
     if ct eq 0 then message, 'cannot find crpix3 in header'
     
     
     result = {extast3, extast: ptr_new(ast), $
               crval3 : crval3, crpix3 : crpix3, cdelt3:cdelt3 }
  endelse

end
