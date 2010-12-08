;+
; PURPOSE:
;  This function creates a blank image and fits header, based on input
;  parameters describing the desired image size and orientation.
;
; KEYWORD PARAMETERS:
;  center: A 2 element array specifying the sky location of the center
;          pixel. The coordinate system can be specified with
;          COORDSYS. This keyword is required.
;  width: The width of the image in DEGREES. A one or two element
;         vector. If width is not specified, both pixwidth and
;         pixscale must be.
;  pixwidth: The width of the image in PIXELS. A one or two element
;            vector. If pixwidth is not specified, both width and
;            pixscale must be.
;  pixscale: The size of an individual pixel in DEGREES. A one or two
;            element vector. If not specified, both width and pixwidth
;            must be. 
;  coordsys: An optional string indicating which coordinate system to
;            use. Options are FK5 and GAL. Default is FK5  
;  type: The data type to use for the image. Default is 'FLOAT'.
;  extra: Any other keywords will be added to the header with the
;         appropriate names.
;
; OUTPUT:
;  A structure with two fields. result.map is the blank array, and
;  result.head is the header
;
; MODIFICATION HISTORY:
;  March 2010: Written by Chris Beaumont
;  December 2010: Fixed a big when checking type of coordsys
;-
function map_init, center = center, $
              width = width, $
              pixwidth = pixwidth, $
              pixscale = pixscale, $
              coordsys = coordsys, $
              type = type, $
              _extra = extra

  compile_opt idl2
  on_error, 2

  ncen = n_elements(center)
  npw = n_elements(pixwidth)
  nwd = n_elements(width)
  nsc = n_elements(pixscale)

  if ncen eq 0 && npw eq 0 && nwd eq 0 && nsc eq 0 then begin
     print, 'calling sequence'
     print, 'result = map_init([center = center, width = width'
     print, '                  pixwidth = pixwidth, pixscale = pixscale'
     print, '                  coordsys = coordsys, type = type, _extra])'
     return, !values.f_nan
  endif

  if ncen ne 2 then $
     message, 'Center keyword must contain 2 values'
  crval = center

  if npw ne 0 && min(pixwidth) le 0 then $
     message, 'Image size (pixels) must be positive'
  if nwd ne 0 && min(width) le 0 then $
     message, 'Image size (degrees) must be positive'

  if (nsc ne 0) + (nwd ne 0) + (npw ne 0) eq 3 then $
     message, /continue, 'Warning: size of image is over-specified'

  if (ncen > npw > nwd > nsc) gt 2 then $
     message, 'Input keywords must be 2 elements or less'

  ;- compute naxis (image size in pixels)
  if npw eq 2 then naxis = pixwidth
  if npw eq 1 then naxis = [pixwidth, pixwidth]
  if npw eq 0 then begin
     if nwd eq 0 || nsc eq 0 then $
        message, 'Cannot determine image size in pixels. '+$
                 'Provide either pixwidth or pixscale and width keywords'
     naxis = floor(width / abs(pixscale))
     if n_elements(naxis) eq 1 then naxis = replicate(naxis, 2)
  endif
  
  if min(naxis) le 1 then $
     message, 'Requested map is <1 pixel wide'

  ;- compute cdelt (pixel scale in degrees)
  if nsc eq 2 then cdelt = pixscale
  if nsc eq 1 then cdelt = [-pixscale, pixscale]
  if nsc eq 0 then begin
     if nwd eq 0 then $
        message, 'Cannot determine pixel scale. '+$
                 'Provide either pixsccale or width and pixwidth keywords'
     cdelt = 1.0 * width / naxis
  endif
  if cdelt[0] gt 0 then cdelt[0] *= -1

  ;- plus 1 is needed for 1-based fits convention
  crpix = (naxis+1) / 2.

  ;- define coordinate system keywords
  if n_elements(coordsys) eq 0 || strupcase(coordsys) eq 'FK5' then begin
     ctype1='RA---TAN'
     ctype2='DEC--TAN'
  endif else if strupcase(coordsys) eq 'GAL' then begin
     ctype1='GLON-TAN'
     ctype2='GLAT-TAN'
  endif else message, 'Input coordsys must by FK5 or GAL

  
  if n_elements(type) eq 0 then type='FLOAT'
  case strupcase(type) of 
     'FLOAT': result = fltarr(naxis[0], naxis[1])
     'DOUBLE': result = dblarr(naxis[0], naxis[1])
     'INT': result = intarr(naxis[0], naxis[1])
     'LONG': result = lonarr(naxis[0], naxis[1])
     'UINT': result = uintarr(naxis[0], naxis[1])
     'ULONG': result = ulonarr(naxis[0], naxis[1])
     'ULONG64': result = ulon64arr(naxis[0], naxis[1])
     'LONG64': result = ulon64arr(naxis[0], naxis[1])
     'BYTE': result = bytarr(naxis[0], naxis[1])
     else: message, 'Input TYPE value not recognized'
  endcase
  
  mkhdr, head, result

  sxaddpar, head, 'CRVAL1', crval[0]
  sxaddpar, head, 'CRVAL2', crval[1]
  sxaddpar, head, 'CRPIX1', crpix[0]
  sxaddpar, head, 'CRPIX2', crpix[1]
  sxaddpar, head, 'CDELT1', cdelt[0]
  sxaddpar, head, 'CDELT2', cdelt[1]
  sxaddpar, head, 'CTYPE1', ctype1
  sxaddpar, head, 'CTYPE2', ctype2

  ;- add extra header parameters, if provided
  if keyword_set(extra) then begin
     names = tag_names(extra)
     for i = 0, n_elements(names) - 1, 1 do begin
        sxaddpar, head, names[i], extra.(i)
     endfor
  endif

  result = {map: result, head: head}
  return, result
end
