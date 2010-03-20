;+
; NAME:
;  SKY2PIX
; 
; PURPOSE:
;  This function translates between 'real' coordinates (eg, ra dec) to
;  pixel coordinates in a fits file. Pixel co-ordinates are given as
;  zero-indexed values (IDL convention) as opposed to 1-indexed (FITS
;  convention). This was designed as a wrapper to / substute for
;  commands like ad2xy, which demand that the input data be 2D. This
;  program processes data cubes as well. 
;
; CALLING SEQUENCE:
;  result=sky2pix(header,coords,/backwards)
;
; INPUTS:
;  header: A string array containing a fits header (eg, from mrdfits)
;  coords: An n by m array containing the n-dimensional coordinates of
;          m datapoints. These are 'real sky' coordinates, unless the
;          backwards keyword is set. 
;
; KEYWORD PARAMETERS:
;  backwards: coords are given in pixel coordinates, and the output is
;             given in sky coordinates
;
; OUTPUTS:
;  result: An array contining the pixel coordinates of the real
;          coordinates given by coords. This array is the same size as
;          coords. result [i,j] contains the coordinate component of the jth
;          data point along the ith pixel dimension.
;
; EXAMPLES:
;   find the pixel location of (ra,dec,vel)=(5,6,7) in header h
;   IDL> result=sky2pix(h,[5,6,7])
;   
;   find the pixel location of (ra,dec)=(5,6) in a data cube with
;   header h
;   IDL> result=sky2pix(h,[5,6])
;
; PROCEDURE:
;  This program is a wrapper to the ad2xy, xy2ad, etc programs. Unlike
;  those procedures, however, sky2pix can handle simple 3D cubes. For
;  this to work, however, the cubes must be "aligned" along the third
;  dimension. That is, the third "sky" axis (e.g. velocity) must
;  correspond directly to the third "data" axis. This is equivalent
;  to saying that, if the header uses the CDX_Y keywords, then CD3_X
;  and CDX_3 must be zero, except for CD3_3. Note that this is almost
;  always the case for normal data cubes.
;
; MODIFICATION HISTORY:
;  Written by: Chris Beaumont. July 27 2008
;  December 17 2008: Cosmetic changes. cnb.
;  November 21 2009: Complete re-write by cnb. Uses the extast3
;                    program, and more directly relies on ad2xy to
;                    handle complicated wcs stuff.
;-
function sky2pix, header,coords, backwards=backwards

  compile_opt idl2
 
  ;-check number of inputs
  if n_params() ne 2 then begin
     print,'Calling Sequence: result=sky2pix(header,coords)'
     print,'header:           Fits header'
     print,'coords:           n by m array of m n-dimensional coordinates'
     return, !values.f_nan
  endif
  
  ;-verify that the inputs have the correct size
  sz=size(coords)
  arr_dim=sz[0]

  ;- handle the special case of a single point requested
  if (arr_dim eq 1) then begin
     int_coords = double(reform(coords, n_elements(coords), 1))
     sz = size(int_coords)
  endif else if arr_dim eq 2 then begin
     ;-prevent us from overwriting coordinates
     int_coords = double(coords)
  endif else $
     message, 'Must supply a 2D array (m columns by n rows.'+$
              'N data points in m dimensions)'
  
  ndim = sz[1]
  npts = sz[2]
  
  ;- do easy case first: 2D is just wrappers to adxy or xyad
  if ndim eq 2 then begin
     c1 = reform(int_coords[0,*])
     c2 = reform(int_coords[1,*])
     if keyword_set(backwards) then begin
        xyad, header, c1, c2, a, d
        result = transpose([[a],[d]])
     endif else begin
        adxy, header, c1, c2, x, y
        result = transpose([[x],[y]])
     endelse

  ;- harder case of a cube
  endif else if ndim eq 3 then begin
     c1 = reform(int_coords[0,*])
     c2 = reform(int_coords[1,*])
     c3 = reform(int_coords[2,*])
     
     ;-extract astrometry
     extast3, header, ast

     if keyword_set(backwards) then begin
        xy2ad, c1, c2, *ast.extast, a, d
        vel = (c3 + 1 - ast.crpix3) * ast.cdelt3 + ast.crval3
        result = transpose([[a],[d],[vel]])
     endif else begin
        ad2xy, c1, c2, *ast.extast, x, y
        z = (c3 - ast.crval3) / ast.cdelt3 + ast.crpix3 - 1
        result = transpose([[x],[y],[z]])
     endelse
     extast3, header, ast, /delete

  endif else message, 'sky2pix can only handle 2- and 3- dimensional arrays'
  return, result
 
end
