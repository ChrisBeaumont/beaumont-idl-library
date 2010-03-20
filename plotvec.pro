;+
; PURPOSE:
;  This procedure creates plots of vector fields. It is a wrapper
;  procedure for PLOT and OPLOT, and accepts all of their keywords
;
; CATEGORY:
;  display utilities
;
; CALLING SEQUENCE:
;  plotvec, x,y , xvec, yvec, [linecolor = linecolor, _extra = extra,/over]
;
; INPUTS:
;  x: The x coordinate of each vector tail
;  y: The y coordinate of each vector tail
;  xvec: The x component of the vector length
;  yvec: The y component of the vector length
;
; KEYWORD PARAMETERS:
;  linecolor: The what color to paint the vectors. Used analagously to
;             the COLOR keyword in plot.
;             In addition, any extra kewyords are passed to plot
;  OVER:         overplot on a pre-existing graphics window
;  veclinestyle: The LINESTYLE keyword value to use when drawing the
;                vector lines.
;  linethick:    The line thickness to use when drawing the vectors.
;  _extra:       Extra plot keywords to pass through to PLOT.
;
; MODIFICATION HISTORY:
;  April 2009: Written by Chris Beaumont
;-
pro plotvec, x, y, xvec, yvec, linecolor = linecolor, $
             over = over, _extra = extra, veclinestyle = veclinestyle, linethick = linethick
compile_opt idl2
on_error, 2

if n_params() ne 4 then begin
   print, 'plotvec calling sequence:'
   print, ' plotvec, x, y, xvec, yvec, [linecolor = linecolor, _extra = extra]'
   return
endif

nx = n_elements(x)
if n_elements(y) ne nx || $
   n_elements(xvec) ne nx || $
   n_elements(yvec) ne nx then $
      message, 'x, y, xvec, and vec must contain the same number of elements'

if keyword_set(over) then $
   oplot, x, y, _extra = extra, psym = 3 $
else $
   plot, x, y, _extra = extra, psym = 3

for i = 0, nx-1, 1 do begin
   oplot, x[i] + [0, xvec[i]], y[i] + [0, yvec[i]], color = linecolor, $
          psym = 0, thick = linethick, linestyle = veclinestyle
endfor

end
