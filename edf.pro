;+
; PURPOSE:
;  This procedure calculates the empirical distribution function (edf)
;  of a set of data, evaluated at each input data point. The edf is
;  the empirical equivalent of the cumulative distribution function.
;
; CATEGORY:
;  Statistics
;
; CALLING SEQUENCE:
;  edf, data, x, y, [/plot, _extra = extra]
;
; INPUTS:
;  data: A vector of data values
;
; KEYWORD PARAMETERS:
;  PLOT: Set to plot the results. 
;  _extra: Any extra kewyord passed to edf is forwarded to PLOT
;
; OPTIONAL OUTPUTS:
;  x: On output, the variable holds the values of data, in numerical order
;  y: On output, the variable holds a vector. y[i] = Fraction(data < x)
;
; MODIFICATION HISTORY:
;  June 2009: Written by Chris Beaumont
;-
pro edf, data, x, y, plot = plot, _extra = extra
compile_opt idl2
on_error, 2

;- check inputs
if n_params() lt 1 then begin
   print, 'edf calling sequence'
   print, 'edf, data, x, y, [/plot, plot_keywords]'
   return
endif

if n_elements(data) le 1 then $
   message, 'input data must have at least 2 elements'

fin = where(finite(data),sz)
if sz lt 5 then $
   message, 'input data must have at least 5 finite elements'
fin = data[fin]
s = sort(fin)
x = fin[s]
y = (findgen(sz)) / sz

if keyword_set(plot) then begin
   plot, x, y, xtit = 'X', ytit = 'EDF(x)', psym = 10, _extra = extra
endif

return

end
