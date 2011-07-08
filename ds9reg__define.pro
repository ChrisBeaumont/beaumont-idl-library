;+
; PURPOSE:
;  Defines a simple IDL structure to represent a DS9-style region
;  description. Used in conjunction with write_ds9reg
;-
pro ds9reg__define
  struct = {ds9reg, $
            shape:'', $         ;- shape of the region
            x:0., $             ;- center x position 
            y:0., $             ;- center y position
            radius:0., $        ;- radius (if circle). Assumed to be arcsec
            angle:0., $         ;- angle, if relevant. Degrees.
            text:'', $          ;- text label
            color:'', $         ;- region color
            width:0., $         ;- width (if relevant)
            height:0., $        ;- height (if relevant)
            font:'', $          ;- font for label
            select:1B, $        ;- is selected?
            fixed:0B, $         ;- is fixed?
            edit:1B, $          ;- is editable?
            move:1B, $          ;- is moveable?
            rotate:0B, $        ;- can be rotated? 
            delete:1B}          ;- can be deleted?
end
