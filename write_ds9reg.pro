;+
; PURPOSE:
;  This procedure writes an array of ds9region structures (defined in
;  the Beaumont IDL library) into a region file that ds9 undertands
;
; INPUTS:
;  file: The name of the file to write to.
;  regions: An array of ds9reg structures (see ds9reg__define.pro)
;  system: The coordinate system to use. Choices are: PHYSICAL, IMAGE,
;          FK4, B1950, FK5, J2000, GALACTIC, ECLIPTIC, ICRS
;
; RESTRICTIONS:
;  Currently, only circle, ellipse, and box regions are supported by
;  write_ds9reg. 
;
; MODIFICATION HISTORY:
;  2010: Written by Chris Beaumont
;-
pro write_ds9reg, file, regions, system

  if n_params() ne 3 then begin
     print, 'calling sequence'
     print, 'write_ds9reg, file, regions, system'
     return
  endif

  if size(file,/tname) ne 'STRING' then $
     message, 'File must be a valid string'

  if size(regions,/tname) ne 'STRUCT' || $
     tag_names(regions[0], /structure_name) ne 'DS9REG' then $
        message, 'regions must be a scalar or array of {ds9reg} structures'

  case strupcase(system) of
     'PHYSICAL':
     'IMAGE':
     'FK4':
     'B1950':
     'FK5':
     'J2000':
     'GALACTIC':
     'ECLIPTIC':
     'ICRS':
     else: message, 'system is not a valid coordinate system'
  endcase
  if strupcase(system) eq 'PHYSICAL' || strupcase(system) eq 'IMAGE' $
     then symbol = '' else symbol='"'

  sz = n_elements(regions)
  output = strarr(sz)
  for i = 0L, sz - 1, 1 do begin
     rec = regions[i]

     ;- shape-specific information
     case strupcase(rec.shape) of 
        'CIRCLE': $
           output[i] = string(rec.x, rec.y, rec.radius, symbol, $
                              format='("circle(", f0.5, "," , f0.5, ",", f0.5, a,")#")')
        'ELLIPSE': $ 
           output[i] = string(rec.x, rec.y, $
                              rec.width, symbol, rec.height, symbol, $
                              rec.angle, $
                              format='("ellipse(",2(f0.5,","),2(f0.5,a,","),f0.5,")#")')
        'BOX': $
           output[i] = string(rec.x, rec.y, $
                              rec.width, symbol, rec.height, symbol, $
                              rec.angle, $
                              format='("box(",2(f0.5,","),2(f0.5,a,","),f0.5,")#")')
        else: begin
           message, /con, 'shape parameter '+rec.shape+$
                    ' not supported by write_ds9reg. Aborting'
           return
        end
     endcase


     ;- add in tag information
     if strlen(rec.text) ne 0 then output[i]+=' text ={'+rec.text+'} '
     if strlen(rec.color) ne 0 then output[i]+=' color ='+rec.color+' '
;     output[i] += string(rec.select, rec.fixed, rec.edit, rec.move, $
;                         rec.rotate, rec.delete, $
;                         format='("select = ", i, " fixed = ", i,'+$
;                         '" edit = ", i, " move = ", i," rotate = ", i," delete ",i)')   
  endfor

  ;- write the file
  openw, lun, file, /get
  printf, lun, '# Region file format: DS9 version 4.0'
  printf, lun, '# Created by write_ds9reg.pro on'+systime()
  printf, lun, 'global color=green dashlist=8 3 width=1 font="helvetical 10 normal" '+$
          'select=1 highlite=1 dash=0 fixed=0'
  printf, lun, system
  for i = 0L, sz - 1, 1 do printf, lun, output[i]
  free_lun, lun

end
