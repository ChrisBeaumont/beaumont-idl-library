;+
; PURPOSE:
;  This function returns the values of many different constants used
;  in astrophysics.
;
; CATEGORY:
;  Astrophysics
;
; CALLING SEQUENCE:
;   result = APCON(value, [/list, /cgs, /mks])
;
; INPUTS:
;  value: A string naming the desired constant. For a list of
;  supported constants, use print, APCON(/list)
;
; KEYWORD PARAMETERS:
;  LIST: If set, return a list of the possible constants
;
;  CGS: If set, return values in cgs units (happens by default)
;
;  MKS: If set, return values in mks units.
;  
; OUTPUTS:
;  The value of the input constant
; 
; EXAMPLES:
;   IDL> print, apcon('G', /MKS)
;      6.6730000e-11
;   IDL> print, apcon('G')
;      6.6730000e-08
;
; TODO:
;  Masses, radii of solar system bodies
;
; MODIFICATION HISTORY:
;  May 2009: Written by Chris Beaumont
;-    
function apcon, value, list = list, cgs = cgs, mks = mks
compile_opt idl2
on_error, 2

;- check inputs
if n_params() ne 1 && ~keyword_set(list) then begin
   print, 'APCON calling sequence:'
   print, ' result = APCON(value, [/list, /mks, /cgs])'
   return, !values.f_nan
endif

if keyword_set(cgs) and keyword_set(mks) then $
   message, 'Cannot set both CGS and MKS keywords'

mks = keyword_set(mks)

;- list of possible values. KEEP THIS UPDATED!!!
values = ['G', 'Speed of Light', 'c', $
          'boltzmann constant', 'k', 'kb', $
          'sigma', 'parsec', 'pc', 'au', 'm_sun', $
          'm_solar', 'solar_mass', 'mass_sun', $
          'r_sun', 'r_solar', 'solar_radius', 'radius_sun', $
          'l_sun', 'l_solar', 'solar_luminosity', 'luminosity_sun', $
          'mass_earth', 'earth_mass', 'm_earth', $
          'radius_earth', 'earth_radius', 'r_earth', $
         'Jansky', 'jy', 'h', 'hbar', 'planck', 'm_proton', 'mass_proton', $
         'proton_mass']

;- sort the list. add whitespace for printing
alphabetical = sort(strlowcase(values))
values = values[alphabetical]
nval = n_elements(values)
val_newline = strarr(2, nval)
val_newline[0, *] = values
val_newline[1, *] = string(10B)
values = reform(val_newline, nval * 2)

if keyword_set(list) then return, values

value = strlowcase(value)
switch value of

   'g' : return, mks ? 6.673d-11 : 6.673d-8 

   'speed of light':
   'c' : return, mks ? 299792458D : 299792458D * 1d2 

   'kb':
   'boltzmann constant':
   'k' : return, mks ? 1.3806503d-23 : 1.3806503d-16

   'sigma': return, mks ? 5.6704d-8 : 5.6704d-5

   'au' : return, mks ? 149598000d3 : 149598000d5

   'parsec':
   'pc' : return, mks ? 3.08568025d16 : 3.08568025d18

   'm_sun':
   'm_solar':
   'solar_mass':
   'mass_sun' : return, mks ? 1.98892d30 :  1.98892d33

   'r_sun':
   'r_solar':
   'solar_radius':
   'radius_sun': return, mks ? 6.955d8 : 6.955d10 

   'l_sun':
   'l_solar':
   'solar_luminosity':
   'luminosity_sun': return, mks ? 3.839d26 : 3.839d33

   'mass_earth':
   'earth_mass':
   'm_earth': return, mks ? 5.9742d24 : 5.9742d27

   'radius_earth':
   'earth_radius':
   'r_earth': return, mks ? 6378.1d3 : 6378.1d5

   'jy':
   'jansky': return, mks ? 1d-26 : 1d-23

   'planck':
   'h': return, mks ? 6.626068d-34 : 6.626068d-27
   
   'hbar': return, mks ? 1.05457148d-34 : 1.05457148d-27

   'm_proton':
   'mass_proton':
   'proton_mass': return, mks ? 1.6726d-27 : 1.6726d-24

;- remember to add to the list array when you add more
   else: message, 'constant not recognized: '+value

endswitch


end
   
