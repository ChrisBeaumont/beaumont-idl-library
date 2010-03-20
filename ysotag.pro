;+
; NAME:
;  YSOTAG
;
; PURPOSE:
;   This function tags sources with IR excess based on their colors
;   in Spitzer IRAC bands 1-4. The cut used comes from Megeath et al
;   2004, ApJS 154:367. Sources are tagged as source 0/I, II, or
;   III/main sequence  
; 
; CATEGORY:
;  catalog processing
;
; CALLING SEQUENCE:
;  result=YSOTAG(i1,i2,i3,i4,[/VERBOSE])
;
; INPUTS:
;  i1: IRAC band 1 magnitude(s)
;  i2: IRAC band 2 magnitude(s)
;  i3: IRAC band 3 magnitude(s)
;  i4: IRAC band 4 magnitude(s)
;
; KEYWORD PARAMETERS:
;  VERBOSE: if set, then a summary of the sources is printed to the screen.
;
; OUTPUTS:
;  A vector of bytes the same length as i1-i4. The value at slot i is
;  1, 2, or 3 depending on whether the source at slot i has colors
;  like a class I, II or III/Main Sequence star. Only  sources marked
;  as 1 or 2 should be considered as candidate excess stars.
;
; MODIFICATION HISTORY:
;  Written by: Chris Beaumont, June 2008.
;  November 2008: Changed name from SOURCETAG to YSOTAG
;-  
FUNCTION  ysotag, i1, i2, i3, i4, verbose = verbose

compile_opt idl2
on_error,2

;- check inputs
if n_params() ne 4 then message,'Calling Sequence: tag=ysotag(i1,i2,i3,i4)'

nelem=n_elements(i1)
if (n_elements(i2) ne nelem) || (n_elements(i3) ne nelem) || (n_elements(i4) ne nelem) then $
  message, 'Error -- input irac vectors must be the same length.'


;- Class I tag
c1= ((i1-i2) ge .8) or  ((i3-i4) ge 1.1)

;- Class II tag
c2= ((i1-i2) gt 0) and  ((i1-i2) lt .8) and $
  ((i3-i4) gt .4) and  ((i3-i4) lt 1.1)

;- Create output vector
result = bytarr(nelem) + 3B
hit1 = where(c1, ct1)
hit2 = where(c2, ct2)
if ct1 ne 0 then result[hit1] = 1B
if ct2 ne 0 then result[hit2] = 2B

;- Print results
if keyword_set(verbose) then begin
    print, 'Number of Class 0/1 Sources:    '+strtrim(string(ct1),2)
    print, 'Number of Class 2 Sources:      '+strtrim(string(ct2),2)
    print, 'Number of Class III/MS Sources: '+strtrim(string(nelem-ct1-ct2),2)
endif

return,result

end
