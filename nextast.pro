;+
; NAME:
;  NEXTAST
;
; PURPOSE:
;  This function returns a structure of astrometry parameters parsed
;  from a FITS header. It is meant to mimic the IDL Astronomy User's
;  Library routine 'EXTAST', but is generalized to handing
;  N-dimensional data files (e.g. spectral data cubes). Note, however,
;  that EXTAST handles a wider variety of astrometry parameters and,
;  I'm sure, is much more robust for 2D FITS files
;
; CATEGORY:
;  coordinate systems
;
; CALLING SEQUENCE:
;  result=NEXTAST(header)
;
; INPUTS:
;  header: A string array containing a fits header (ie, the output
;  from mrdfits)
;
; OUTPUTS:
;  An anonymous structure with the following tags:
;     .NAXIS- the number of axes in the file
;     .SZ - An NAXIS element array containing NAXIS1...NAXIS[N]
;     .CD - An NAXIS x NAXIS array holding the CD/cdelt keywords.
;           CD[i,j]=CD[I]_J. If the header uses CDELT keywords
;           instead, they are converted to matrix form (CDELT1 =
;           CD1_1)
;     .CRPIX - An NAXIS element array containing the CRPIX keywords
;     .CRVAL - An NAXIS element array containing the CRVAl keywords
;     .CTYPE - An NAXIS element array containing the CTYPE keywords
;
; RESTRICTIONS:
;  Error checking is limited. This was developed and tested for fits
;  files using cdelt or cdi_j keywords, with no distortion or other
;  fancy features. 
; 
; MODIFICATION HISTORY:
;  Written by: Chris Beaumont, July 27, 2008
;-
FUNCTION nextast, header

compile_opt idl2
on_error, 2

;-check inputs
if n_params() eq 0 then begin
    print,'NEXTAST Calling Sequence: result=NEXTAST(head)'
    print,'head: A FITS header string array'
    return,0
endif

;- find number of axes
naxis=sxpar(header,'NAXIS',count=ct)
if ct eq 0 then message,'NAXIS keyword not present in header'

;-initialize structure variables
sz=lonarr(naxis)
cd=dblarr(naxis,naxis)
crpix=dblarr(naxis)
crval=dblarr(naxis)
ctype=strarr(naxis)

;-fill in sz, crpix, crval, ctype
for i=0, naxis-1, 1 do begin
    n=strtrim(string(i+1),2)
    sz[i]=sxpar(header, 'NAXIS'+n, count=ct)
    if ct eq 0 then $
      message,'NAXIS'+n+' keyword not present in header'
    crval[i]=sxpar(header,'CRVAL'+n,count=ct)
    if ct eq 0 then $
      message, 'CRVAL'+n+' keyword not present in header'
    crpix[i]=sxpar(header,'CRPIX'+n, count=ct)
    if ct eq 0 then $
      message, 'CRPIX'+n+' keyword not present in header'
    ctyp=sxpar(header,'CTYPE'+n,count=ct)
    if ct ne 0 then ctype[i]=ctyp
endfor

;- cdelt or cd keywords
cd[0,0]=sxpar(header,'cdelt1',count=ct)

if ct eq 0 then begin

    ;-CDELT keywords not present. Check for CD
    for i = 0, naxis-1, 1 do begin
        for j = 0, naxis-1, 1 do begin
            n = strtrim(string(i+1), 2)
            m = strtrim(string(j+1), 2)
            cd[i,j] = sxpar(header,'CD' + n + '_' + m, count = ct)
            ;-usually, i=j elements should be defined. Warn if not
            if (i eq j) && (ct eq 0) then $
              message,'Warning: CD'+n+'_'+m+' not defined',/continue
        endfor
    endfor 

endif else begin

    ;-CDELT present. Convert to CD format
    for i = 1,naxis-1, 1 do begin
        cd[i,i] = sxpar( header, 'cdelt' + strtrim(string(i+1), 2), count = ct)
        if ct eq 0 then message,'CDELT/CD Keywords Absent'
    endfor

    ;-CROTA is not implemented. issue a warning if present
    count = 0                
    for i = 1, naxis, 1 do begin
       junk = sxpar(header, 'crota' + strtrim(string(i), 2), count = temp)
       count+= temp
    endfor
    if count ne 0 then $
       message, 'CROTA keywords not supported. Astrometry is meaningless!', /continue
endelse

result={naxis:naxis,sz:sz,cd:cd,crpix:crpix,crval:crval,ctype:ctype}

return,result
end
