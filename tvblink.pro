;+
; NAME:
; TVBLINK
; 
; PURPOSE:
;  Blink between several images, without having to manually set up the
;  plotting windows.
;
; Category:
;  Display utilities
;
; PROCEDURE:
;  This is a wrapper routine for the BLINK procedure in the IDL
;  Astronomy User's library. TVBLINK goes through the work of
;  creating, displaying, and destroying the image windows which are
;  blinked between.
;
; CALLING SEQUENCE:
;  TVBLINK, image1, image2, [image3, ... image5, 
;           delay = delay, /scale]
;
; INPUTS:
;  image1: 2D image array to flip through
;  image2: 2D image array to flip through
;  image3: 2D image array to flip through
;  image4: 2D image array to flip through
;  image5: 2D image array to flip through
; 
; KEYWORD PARAMETERS:
;   DELAY: The time delay, in seconds, between blinking
;   SCALE: If set, the input images are auto-stretched
;
; SIDE EFFECTS:
;  Temporary plotting windows are created, and images are blinked on
;  the screen.
;
; PROCEDURES CALLED:
;  BLINK (IDL Astro Library)
;  TVIMAGE (David Fanning's Coyote Library)
;
; MODIFICATION HISTORY:
;  Written by: Chris Beaumont, December 2008
;-
PRO tvblink, image1, image2, image3, image4, image5, SCALE=scale, DELAY=delay

compile_opt idl2
on_error, 2

;-save initial device information
win0 = !d.window
!p.multi = 0

;- check for inputs
nimage = n_params()

if nimage lt 2 || nimage gt 5 then begin
    print, 'TVBLINK calling sequence: '
    print, 'TVBLINK, image1, image2, [image3 ... image5,'
    print, '         /SCALE, delay = delay'
    return
endif

;- array to hold which windows we create
winID = intarr(nimage)

;- get image dimensions, create a new window, and plot
for i = 0, nimage-1, 1 do begin
    iPlusStr = strtrim(i+1, 2)
    tmp = execute('sz = size(image'+ iPlusStr +')')

    if sz[0] ne 2 then message, 'Input images must be 2D arrays'
    window, /free, xsize=sz[1], ysize=sz[2], /pixmap, retain = 2
    winID[i] = !d.window

    tmp = execute('tvimage, image' + iPlusStr + ', scale=keyword_set(scale)')
endfor

if keyword_set(delay) then blink, winID, delay $
  else blink, winID

;-erase windows, restore original window
for i = 0, n_elements(winID)-1, 1 do wdelete, winID[i]
if (win0 ne -1) then wset, win0

end
