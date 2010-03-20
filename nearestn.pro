;+
; PURPOSE:
;  nearestN takes takes a list of source locations and, for a list of
;  test positions, returns the index of the n'th nearest source.
;-

;+
;
; PURPOSE:
;  findNeighbors is an internal routine used by nearestN. Given an
;  (x,y) point and histogram information about a set of sample points,
;  this function returns a list of at least n objects, one of which is
;  guaranteed to be the nth nearest neighbor.
;
; CALLING SEQUENCE:
;  result = nearestn_findNeighbors(x, y, n, h, ri, nx, ny)
; 
; INPUTS:
;  x: The x data point to find neighbors for
;  y: The y data point to find neighbors for
;  n: Ensure that the n'th nearest neighbor is included in the output
;  h: The histgogram created in nearestN
; ri: The reverse indices variable from the histogram calculated in
;     nearestN
; nx: The number of bins along the x axis in the histogram described
;     by h
; ny: The number of bins along the y axis in the histogram described
;     by h
;
; OUTPUTS:
;  A list of indices referenceing objects in the histogram created in
;  nearestN. One of these objects is guaranteed to be the nth nearest
;  neighbor.
;
; PROCEDURE:
;  The function uses the reverse_indices variable from histogram to
;  effeciently index objects nearby (x,y).
;
; MODIFICATION HISTORY:
;  Written by: Chris Beaumont, October 2008
;-
function nearestn_findNeighbors, x, y, n, h, ri, nx, ny

compile_opt idl2, hidden
;on_error, 2
  
;- find how big of a histogram bin square we need to collect n points
  delt = 0
  while 1 do begin
     if total(h[(x - delt) > 0 : (x + delt) < (nx - 1) , $
                (y - delt) > 0 : (y + delt) < (ny - 1)]) $
        gt n then break else delt++
  endwhile
  
;- collect points inside this square. Add an extra border to
;- guarantee that the nth closest point is found
  xbinlo = (x - delt - 2 * 1) > 0
  xbinhi = (x + delt + 2 * 1) < (nx - 1)
  ybinlo = (y - delt - 2 * 1) > 0
  ybinhi = (y + delt + 2 * 1) < (ny -1)
  xbins = indgen(xbinhi - xbinlo + 1) + xbinlo
  ybins = indgen(ybinhi - ybinlo + 1) + ybinlo
  nxbins = n_elements(xbins)
  nybins = n_elements(ybins)
  
  bins = rebin(xbins, nxbins, nybins) + nx * rebin( 1#ybins, nxbins, nybins)
  candidates = [-1]
  for i=0, nxbins * nybins - 1, 1 do $
     if ri[bins[i]+1] eq ri[bins[i]] then continue else $
        candidates=[candidates, ri[ri[bins[i]]:ri[bins[i]+1]-1]]
  return, candidates[1: n_elements(candidates)-1]
  
end


;+
; PURPOSE:
;  This function returns the nth closest object to a given set of coordinates
;
; PROCEDURE:
;  Designed to scale roughly linearly with the catalog size, as opposed to more common
;  O(n^2) methods (e.g. computing every distance for each point and
;  sorting).
;
; CATEGORY:
;  catalog processing
;
; CALLING SEQUENCE:
;  result = nearestN(points, catalog, n, [dist = dist, /all])
;
; INPUTS:
;   points: The points to find the nth nearest neighbor for
;  catalog: A (2,m) array of m (x,y) points
;        n: Find the n'th closest neighbor. The closest point
;        corresponds to n = 0
;
; KEYWORD PARAMETERS:
;  dist: A variable to hold the distances between the points and their
;  nth nearest neighbors.
;
;  all: If set, returns all n+1 closest neightbor indices. In this
;  case, the result is a n + 1 by m array
;
;  verbose: Print extra output
;
; OUTPUTS:
;  A vector whose ith row gives the index of the nth closest
;  point to points[i]
;
; RESTRICTIONS:
;  Coordinate space is assumed to be euclidian.
;
; MODIFICATION HISTORY:
;  Written by: Chris Beaumont, September 2008
;  December 2008: Updated so that the points to find neighbors for
;  need not be the list of object coordinates itself. cnb.
;  March 2009: Added the DIST keyword. cnb.
;  March 2009: Fixed bug when requesting n = 0. Added stricter input
;  checking. cnb.
;  March 2009: Added ALL keywrod. cnb.
;  April 2009: Fixed bug in which test points outside the cloud of
;  catalog points caused a crash. cnb.
;  May 2009: Added VERBOSE keyword. cnb.
;  June 2009: Reworked VERBOSE outputs to be used with VERBIAGE
;             program. cnb. 
;  November 2009: Fixed bug that incorrectly populated the DIST array
;                 when /ALL was set
;-
function nearestn, points, catalog, n, dist = dist, all = all, verbose = verbose

compile_opt idl2
;on_error, 2

;- check inputs
if n_params() ne 3 then begin
   print, 'nearestN calling sequence: '
   print, 'result = nearestN(points, catalog, n)'
   print,'          points: (2 by p) array of (x,y) points to find neighbors for'
   print,'          catalog: (2 by q) array of reference points'
   print,'          n: Find the nth nearest neighbor'
   print,'          an array whose ith row gives the index, in catalog, of the nth closest object to points[*,i]'
   return, -1
endif

if n lt 0 then message, 'Must request a neighbor index >= 0'

if size(points, /n_dimensions) ne 2 || size(catalog, /n_dimensions) ne 2 then $
   message, 'points and catalog must be 2D arrays'

if n_elements(points[*,0]) ne 2 || n_elements(catalog[*,0]) ne 2 then $
   message, 'points and catalog must be 2-column arrays'

if n ge n_elements(catalog[0,*]) then $
   message, 'Cannot request a neighbor index n > number of points in catalog'

;- Set up parameters for a 2D histogram. We want each histogram bin to contain
;- AvgPtsPerGrid objects on average.
  nobj = n_elements(catalog[0,*])
  AvgPtsPerGrid = .20 * (n > 1)
  xhi = max([reform(catalog[0,*]),reform(points[0,*])], min=xlo,/nan)
  yhi = max([reform(catalog[1,*]),reform(points[1,*])], min=ylo,/nan)
  area = (xhi-xlo) * (yhi - ylo)
  gridsize = sqrt(area / nobj * AvgPtsPerGrid)
  
;- Calculate the 2D histogram.
  numXbins = floor((xhi - xlo) / gridsize) + 1L
  numYbins = floor((yhi - ylo) / gridsize) + 1L
  xbins = floor((catalog[0,*] - xlo) / gridsize)
  ybins = floor((catalog[1,*] - ylo) / gridsize)
  bins  = xbins + numXbins * ybins
  h = histogram(bins, reverse_indices = ri, min = 0, max= numXbins * numYbins-1,/nan)
  h = reform(h, numXbins, numYbins, /overwrite)
  
  npts = n_elements(points[0,*])

  if ~keyword_set(all) then answers = lonarr(npts) else $
     answers = lonarr(n+1, npts)


  ptxbin = floor((points[0,*] - xlo) / gridsize)
  ptybin = floor((points[1,*] - ylo) / gridsize)

  report = obj_new('looplister', npts, 10)
  
  for i = 0L, npts -1 , 1 do begin
     report->report, i, out = progress
     verbiage, progress, 3, verbose
     x = points[0,i]
     y = points[1,i]
     candidates = nearestn_findNeighbors(ptxbin[i], ptybin[i], $
                                         n, h, ri, numXbins, numYbins)
     dist = reform((x - catalog[0,candidates])^2 + (catalog[1,candidates] - y)^2)
     sorted = sort(dist)
     if ~keyword_set(all) then begin
        answers[i] = candidates[sorted[n]]
     endif else begin
        answers[*,i] = candidates[sorted[0:n]]
     endelse
  endfor
  obj_destroy, report

  if arg_present(dist) then begin
     nrow = n_elements(catalog[0,*])
     dist = dblarr(n+1, n_elements(catalog[0,*]))
     for i = 0, n, 1 do begin
        dist[i, *] = sqrt(total( (points - catalog[*, answers[i,*]]) ^2, 1))
     endfor
  endif

  return, answers
end
