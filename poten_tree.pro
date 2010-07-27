;+
; PURPOSE:
;  This function computes the potential energy of a mass
;  distribution. It uses a divide and conquer algorithm based on the
;  Barnes-Hut algorithm, and scales as N(log(N)). The poten_slow
;  program is more accurate, but scales as N^2. Generally, this
;  procedure will calculate energies accurate to 1%
;
; INPUTS:
;  pos: A [3, n] array of 3D particle locations
;  mass: A n element vector of masses
;
; KEYWORD PARAMETERS:
;  theta: A precision pramater which controls the algorithm. Higher
;  values translate to faster run time and larger errors. A value of 1
;  is recommended, and usually achieves 1% accuracy. A value of 1.5
;  achieves 1% accuracy for >100 evenly distributed particles. Default
;  is 1
;
; OUTPUTS:
;  The potential energy of the system. It is assumed that G=1, so that
;  PE = sum_i (sum j > i (m_i * m_j / r_ij) )
;
; MODIFICATION HISTORY:
;  July 2010: Written by Chris Beaumont.
;-
function poten_tree, pos, mass, theta = theta
  compile_opt idl2

  ;- check inputs
  if n_params() ne 2 then begin
     print, 'calling sequence:'
     print, ' result = poten_tree(pos, mass, [theta = theta])'
     return, !values.f_nan
  endif

  npt = n_elements(mass)
  if size(pos, /n_dim) ne 2 then $
     message, 'pos must be a 2D array'
  if n_elements(pos[*,0]) ne 3 then $
     message, 'positions must be 3 Dimensional. Pos must be [3, m]'


  ;- construct the tree
  lo = min(pos, dim=2)
  hi = max(pos, dim=2)
  bounds = transpose([ [lo], [hi] ])
  sz = size(bounds)
  assert, sz[1] eq 2 && sz[2] eq 3

  tree = obj_new('otree', 0, 0, bounds)
  result = 0.
  neval = 0.
  for i = 0L, npt - 1 do begin
     result += tree->calcPotential(pos[*,i], mass[i], neval = n, theta = theta)
     tree->insert, pos[*, i], mass[i]
     neval+=n
  endfor

  ;- sanity check-- tree looks ok?
  ;assert, tree->enforceAssertions(/verbose)

  ;- this line gauges how well we're doing
  ;print, neval, npt^2, npt * alog(npt)

  obj_destroy, tree
  return, result

end

pro test
  nstep = 5
  max = 20

  nelem = fltarr(nstep)
  time1 = nelem
  time2 = nelem
  time3 = nelem
  pot1 = nelem
  pot2 = nelem

  size = floor(arrgen(2., max, nstep = nstep))
  for i = 0, nstep - 1, 1 do begin
     mass = fltarr(size[i], size[i], size[i]) + 1
     indices, mass, x, y, z
     mass2 = reform(mass, size[i]^3)
     x = reform(x, size[i]^3, /over)
     y = reform(y, size[i]^3, /over)
     z = reform(z, size[i]^3, /over)

     pos = transpose([[x],[y],[z]])

     t0 = systime(/seconds)
     pot1[i] = poten_tree(pos, mass2, theta = 1.)
     time1[i] = systime(/seconds) - t0

     t0 = systime(/seconds)
     pot2[i] = poten_slow(pos, mass2)
     time2[i] = systime(/seconds) - t0

     t0 = systime(/seconds)
     junk = poten(pos, mass2)
     time3[i] = systime(/seconds) - t0

     nelem[i] = n_elements(mass)
  endfor

  print, pot1, pot2
;  return
  fit1 = poly_fit(nelem, time1, 2, yfit = yfit)
  fit1 = mean(time1 / (nelem * alog(nelem)))
  fit2 = poly_fit(nelem, time2, 2, yfit = yfit)
  nums = arrgen(10, 1d7, nstep = 50, /log)
  est1 = fit1 * nums * alog(nums)
  est2 = fit2[0] + fit2[1] * nums + fit2[2] * nums^2
  plot, nelem, time1, /xlog, /ylog, psym = 4, xra = [10, 1d7], yra = minmax([est1]), $
        charsize = 2
  oplot, nelem, time2, color = fsc_color('red'), psym = 4
  oplot, nums, est1
  oplot, nums, est2, color = fsc_color('red')
  oplot, nelem, time3, psym = 4, color = fsc_color('blue')
end


;- a quick sanity check on the 2x2x2 cube case
pro test2
  mass = fltarr(2,2,2)+1
  indices, mass, x, y, z
  mass = reform(mass, 8)
  x = reform(x, 8)
  y = reform(y, 8)
  z = reform(z, 8)
  p = transpose([[x],[y],[z]])
  result = 0.
  for i = 0, 7, 1 do begin
     for j = 0, 7, 1 do begin
        if i eq j then continue
        result += 1 / sqrt(total((p[*, i] - p[*, j])^2, 1))
     endfor
  endfor
  result /= 2.
  print, result
end

pro test3
  ;- speedtests
  sz = [16LL, 32, 64, 100]
  speed = sz * 0
  for i = 0, 3, 1 do begin
     image = bytarr(sz[i], sz[i], sz[i])
     num = sz[i]^3
     indices, image, x, y, z
     x = reform(x, num) & y = reform(y, num) & z = reform(z, num)
     pos = transpose([[x],[y],[z]])
     mass = replicate(1, n_elements(image))
     t = systime(/seconds)
     e = poten_tree(pos, mass, theta = 1)
     speed[i] = systime(/seconds) - t
  endfor
  plot, sz^3, speed, thick = 3, charsize = 2, psym = -4
  a = linfit(sz^3, speed)
  stop
end
