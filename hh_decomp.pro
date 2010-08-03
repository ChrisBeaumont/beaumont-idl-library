;XXXX not right yet-- see first test

;+
; PURPOSE:
;  This procedure computes the Helmholz decomposition of a 3D vector
;  field into its divergence and curl free components.
;
; INPUTS:
;  x: The x component of the vector. A 3D array
;  y: The y component of the vector. A 3D array
;  z: The z component of the vector. A 3D array
;
; OUTPUTS:
;   dx: The x component of the divergence-y (curl-free) term
;   dy: The y component of the divergence-y (curl-free) term
;   dz: The z component of the divergence-y (curl-free) term
;   cx: The x component of the curly (divergence-free) term
;   cy: The y component of the curly (divergence-free) term
;   cz: The z component of the curly (divergence-free) term
;
;
; PROCEDURE:
;  The input vector is projected into fourier space, and decomposed
;  into modes parallel and perpendicular to k. These are then
;  projected back into the original space, and are equal to the curl-
;  and divergence-free vectors, respectively. 
;
; MODIFICATION HISTORY:
;  2010-07-29: Created by Chris Beaumont
;-
pro hh_decomp, x, y, z, $
               dx, dy, dz, $
               cx, cy, cz

  fx = fft(x,/inverse)
  fy = fft(y,/inverse)
  fz = fft(z,/inverse)
  
  sz = size(x)
  fft_kind, fx, kx, ky, kz
  k = sqrt(kx^2 + ky^2 + kz^2)
  eps = 1e-12
  k += eps

  ;- take dot product of f and k-hat
  dot = (fx * kx + fy * ky + fz * kz) / k
  
  dx = kx/k * dot & dy = ky/k * dot & dz = kz/k * dot
  cx = fx - dx & cy = fy - dy & cz = fz - dz

  ;- sanity check: cx dot k = 0
  ;dot2 = cx * kx + cy * ky + cz * kz
  ;assert, max(dot2) lt 1e-4

  ;cx = real_part(fft(cx,/inverse))
  ;cy = real_part(fft(cy,/inverse))
  ;cz = real_part(fft(cz,/inverse))

  ;dx = real_part(fft(dx,/inverse))
  ;dy = real_part(fft(dy,/inverse))
  ;dz = real_part(fft(dz,/inverse))


  cx = real_part(fft(cx))
  cy = real_part(fft(cy))
  cz = real_part(fft(cz))

  dx = real_part(fft(dx))
  dy = real_part(fft(dy))
  dz = real_part(fft(dz))

end

pro test
  im = fltarr(64, 64, 64)
  indices, im, x, y, z 
  x -= 11.5 & y -= 31.5 & z -= 31.5

  ;- case 1. Velcoity field is curl free
  hh_decomp, x, y, z, $
             dx, dy, dz, $
             cx, cy, cz

  ;- d should equal f(x,y,z). c should equal zero
  plot, x, dx, psym = 3
  oplot, x, cx, psym = 4
  oplot, y, dy, psym = 3, color = fsc_color('red')
  oplot, y, cy, psym = 4, color = fsc_color('red')
  oplot, z, dz, psym = 3, color = fsc_color('blue')
  oplot, z, cz, psym = 4, color = fsc_color('blue')
  stop

  ;- case 2: div free field
  x2 = y & y2 = x & z2 = 3*x + y
  hh_decomp, x2, y2, z2, $
             dx, dy, dz, $
             cx, cy, cz
  ;- c should equal f(x,y,z). d should be zero
  plot, x2, dx, psym = 3, xra = minmax(z2), yra = minmax(z2)
  oplot, x2, cx, psym = 4
  oplot, y2, dy, psym = 3, color = fsc_color('red')
  oplot, y2, cy, psym = 4, color = fsc_color('red')
  oplot, z2, dz, psym = 3, color = fsc_color('blue')
  oplot, z2, cz, psym = 4, color = fsc_color('blue')
  stop

  ;- case 3: case 1 + case2. Should recover individual terms
  x3 = x + x2 & y3 = y + y2 & z3 = z + z2
  hh_decomp, x3, y3, z3, $
             dx, dy, dz, $
             cx, cy, cz
  ;- c should equal x2. d should equal x
  plot, x, dx, psym = 3, xra = minmax(z3), yra = minmax(z3)
  oplot, x2, cx, psym = 4
  oplot, y, dy, psym = 3, color = fsc_color('red')
  oplot, y2, cy, psym = 4, color = fsc_color('red')
  oplot, z, dz, psym = 3, color = fsc_color('blue')
  oplot, z2, cz, psym = 4, color = fsc_color('blue')
  
end
