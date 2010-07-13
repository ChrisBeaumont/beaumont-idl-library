;+
; PURPOSE:
;  This function computes the potential energy of a mass
;  distribution. It directly sums the interactions between all
;  particles, so it is exact (to within machine precision) but scales
;  as N^2. The poten_tree routine is slightly (1%) less accurate, but
;  scales as NlogN. This routine is faster for <<10^4 particles,
;  because the algorithm is simpler. Furthermore, this routine can
;  calculate the potential in any dimension, whereas poten_tree is
;  restricted to 3 dimensions.
;
; INPUTS:
;  pos: A [ndim, n] array of n-dimensional particle locations
;  mass: A n element vector of masses
;
; OUTPUTS:
;  The potential energy of the system. It is assumed that G=1, so that
;  PE = sum_i (sum j > i (m_i * m_j / r_ij) )
;
; MODIFICATION HISTORY:
;  July 2010: Written by Chris Beaumont.
;-
function poten_slow, pos, mass
  ;- check inputs
  if n_params() ne 2 then begin
     print, 'calling sequence:'
     print, 'result = poten_slow(pos, mass)'
     return, !values.f_nan
  endif

  sz = size(pos)
  if size(pos, /n_dim) ne 2 then $
     message, 'pos must be a 2D array'

  ndim = sz[1] & npt = sz[2]

  eps = 1e-12
  result = 0.

  for i = 1L, npt - 1, 1 do begin
     dist = pos[*, 0:i-1]
     for j = 0, ndim - 1 do dist[j, *] -= pos[j,i]
     dist = sqrt(total(dist^2, 1))
     add = (mass[0:i-1] * mass[i]) * dist / (dist + eps)^2
     result += total(add)
  endfor
  return, result
end

;- see poten_tree for test routines
