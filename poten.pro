;+
; PURPOSE:
;  This function calculates the potential energy of a collection of
;  point masses. It is a wrapper to poten_slow and poten_tree, and
;  attempts to call the most efficient program.
;
; INPUTS:
;  pos: A [3, n] array of 3D positions
;  mass: An n-element vector of masses
;
; OUTPUTS:
;  The gavitational potential energy, given by
;  PE = sum_i (sum j > i (m_i * m_j / r_ij) )
;
; DESCRIPTION:
;  The poten_tree program scales as N log N, but has more overhead
;  than the N^2 poten_slow algorithm. Tests on a uniform grid of
;  particles suggest that the algorithms run in similar times for
;  ~10^4 particles. For larger systems, poten_tree is faster. This
;  algorithm calls poten_tree, with a theta of 1.5, when nobj > 10^3
;
; MODIFICATION HISTORY:
;  July 2010: Written by Chris Beaumont
;-
function poten, pos, mass
  nobj = n_elements(mass)
  if nobj lt 1d3 then return, poten_slow(pos, mass) $
  else return, poten_tree(pos, mass, theta = 1.5)
end
