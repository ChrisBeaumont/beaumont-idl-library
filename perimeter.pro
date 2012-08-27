;+
; PURPOSE:
;  Compute the perimeter of a polygon defined by a set of x/y points
;
; CALLING SEQUENCE:
;  p = perimeter(x, y)
;
; INPUTS:
;  x: List of x points
;  y: List of y points
;
; OUTPUTS:
;  Perimeter defined by connecting x/y pairs. The perimeter includes
;  the line segment from the last x/y pair to the first x/y pair.
;
; METHOD SUMMARY:
;  The length of the line segment connecting each x/y pair is computed
;  using the pythagorean theorem in double precision. Lengths are
;  summed.
;
; MODIFICATION HISTORY:
;  May 2012: Written by Chris Beaumont
;-
function perimeter, x, y

  x0 = double(x)
  x1 = shift(x0, 1)
  y0 = double(y)
  y1 = shift(y0, 1)
  dl = sqrt((x0 - x1) ^ 2 + (y0 - y1) ^ 2)
  return, total(dl)

end


;- tests for perimeter
pro test_perimeter

  x = [0., 0, 1, 1]
  y = [0., 1, 1, 0]
  p = perimeter(x, y)
  p_ans = 4
  assert, abs(p - p_ans) lt 1e-3

  x = [0, 0, 1]
  y = [0, 1, 0]
  p = perimeter(x, y)
  p_ans = 2 + sqrt(2)
  assert, abs(p - p_ans) lt 1e-3

  theta = arrgen(0., 2 * !pi, nstep = 200)
  x = cos(theta)
  y = sin(theta)
  p = perimeter(x, y)
  p_ans = 2 * !pi
  assert, abs(p - p_ans) le 1e-3

  print, 'tests passed'
end
