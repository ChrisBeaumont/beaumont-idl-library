;+
; PURPOSE: 
;  This function computes an upper limit for the strength of a signal
;  embedded in a sequence of noisy measurements.
;
; CATEGORY:
;  Statistics
;
; INPUTS:
;  signal: The SUM of n independent measurements.
;  noise: The per-measurement noise standard deviation. Currently, the
;         noise is assumed to be normally distributed.
;      n: The number of measurements that went into the sum
;     mu: The per-measurement background level.
; 
; KEYWORD PARAMETERS:
;  confidence: The posterior probability that the true flux is less
;              than the reported upper limit. The default is .998650
;              (3-sigma, one tailed).
;  plot: If set, plot the posterior distribution and upper limit.
;
; OUTPUTS:
;  An upper limit for any INTEGRATED source flux (that is, summed up
;  over all measurements) embedded in the signal.
;
; PROCEDURE:
;  Signal is assumed to be the sum of n independent measurements from
;  a process given by y = mu + source_flux + eps, where eps is
;  gaussian noise with mean zero and variance sigma. The procedure
;  employs a Bayesian approach to find an upper limit for source_flux
;  * n. A prior enforces that source_flux >= 0. The program returns
;  the value f_crit = source_flux_crit * n such that the posterior
;  probability that f_true < f_crit is equal to confidence
;
; TODO:
;  Add in Poisson (and other) noise models
;
; MODIFICATION HISTORY:
;  Sep 2009: Written by Chris Beaumont
;  Sep 14 2009: Added input parameter checking. cnb.
;- 
function upperlim, signal, noise, n, mu, confidence = confidence, plot = plot
  compile_opt idl2
  on_error, 2

  ;- parse input
  if n_params() ne 4 then begin
     print, 'upperlim calling sequence:'
     print, ' result = upperlim(signal, noise, n, mu, [confidence = confidence, /plot]'
     return, !values.f_nan
  endif

  if ~keyword_set(confidence) then confidence = 0.998650

  ;- assume a gaussian noise pattern for now
  sig_zero = (0D + mu * n - signal) / (noise * sqrt(n))
  cdf_zero = gauss_pdf(sig_zero)
  cdf = confidence * (1 - cdf_zero) + cdf_zero

  ;- want to solve gauss_pdf(x) = rootGoal
  x = gauss_cvf(1 - cdf)
  result = x * noise * sqrt(n) + signal - mu * n

  ;- plot the posterior distribution, if requested
  if keyword_set(plot) then begin
     loval = upperlim(signal, noise, n, mu, confidence = 1d-6)
     midval = upperlim(signal, noise, n, mu, confidence = 0.5)
     hival = upperlim(signal, noise, n, mu, confidence = 1 - 1d-6)
     
     xs = arrgen(loval, hival, nstep = 10000)
     sigs = (xs + mu * n - signal) / (noise * sqrt(n))
     ys = 1 / sqrt(2D * !dpi * n * noise^2) * exp(-(xs + mu * n - signal)^2 / (2 * n * noise^2))
     ys /= (1D - cdf_zero)
     plot, xs, ys, charsize = 2, yra = minmax(ys)
     oplot, result + [0,0], minmax(ys), color = fsc_color('red')
     oplot, midval + [0,0], minmax(ys), color = fsc_color('green')
     
                                ;- some sanity checks
                                ;print, int_tabulated(xs, ys)
                                ;hit = where(xs lt midval)
                                ;print, int_tabulated(xs[hit], ys[hit])
                                ;hit = where(xs lt result)
                                ;print, int_tabulated(xs[hit], ys[hit]), confidence
  endif
  
  return, result


end
