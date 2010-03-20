;+
; PURPOSE:
;  This class implements the Likelihood abstract class to compute
;  likelihoods using a 1D gaussian as the model distribution.
;
; CATEGORY:
;  Statistics
;
; MODIFICATION HISTORY:
;  Sep 2009: Written by Chris Beaumont
;-


;+
; PURPOSE:
;  Computer the log-likelihood of the data under a given model.
;
; OUTPUTS:
;  The log-likelihood of the data
;
; INPUTS:
;  model: A two dimensional vector giving [mu, sigma].
;-
function gauss_like::loglikelihood, model
  mu = model[0]
  sigma = model[1]
  norm = 1 / sqrt(2 * !dpi * sigma^2D)
  return, total( alog(norm) - (*self.data - mu)^2 / (2D * sigma^2D))
end


;+
; PURPOSE:
;  Compute the likelihood of the data under a given model
;
; OUTPUTS:
;  The likelihood of the data under model.
;
; INPUTS:
;  model: A 2 element vector giving [mu, sigma] of the model gaussian.
;
;-
function gauss_like::likelihood, model
  mu    = model[0]
  sigma = model[1]
  norm = 1 / sqrt(2 * !dpi * sigma^2D)
  return, product(norm * exp(-(*self.data - mu)^2D / (2D * sigma^2D)))
end


;+
; PURPOSE:
;  Instantiate the object
;
; CATEGORY:
;  Statistics
;-
pro gauss_like__define
  define = {gauss_like, inherits likelihood}
end
