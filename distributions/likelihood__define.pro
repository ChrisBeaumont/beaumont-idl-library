;+
; PURPOSE:
;  This is an abstract class for objects dealing with
;  likelihoods. LIKELIHOOD objects are a subset of DISTRIBUTION
;  objects that specifically deal with data.
;
; CATEGORY:
;  Statistics
;
; NOTES:
;  The likelihood and loglikelihood methods are not implemented for
;  this abstract class, and trying to use these methods on a raw
;  likelihood object will generate an error message. Subclasses should
;  override these methods
;
; MODIFICATION HISTORY:
;  Sep 2009: Written by Chris Beaumont.
;-


;+ 
; PURPOSE:
;  Return the natural logarithm of the likelihood, given a model for
;  the distribution.
;
; INPUTS:
;  model: Data specifying the model parameters.
;
; OUTPUTS:
;  Ln( Probability( data | model) )
;- 
function likelihood::loglikelihood, model
  message, 'Log-Likelihood method not implemented in this abstract class'
  return, !values.f_nan
end


;+
; PURPOSE:
;  This method returns the likelihood of the data, given a set of
;  model parameters. 
;
; INPUTS:
;  model: The model parameters.
;
; OUTPUTS:
;  Probability(data | model)
;-
function likelihood::likelihood, model
  message, 'Likelihood method not implemented in this abstract class.'
  return, !values.f_nan
end


;+
; PURPOSE:
;  Implements the DISTRIBUTION class's pdf method, which is
;  identical to the likelihood.
;-
function likelihood::pdf, model
  return, self->likelihood(model)
end


;+
; PURPOSE:
;  Create the likelihood object
;
; INPUTS:
;  data: The data used in likelihood evaluation
;
; OUTPUTS:
;  1 for success
;- 
function likelihood::init, data
  self.data = ptr_new(data)
  return, 1
end

;+
; PURPOSE:
;  Redefine the data for this likelihood object
;
; INPUTS:
;  data: The new data
;-
pro likelihood::setData, data
  *self.data = data
end

;+
; PURPOSE:
;  Return the data used in this object
;
; OUTPUTS:
;  the data
;-
function likelihood::getData
  return, *self.data
end


;+
; PURPOSE:
;  Destroy the object instance.
;-
pro likelihood::cleanup
  ptr_free, self.data
end


;+
; PURPOSE:
;  Instantiate the likelihood object
;-
pro likelihood__define
  define = {likelihood, inherits distribution, data : ptr_new()}
end
