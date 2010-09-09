;+
; PURPOSE:
;  Projects a given vector onto the vector space defined by the
;  principal compents, and returns the result
;
; INPUTS:
;  data: A vector to project
;
; KEYWORD PARAMETERS:
;  nterm: Set to an integer to project onto only the first nterm
;         principal components.
;  coeffs: Set to a variable to hold the coefficients of the linear
;         combination of principal components. In other words, result
;         = sum( coeff[i] * PC_i )
;
; OUTPUTS:
;  The projection of the input onto the principal components. This is
;  equivalent to sum(coeff[i] * PC_i) where coeff[i] = sum(data *
;  PC_i)
;
; MODIFICATION HISTORY:
;  March 2010: Written by Chris Beaumont
;  April 2010: Fixed a bug that crashed this function when called with
;              only one data point. cnb
;  April 2010: Number of returned coeffs matches nterm. cnb.
;  August 2010: Truncated intermediate arrays when nterm <
;               ndim. Speeds up execution. cnb.
;  August 10 2010: Further optimization when nterm < ndim. cnb.
;  August 18 2010: Fixed spelling error in definition of get_mean. cnb.
;-  
function pricom::project, data, nterm = nterm, coeffs = coeffs
  if ~keyword_set(nterm) then nterm = self.nevec else $
     nterm = nterm < self.nevec
  
  sz = size(data)
  if sz[1] ne self.ndim then message, 'input data not the correct dimension'
  mean = rebin(*self.mean, sz[1], sz[2])
  norm = data - mean
  
  ;- dot the data into each eigenvector. Get coeffs
  ;- self.evec: [ndim, ntraining]
  ;- norm: [ndim, ndata]
  ;- evec ## tr(norm) : [ndata, ntrain]
  if nterm lt self.nevec then $
     coeffs = (*self.evec)[*, 0:nterm-1] ## transpose(norm) $ ;- N data cols by M evec rows
  else $
     coeffs = (*self.evec) ## transpose(norm) ;- N data cols by M evec rows

  ;- truncate arrays to nterm
  result = norm * 0
  nobj = n_elements(data) / sz[1]
  for i = 0, nobj - 1 do $
     result[*, i] = total(rebin(coeffs[i,*], self.ndim, nterm) * $
                          (*self.evec)[*, 0:nterm-1], 2)
  return, result + mean
end

;+
; PURPOSE:
;  Return the principal components.
;
; INPUTS:
;  none
;
; OUTPUTS:
;  the principal components
;-
function pricom::get_pc
  return, *self.evec
end

function pricom::get_eval
  return, *self.eval
end

;+
; PURPOSE:
;  Get the mean of the training data
;-
function pricom::get_mean
  return, *self.mean
end

;+
; PURPOSE:
;  Build a new PCA object.
; 
; INPUTS:
;  data: an (ndim) by (nobj) array of training data. Contains (nobj)
;  vectors, each with (ndim) elements. These are the data used to
;  generate the principal components
;-
function pricom::init, data

  sz = size(data)
  nobj = sz[2]
  ndim = sz[1]
  cnb_pca, data, eval, evec, mean = mean

  nevec = n_elements(evec[0,*])
  norm_data = data - rebin(mean, ndim, nobj)

  ;- populate the object
  self.ndim = ndim
  self.nobj = nobj
  self.nevec = nevec
  self.data = ptr_new(norm_data)
  self.eval = ptr_new(eval)
  self.evec = ptr_new(evec)
  self.mean = ptr_new(mean)

  return, 1
end

;+
; PURPOSE:
;  Free all the pointers associated with the object, upon deletion
;-
pro pricom::cleanup
  ptr_free, self.eval
  ptr_free, self.evec
  ptr_free, self.data
  ptr_free, self.var
  ptr_free, self.mean
end

;+
; PURPOSE:
;  This class provides a more convenient interface for working with
;  principal component analysis. The user supplies a training data
;  set, which is used to generate principal components via the IDL
;  procedure PCOMP. The methods in this class then allow a simple way
;  to project new vectors onto PCA space, obtain the principal
;  components, etc.
;
; MODIFICATION HISTORY:
;  April 2010: Written by Chris Beaumont
;  June 11 2010: Class now uses the cnb_pca function to calculate
;  principal components. cnb.
;-
pro pricom__define
  data = {pricom, nobj : 0, ndim: 0, nevec: 0, data : ptr_new(), $
          eval : ptr_new(), evec : ptr_new(), var: ptr_new(), mean : ptr_new()}
end
