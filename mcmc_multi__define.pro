;+
; PURPOSE:
;  mcmc_multi is a subclass of the mcmc class to create markov chain
;  monte carlo simulations on multiple chains simultaneously.
;
;  One of IDL's principal weaknesses is that, since it is an
;  interpreted scripting language, it is much slower than compiled
;  languages for repetitive tasks like looping. The mcmc_multi class
;  tries to mitigate this problem by vectorizing the mcmc code. In
;  this class, multiple chains are run simultaneously and, assuming
;  that the code which generates and evaluates trial links is
;  vectorized well, this may be substantially faster than running one
;  long mcmc chain.
;
;  To use this class, the user must supply ARRAYS for the seed and
;  data variables when initializing the object. Each element in the
;  array represents the seed/data for a particular chain. The final
;  set of chains fetched from the getChain method will have nchain
;  columns and nstep rows.
;
;  As with the mcmc class, the user must also override the
;  logTargetDistribution and selectTrial methods in a subclass of
;  mcmc_multi. These methods have the same signatures as in a normal
;  mcmc object, but the inputs and outputs must now be arrays, not
;  scalars. These objects should be written in a vectorized fashion,
;  otherwise mcmc_multi will probably have no speed bost over mcmc.
;
;  Some quick testing showed that mcmc_multi was about a factor of 2
;  slower than mcmc when run using only 1 chain, but was 10x faster
;  when comparing 100 chains to 1 chain with 100x as many steps.
;
; MODIFICATION HISTORY:
;  October 2009: Written by Chris Beaumont, adapted from the mcmc
;                class.
;  December 2009: Added tracking of maximum posterior
;-


;+
; PURPOSE:
;  Run all of the chains
;
; CATEGORY:
;  statistics
;
; COMMON BLOCKS:
;  mcmc_multi_common: Holds the seed variable for calls to randomu
;
; MODIFICATION HISTORY:
;  October 2009: Written by Chris Beaumont
;-
pro mcmc_multi::run
  compile_opt idl2
  common mcmc_multi_common, randSeed ;- save the random seed value

  current = *self.seed
  currentValue = self->logTargetDistribution(current)
  
  nsuccess = lonarr(self.nchain) & nfail = lonarr(self.nchain)
  thinStep = 0L
 
  for i = 0L, self.nstep - 1, 1 do begin
     
     ;- pick a new trial link
     trial = self->selectTrial(current, transitionRatio = transitionRatio)
     newValue = self -> logTargetDistribution(trial)
     hit = where(newValue gt *self.peakvals, hitct)
     if hitct ne 0 then begin
        (*self.peaks)[hit] = trial[hit]
        (*self.peakvals)[hit] = newValue[hit]
     endif

     ;- determine acceptance probability via MH algorithm
     alpha = exp(newValue - currentValue) * transitionRatio
     u = randomu(randSeed, self.nchain)

     good = where(u lt alpha, ngood, complement = bad, ncomplement = nbad)
     if ngood ne 0 then begin
        nsuccess[good]++
        current[good] = trial[good]
        currentValue[good] = newValue[good]
     endif
     if nbad ne 0 then nfail[bad]++
     
     ;- save links to chain
     if (++thinStep) eq self.thin then begin
        thinStep = 0
        (*self.chain)[*, i / self.thin] = current
        (*self.logf)[*, i/self.thin] = currentValue
     endif

  endfor  
  
  *self.nsuccess_multi = nsuccess & *self.nfail_multi = nfail

end

;+
; PURPOSE:
;  Return the chain link which have the maximum target distribution
;  value
; KEYWORD PARAMETERS:
;  value: The value of the maximum target distribution
;
; MODIFICATION HISTORY:
;  December 2009: Written by cnb.
;-
function mcmc_multi::getPeak, value = value
  value = *self.peakvals
  return, *self.peaks
end

;+
; PURPOSE:
;  This function returns the numbers of accepted and, optionally,
;  rejected trial links for each chain during the run
;
; KEYWORD PARAMETERS:
;  nfail: A named variable to hold the number of rejected links for
;  each chain during the run. An array
; 
; OUTPUTS:
;  The number of accepted links in each chain during the run. An
;  array. 
;-
function mcmc_multi::getNSuccess, nfail = nfail
  nfail = *self.nfail_multi
  return, *self.nsuccess_multi
end



;+
; PURPOSE:
;  Initialize the mcmc_multi object
;
; CATEGORY:
;  statistics
;
; INPTUS:
;  seed:  An array of starting seed links, one for each chain
;  nstep: The number of desired links for each chain
;  data:  An array of any relevant data, one for each chain
;
; KEYWORD PARAMETERS:
;  thin: If set to a non-negative integer, save only every THINth
;  links in the chain. Helpful for saving space with long chains

function mcmc_multi::init, seed, nstep, data, thin = thin
  compile_opt idl2
  on_error, 2

  if n_params() ne 3 then begin
     print, 'mcmc_multi calling sequence:'
     print, " obj = obj_new('mcmc_multi', seed, nstep, data)"
     return, 0
  endif

  if n_elements(thin) ne 0 && thin le 0 then $
     message, 'thin must be a positive number'
  
  self.thin = keyword_set(thin) ? thin : 1 

  self.seed = ptr_new(seed)
  self.data = ptr_new(data)
  self.nchain = n_elements(seed)
  self.peakvals = ptr_new(replicate(-!values.f_infinity, self.nchain))
  self.peaks    = ptr_new(seed)
  if n_elements(data) ne self.nchain then begin
     self->cleanup
     message, 'Data and Seed arrays do not have the same length.'
  endif

  self.chain = ptr_new(replicate(seed[0], self.nchain, nstep / self.thin))
  self.logf = ptr_new(dlbarr(self.nchain, nstep / self.thin))
  self.nsuccess_multi = ptr_new(replicate(0L, self.nchain))
  self.nfail_multi = ptr_new(replicate(0L, self.nchain))
  self.nstep = nstep
  return, 1
end


;+ 
; PURPOSE:
;  Free memory when we are finished
;
; CATEGORY:
;  statistics
;
; MODIFICATION HISTORY:
;  October 2009: Written by Chris Beaumont
;-
pro mcmc_multi::cleanup
  self->mcmc::cleanup
  ptr_free, self.nsuccess_multi
  ptr_free, self.nfail_multi
  ptr_free, self.peaks
  ptr_free, self.peakvals
  return
end


;+
; PURPOSE:
;  define the mcmc_multi data structure
;
; CATEGORY:
;  statistics
;
; MODIFICATION HISTORY:
;  October 2009: Written by Chris Beaumont
;-
pro mcmc_multi__define
  data = {mcmc_multi, $
          inherits mcmc, $
          nsuccess_multi : ptr_new(), $ ;- now an array to hold successes
          nfail_multi : ptr_new(), $    ;- ditto for failures
          nchain : 0, $                 ;- number of chains
          peaks : ptr_new(), $          ;- peak of posterior in each chain
          peakvals : ptr_new() $         ;- value of max posterior
         }
end
        
