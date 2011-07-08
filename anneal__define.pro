;+
; PURPOSE:
;  The anneal class performs simulated annealing to maximize a fitness
;  function. This is a little different than normal, where annealing
;  minimizes a penalty (or energy) function.
;
;  Simulated annealing maximizes a function by taking random steps in
;  parameter space from the current input. At each step, the function
;  to maximize is evaluated given the current input. If the input
;  yields a higher fitness than the previous input, it is
;  automatically accepted and the old input discarded. If it is lower,
;  than it is accepted with a probability that decreases as the
;  simulation continues. The process continues, drawing new random
;  inputs and occasionally swapping out the old inputs, until the
;  simulation ends. Thus, the process transitions from a random walk
;  to a hill-climbing algorithm; this helps prevent getting stuck at
;  local maxima.
;
; CATEGORY:
;  Optimization
;
; MODIFICATION HISTORY:
;  Feb 2010: Written by Chris Beaumont
;-


;+
; PURPOSE:
;  This function selects the next trial input, given the current
;  input. This is not implemented; each individual problem must
;  subclass this class, overriding the selectTrial method.
;
; INPUTS:
;  state: The current input
;
; OUTPUTS:
;  A new input to try
;-
function anneal::selectTrial, state
  message, 'Function not implemented'
  return, 0
end


;+
; PURPOSE:
;  This function evaluates the target function on a given
;  input. Again, this function must be overridden in a subclass in
;  order for the procedure to work. 
;
; INPUTS:
;  state: The input to evaluate
;
; OUTPUTS:
;  The f(state), where f is the function to maximize
;-
function anneal::fitness, state
  message, 'Function not implemented'
  return, 0
end


;+
; PURPOSE:
;  This function calculates the "temperature" at a given iteration of
;  the simulation. This should probably be overridden for specific
;  implementations, though a default cooling function is provided.
;
; INPUTS:
;  step: The iteration number of the simulation
;
; OUTPUTS:
;  The temperature at the current step
;-
function anneal::temperature, step
  T0 = 10
  eta = .99
  return,  T0 * exp(step * alog(eta))
end


;+
; PURPOSE:
;  This function returns the best set of inputs encountered during the
;  run
;
; KEYWORD PARAMETERS:
;  fitness: Set to a named variable to hold the function's
;  output at the returned set of inputs
;
; OUTPUTS:
;  The set of input which yielded the maximum fitness value
;-
function anneal::getBestState, fitness = fitness
  fitness = self.best_fitness
  return, *self.best_state
end


;+
; PURPOSE:
;  Returns the best maximum function output encountered during the
;  simulation
;
; KEYWORD PARAMETERS:
;  state: Set to a named variable to hold the input corresponding to
;  the maximum function output
;
; OUTPUTS:
;  The maximum function output
;-
function anneal::getBestFitness, state = state
  state = *self.best_state
  return, self.best_fitness
end


;+
; PURPOSE:
;  Returns the sequence of inputs attempted during the
;  simulation. Only works if /SAVE was set during object creation
;
; KEYWORD_PARAMETERS:
;  fitnesses: Set to a named variable to hold all of the function
;  evaluations.
;
; OUTPUTS:
;  An array of all the states 
;_
function anneal::getAllStates, fitnesses = fitnesses
  if ~self.doSave then return, !values.f_nan
  if arg_present(fitnesses) then fitnesses = *self.fitnesses
  return, *self.states
end


;+
; PURPOSE:
;  Returns all of the function evaluations computed during the
;  simulation. Only works if /SAVE was set during object creation.
;
; KEYWORD_PARAMETERS:
;  states: Set to a named variable to hold all of the inputs tried.
;
; OUTPUTS:
;  All of the function evaluations during the simulation
;-
function anneal::getAllFitnesses, states = states
  if ~self.doSave then return, !values.f_nan
  if arg_present(states) then states = *self.states
  return, *self.fitnesses
end


;+
; PURPOSE:
;  This procedure actually runs the annealing simulation.
;-
pro anneal::run
  common anneal_seed, seed
  
  ;- initialize variables
  state = *self.start_state
  fitness = self->fitness(state)
  best_fitness = -!values.f_infinity
  doSave = self.doSave
  nstep = self.nstep

  if doSave then begin
     states = replicate(state, nstep)
     fitnesses = replicate(0D, nstep)
  endif

  ;- run the procedure
  lastprint = systime(/seconds)
  for i = 0, nstep - 1, 1 do begin
     if self.verbose ne 0 then begin
        time = systime(/seconds)
        if (time - lastprint) gt self.verbose then begin
           print, i, temperature, best_fitness, $
                  format='("Step, T, fitness: ", i10, 2x, 2(e0.5, 2x))'
           lastprint = time
        endif
     endif

     temperature = self->temperature(i)
     new_state = self->selectTrial(state)
     new_fitness = self->fitness(new_state)
     prob = exp((new_fitness - fitness) / temperature)

     if randomu(seed) lt prob then begin
        state = new_state
        fitness = new_fitness
     endif
     if (fitness gt best_fitness) then begin
        best_fitness = fitness
        best_state = state
     endif

     if doSave then begin
        states[i] = state
        fitnesses[i] = fitness
     endif
  endfor

  ;- save the variables
  self.best_state = ptr_new(best_state)
  self.best_fitness = best_fitness
  if doSave then begin
     self.states = ptr_new(states)
     self.fitnesses = ptr_new(fitnesses)
  endif
  
end


;+
; PURPOSE:
;  This function initializes the object
;
; INPUTS:
;  start_state: The seed input value to start the simulation on.
;  nstep: The number of iterations to run the simulation for
; 
; KEYWORD_PARAMETERS:
;  save: Set to a nonzero value to save every step of the
;  simulation. These can be retrieved using the getFitnesses and
;  getStates methods, but will use more memory
;  verbose: Set to 1 to print output during calculation.
;-
function anneal::init, start_state, nstep, save = save, verbose = verbose
  if n_params() ne 2 then begin
     print, 'annel calling sequence'
     print, "obj = obj_new('anneal', nstep, [/start])"
     return, 0
  endif

  self.start_state = ptr_new(start_state)
  self.nstep = nstep
  self.doSave = keyword_set(save)
  self.states = ptr_new()
  self.fitnesses = ptr_new()
  self.best_state = ptr_new()
  self.verbose = keyword_set(verbose) ? verbose : 0
  return, 1
end


;+
; PURPOSE:
;  Frees the heap variables when this object is destroyed
;-
pro anneal::cleanup
  ptr_free, self.start_state
  ptr_free, self.best_state
  ptr_free, self.states
  ptr_free, self.fitnesses
end


;+
; PURPOSE:
;  Defines the annealing class
;-
pro anneal__define
  data = {anneal, $
          start_state : ptr_new(), $
          best_state : ptr_new(), $
          best_fitness : 0D, $
          states : ptr_new(), $
          fitnesses : ptr_new(), $
          nstep : 0L, $
          verbose:0., $
          doSave : 0B}
end
