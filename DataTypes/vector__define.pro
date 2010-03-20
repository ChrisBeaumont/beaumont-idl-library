;+
; PURPOSE:
;  These routines define a vector data type. This vector class differs
;  from an array in that it grows as necessary to accomodate new
;  data. This enhancement comes at some expense of convenience:
;   -- The vector carries the baggage associated with objects (they
;      must be created with obj_new, and destroyed with obj_destroy)
;   -- Manipulating the data inside a vector is more cumbersome (it
;      must be done through the vector methods, instead of simple
;      syntax like array[3:5] = 10. Adding data is also less flexible;
;      data must be added in contiguous chunks (as opposed to
;      operations like array[[1,2,5,7]] = [9,8,7,6]
;  This vector class is distinct from the stack class, in that data
;  can be accessed and modified anywhere in a vector. In a stack,
;  objects can only be added to and retrieved from the top of the stack.
;  
; CATEGORY:
;  Data Types
;
; CALLING SEQUENCE:
;  newVector = obj_new('vector')
;
; CLASS STRUCTURE:
;  class = {vector, 
;           data : ptr_new()  ;- pointer to the vector data
;           top  : 0L         ;- the highest occupied index in data
;
; MODIFICATION HISTORY:
;  May 2009: Written by Chris Beaumont
;-

;+
; PURPOSE:
;  This procedure adds a set of values at a specified starting index
;  in the vector. Any old values in overlapping indices will be
;  overwritten. The vector expands as necessary to accomodate the
;  insertion.
;
;
; INPUTS:
;  vals: A scalar or array of values to add
;  ind: The first index into which vals should be inserted.
;
; EXAMPLE:
;  Assume an initial vector of [1,2,3]
;  vector->insertAt, [4,5], 3 would produce [1,2,3,4,5]
;  vector->insertAt, [9], 1 would produce [1, 9, 3]
;  vector->insertAt, 10, 5 would produce [1,2,3,?,10]
;                    (the ? value will be some random number)
;-
pro vector::insertAt, vals, ind

  ;- check inputs
  if n_params() ne 2 then begin
     print, 'vector->insertAt calling sequence:'
     print, 'vector->insertAt, vals, ind'
     return
  endif

  if n_elements(ind) ne 1 then $
     message, 'ind must be a scalar'

  if ind lt 0 then $
     message, 'ind must be >= 0'

  ;- add data
  self->ensureCapacity, vals, ind
  (*self.data)[ind] = vals
end


;+
; PURPOSE:
;  This procedure sets a subset of the vector to a specified value
;
; INPUTS:
;  value: The value to set the specified vector elements to
; 
; OPTIONAL INPUTS:
;  first: The first index of the vector to set to value. Defaults to
;         zero.
;  last:   The last index of the vector to set to value. Defaults to
;         self.top. 
;
; KEYWORD PARAMETERS:
;  firstInd: The same as the first argument. If both are present, the
;            keyword overrides the value of the argument
;  lastInd: The same as the last argument. If both are present, the
;           keyword overrides the value of the argument
;
; EXAMPLE:
;  Assume an inital vector of [1,2,3,4,5]
;  vector->fill, 0 would yield [0,0,0,0,0]
;  vector->fill, 9, 2,3 would yield [1,2,9,9,5]
;  vector->fill, -1, lastInd = 3 would yield [-1, -1, -1, 4, 5]
;  vector->fill, -1, firstInd = 1 would yield [1, -1, -1, -1, -1]
;-
pro vector::fill, value, first, last, firstInd = firstInd, lastInd = lastInd
  if n_params() lt 1 then begin
     print, 'vector->fill calling sequence:'
     print, 'vector->fill, value, [first, last, firstInd = firstInd, lastInd = lastInd]'
     return
  endif


  ;- determine start and ending indices
  if n_params() eq 1 and ~keyword_set(firstInd) then first = 0
  if n_params() lt 3 and ~keyword_set(lastInd) then last = self.top
  if keyword_set(lastInd) then last = lastInd
  if keyword_set(firstInd) then first = firstInd

  ;- make sure the indices are sane
  if first lt 0 then $
     message, 'value for first must be >= 0'
  if first gt last then $
     message, 'value for first must be <= last'

  insert = replicate(value, last - first + 1)
  self->ensureCapacity, insert, first
  (*self.data)[first] = insert
end

 
;+
; PURPOSE:
;  This procedure adds a set of values to the end of the vector
;
; INPUTS:
;  vals: The values to add
;
; EXAMPLE:
;  Assume an initial vector of [1,2,3]
;  vector->append, 10 would yield [1,2,3,10]
;  vector->append, [4,5,6] would yield [1,2,3,4,5,6]
;-
pro vector::append, vals
  
  if n_params() ne 1 then begin
     print, 'vector->append calling sequence:'
     print, 'vector->append, vals'
     return
  endif

  top = self.top
  self->ensureCapacity, vals, top + 1
  (*self.data)[top+1] = vals
end


;+
; PURPOSE:
;  This procedure wipes out the old data associated with the vector.
;-
pro vector::delete
  self.top = -1L
  ptr_free, self.data
end


;+
; PURPOSE:
;  This procedure updates the size of the vector to accommodate adding
;  new data. It also updates the value of top
;
; INPUTS:
;  vals: A scalar or array of values to be added to the vector
;   ind: The starting index at which vals are to be added  
;-
pro vector::ensureCapacity, vals, ind
  sz = n_elements(vals)
  maxind = ind + sz - 1

  ;- special case: The data pointer is empty
  if ~ptr_valid(self.data) then begin
     self.data = ptr_new(replicate(vals[0], maxind + 1), /no_copy)
     self.top = maxind
     return
  endif

  capacity = n_elements(*self.data)
  
  ;- grow the array as necessary
  if capacity le maxind then begin
     newsize = 2 * capacity > maxind
     newdata = replicate((*self.data)[0], newsize)
     newdata[0] = *self.data
     *self.data = newdata
  endif

  ;- update the value for top as necessary
  self.top = self.top > maxind
end


;+
; PURPOSE:
;  This function retrieves the data from the specified indices of a
;  vector
;
; INPUTS:
;  ind: The index or indices of the vector from which data should be
;  fetched. 
;
; OUTPUTS:
;  The value of the vector at the requested indices
;-
function vector::getValues, ind
  if n_params() ne 1 then begin
     print, 'vector->getValues calling sequence:'
     print, 'vector->getValues, ind'
     return, !values.f_nan
  endif

  ;- make sure that the requested indices are allowed
  bound = minmax(ind)
  if bound[0] lt 0 or bound[1] gt self.top then $
     message, 'Requested indices lie outside the vector boundaries'
  return, (*self.data)[ind]
end


;+
; PURPOSE:
;  This function returns the vector as an array
;
; OUTPUTS:
;  The vector in array form
;-
function vector::toArray
  if ~ptr_valid(self.data) || self.top lt 0 then return, !values.f_nan
  return, (*self.data)[0:self.top]
end


;+
; PURPOSE:
;  This procedure is called automatically when the object is
;  destroyed. It deletes the data pointer
;-
pro vector::cleanup
  if ptr_valid(self.data) then ptr_free, self.data
end


;+
; PURPOSE:
;  This function is called automatically by obj_new. It sets the
;  initial values of the class structure
;-
function vector::init
  self.top=-1L
  return, 1
end


;+
; PURPOSE:
;  This procedure instantiates a new vector object
;-
pro vector__define
  data={vector, data : ptr_new(), top:0L}
end

