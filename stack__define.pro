compile_opt idl2
;+ 
; PURPOSE:
;  This class provides an array-based implementation of a stack.
;  Elements can be added into a stack (a 'push') and retrieved in
;  a last in, first out manner (via 'peek' or 'pop' methods).
;
; CATEGORY:
;  data types
;
; METHODS:
;  push(data): Add data (scalar or vector) to the stack. Return 1 for success.
;  peek() : Retrieve, but do not remove the head of the stack
;  peek(num): Retrieve, but do not remove, the top num elements on the
;             stack.
;  pop() : Retrieve and remove the top of the stack
;  pop(num): Retrieve and remove the top num elements of the stack
;  isEmpty() : Return a 1 if the stack is empty
;  toArray() : Return the contents of the stack as an array
;  getSize() : Return the number of elements in the array
;  contains(num): Return 1 if num is already in the stack somewhere,
;                 else return 0
;
; INTERNAL METHODS:
;  ensureCapacity(num) : Increase the size of array as necessary to 
;                        safely insert num more elements.
;  fetch(num, /remove) : Driver for peek and pop methods.
;
; MODIFICATION HISTORY:
;  January 2009: Written by Chris Beaumont
;  Oct 2009: Added /NOCOPY keyword to toArray(). cnb.
;  April 2010: Added contains() method. cnb
;-

;+
; PURPOSE:
;  Push data onto the stack
;
; CALLING SEQUENCE:
;  stack->push, data
;
; INPUTS:
;  data: The data to push onto the stack, a scalar or vector. The data
;  should be of the same type as the rest of the stack
;-
pro STACK::push, data
  ;- check inputs
  sz = size(data)
  num = sz[n_elements(sz) - 1]
  type = sz[n_elements(sz) - 2]
  if num eq 0 then begin
    print, 'Calling sequence: Stack->Add(data)'
    print,'                   data: Scalar or array'
    return
  endif
  
  ;- check for type mismatches
  if (self.type ne 0) && (self.type ne type) then begin
     message, /continue, 'Warning: Stack data type different then input data.'
  endif

  ;- case 1 - object data is empty
  if ~ptr_valid(self.data) then begin
    self.data = ptr_new(data)
    self.capacity = num
    self.size = num
    self.type = type
    return
  endif
  
  ;- case 2 - object data exists.
  self -> ensureCapacity, num
  (*(self.data))[self.size : self.size + num - 1] = data
  self.size +=  num
  return
end


;+
; PURPOSE:
;  pop elements off the stack, removing them from the stack.
;
; CALLING SEQUENCE:
;  result = stack->pop([num])
;
; INPUTS:
;  num: The number of elements to remove. Defaults to 1
;
; OUTPUTS:
;  The popped elements.
;-
function stack::pop, num
  if n_elements(num) eq 0 then num = 1
  return, self->fetch(num, /remove)
end


;+
; PURPOSE:
;  return, but don't remove, elements from the top of the stack
;
; CALLING SEQUENCE:
;  result = stack->peek([num])
;
; INPUTS:
;  num: The number of elements to return. Defaults to 1
;
; OUTPUTS:
;  The num topmost elements of the stack
;-
function stack::peek, num
  if n_elements(num) eq 0 then num = 1
  return, self->fetch(num)
end

;+ 
; PURPOSE:
;  Determine whether num exists in the stack already
;-
function stack::contains, num
  hit = where(*(self.data) eq num, ct)
  return, ct ne 0
end


;+
; PURPOSE:
;  internal  driver for peek and pop
;-
function stack::fetch, num, remove = remove
  compile_opt idl2, hidden
  if n_elements(num) eq 0 then num = 1
  if (num gt self.size) then $
     message, 'Fetching more elements than are present in the stack: ' + $
              strtrim(string(num),2)

  ind = self.size - 1 - findgen(num)
  if keyword_set(remove) then self.size -= num
  return, (*(self.data))[ind]
end


;+
; PURPOSE:
;  Test whether the stack has any data
; 
; CALLING SEQUENCE:
;  result = stack->isEmpty()
;
; OUTPUTS:
;  1 if the stack is empty, 0 if not
;-
function stack::isEmpty
  return, self.size eq 0
end


;+
; PURPOSE:
;  Return the number of elements in the stack 
;
; CALLING SEQUENCE:
;  result = stack->getSize()
;
; OUTPUTS:
;  The number of elements in the stack
;-
function stack::getSize
  return, self.size
end


;+ 
; PURPOSE:
;  Return the stack in array form
;
; CALLING SEQUENCE:
;  result = stack->toArray()
;
; INPUTS:
;  count: On output, holds the number of elements in the stack.
;
; OUTPUTS:
;  The stack in array form, or -1 if the tack is empty
;-  
function stack::toArray, count, nocopy = nocopy
  count = ptr_valid(self.data) ? self.size : 0
  if ptr_valid(self.data) then begin
     if keyword_set(nocopy) then $
        return, temporary((*(self.data))[0:self.size - 1]) $
     else $
        return, (*(self.data))[0:self.size - 1]
  endif else return, -1
end


;+
; PURPOSE:
;  An internal method to ensure the stack is big enough for 
;  push events
;-
pro stack::ensureCapacity, num
  compile_opt idl2, hidden

  if (num + self.size) lt self.capacity then return
  while (num + self.size) ge self.capacity do $
     self.capacity *= 2 + 1
  temp = replicate((*(self.data))[0], self.capacity)
  temp[0] = *(self.data)
  ptr_free, self.data
  self.data = ptr_new(temp, /no_copy)
return
end


;+
; PURPOSE:
;  clean up the stack upon deletion, deleting pointers
;-
pro stack::cleanup
  compile_opt idl2, hidden
  ptr_free, self.data
end


;+
; PURPOSE:
;  Define stack data structure
;-
pro STACK__DEFINE
  compile_opt idl2, hidden
  obj = {stack, data : ptr_new(), size : 0, capacity : 0, type : 0}
end
