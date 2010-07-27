;+
; PURPOSE:
;  The otree class implements the oct tree data structure, used for
;  calculating the potential energy of a system of point masses. The
;  data structure, and its utility in N body simulations, is discussed
;  by Barnes and Hut 1986 (Nature v324 p446).
;
;  The oct tree recursively partitions a 3D cell into 8 subsells. New
;  points are added to the tree such that each point corresponds to a
;  leaf, and all points are separate from one another. 
;
; MODIFICATION HISTORY:
;  July 2010: Written by Chris Beaumont
;-

;+ 
; PURPOSE:
;  create a new oct tree node
;
; INPUTS:
;  pos: A 3-element vector, defining a particle position
;  mass: The particle's mass
;  bounds: A [2,3] array denoting the lower and upper bounds for each
;  dimension. Note that pos must lie within this boundary.
;
; OUTPUTS:
;  1 for success
;
; MODIFICATION HISTORY:
;  July 2010: Written by Chris Beaumont
;-
function otree::init, pos, mass, bounds
  self.pos = pos
  self.mass = mass
  self.tot_mass = mass
  self.com = pos
  self.bounds = bounds
  return, 1
end


;+
; PURPOSE:
;  Initialize one of this child's nodes as an empty leaf.
;
; INDEX:
;  The index (0-7) of the node to create
;
; PROCEDURE:
;  If child[index] already exists, the program exists
;  quietly. Otherwise, an empty node with the appropriate boundaries
;  is created.
;-
pro otree::addEmptyChild, index
  if obj_valid(self.children[index]) then return

  index3 = [index mod 2, (index / 2) mod 2, index / 4]

  ;- center of current cell
  cen = total(self.bounds, 1)/2.
  ;- half width of cell
  delt = (self.bounds[1,*] - self.bounds[0,*])/2.
  ;- new center
  cen += delt/2 * (2 * index3 - 1)
  bounds = transpose( [ [cen - delt/2], [cen + delt/2] ])
  if max(bounds[1,*] le bounds[0,*]) then stop
  self.children[index] = obj_new('otree', cen, 0., bounds)
end


;+
; PURPOSE:
;  Free the pointers to each child node
;-
pro otree::cleanup
  for i = 0, 7 do obj_destroy, self.children[i]
end

;+
; PURPOSE:
;  return one of the children
; 
; INPUTS:
;  index: The index to return
;
; OUTPUTS:
;  children[index], an otree object
;-
function otree::getChild, index
  return, self.child[index]
end


;+
; PURPOSE:
;  get all of the children
;
; OUTPUTS:
;  An 8 element array of chldren otree nodes
function otree::getChildren
  return, self.children
end

;+
; PURPOSE:
;  This procedure inserts a new point mass into the system,
;  re-arranging leaves if necessary
;
; INPUTS:
;  pos: A 3 element position array
;  mass: A scalar mass
;
; PROCEDURE:
;  The tree is traversed recursively until an appropriate empty leaf
;  is found, and the new particle is added. If the leaf is non-empty,
;  then the leaf is expanded into a node, and both masses are inserted
;  into one of the sub-cells
;-
pro otree::insert, pos, mass
  
  ;- adding zero mass would screw things up. don't do it!
  if mass eq 0 then return

  ;- update node's total mass, com
  self.com = (self.com * self.tot_mass + pos * mass) / (self.tot_mass + mass)
  self.tot_mass += mass
  
  ;- cell is empty. add and exit
  if self.tot_mass eq mass then begin
     assert, self->isLeaf()
     self.pos = pos             
     self.mass = mass
     return
  endif else begin 

     ;- insert into the proper sub-cell
     self->pickChild, pos, ind1, index
     self->addEmptyChild, ind1
     self.children[ind1]->insert, pos, mass

     ;- is a particle already living here?
     ;- if so, insert into a subcell
     if self.mass ne 0 then begin
        self->pickChild, self.pos, ind1, index
        self->addEmptyChild, ind1
        self.children[ind1]->insert, self.pos, self.mass
        ;- clear the node
        self.pos = [0,0,0]
        self.mass = 0.
     endif
  endelse  
end


;+
; PURPOSE:
;  This function calculates the potential energy between a point mass
;  and the system of particles already added to the tree.
;
; INPUTS:
;  pos: A 3 element position
;  mass: A scalar mass
;
; KEYWORD PARAMETERS:
;  neval: On output, will hold the total number of inter-particle
;  interactions. 
;  theta: Set to a value to control the precision of the
;  algorithm. Smaller numbers lead to higher accuracies and longer
;  calculations. theta=1 gives about 1% error for most
;  input. theta=1.5 is just as good for a uniform grid of particles,
;  when n_particle > 10^3
;
; OUTPUTS:
;  The potential energy between the input point and the system. 
;
; PURPOSE:
;  This is based on the Barnes Hut algorithm, which treates sub-trees
;  as single point masses if the center of mass is sufficiently
;  distant from the input position. This reduces the number of
;  computations to O(N log N).
;
;  Note that interactions of particles within 10^-12 of each other are
;  ignored. This is meant handle the case when one wants to calculate
;  the potential energy of a particle already in the system; the
;  interaction with itself is ignored.
;
; MODIFICATION HISTORY:
;  July 2010: Written by Chris Beaumont
;-
function otree::calcPotential, pos, mass, neval = neval, theta = theta
  if ~keyword_set(theta) then theta = 1. else theta = float(theta)
  eps = 1e-12 ;- avoids div by zero errors

  delt = sqrt(total((pos - self.com)^2))

  len = self.bounds[1,*] - self.bounds[0,*]
  sep = min(delt/len)

  ;- case 1: sep > 1 / theta. Approximate cell as a point particle
  if self->isLeaf() || sep gt 1 / theta then begin                                
     neval = 1
     return, mass * self.tot_mass * delt / (delt + eps)^2
  endif else begin
     result = 0
     neval = 0
     for i = 0, 7 do begin
        if ~obj_valid(self.children[i]) then continue
        result += self.children[i]->calcPotential(pos, mass, neval = n, theta = theta)
        neval+=n
     endfor
     return, result
  endelse
  ;- can't get here
  return, !values.f_nan
end


;+
; PURPOSE:
;  This function ensures that the tree structure is still sane. It
;  traverses the tree, checking a variety of assumptions
;
; OUTPUTS:
;  1 if the tree passes all sanity checks
;-
function otree::enforceAssertions, verbose = verbose

  ;- mass bookkeeping correct?
  m = 0.
  for i = 0, 7 do if obj_valid(self.children[i]) then m+= self.children[i]->getTotalMass()
  m += self.mass
  if abs(m - self.tot_mass) gt 1e-3 then begin
     if keyword_set(verbose) then print, 'Mass error: ', m, self.tot_mass
     return, 0
  endif

  ;- com bookkeeping correct?
  com = [0.,0.,0.]
  for i = 0, 7 do if obj_valid(self.children[i]) then $
     com += self.children[i]->getCOM() * self.children[i]->getTotalMass()
  com += self.com * self.mass
  com /= self.tot_mass
  if max(abs(com - self.com)) gt 1e-4 then begin
     if keyword_set(verbose) then print, 'COM bookkeeping error'
     return, 0
  endif

  ;- bounds must be in correct order
  if max(self.bounds[0, *] ge self.bounds[1,*]) then begin
     if keyword_set(verbose) then print, 'Bounds are inconsistent'
     return, 0
  endif

  ;- a valid leaf?
  if self->isLeaf() then begin

     ;-position must be within bounds
     if max(self.pos lt self.bounds[0,*]) eq 1 || $
        max(self.pos gt self.bounds[1,*]) eq 1 then begin
        if keyword_set(verbose) then print, 'position out of bounds'
        return, 0
     endif
                                
     if self.tot_mass ne self.mass then begin
        if keyword_set(verbose) then print, 'leaf mass inconsistent'
        return, 0
     endif

     if max(abs(self.com - self.pos)) gt 1e-4 then begin
        if keyword_set(verbose) then print, 'leaf com inconsistent: ', self.com, self.pos
        return, 0
     endif

  endif else begin

     if self.mass ne 0 then begin
        if keyword_set(verbose) then print, 'non-leaf node has a particle'
        return, 0
     endif

     ;- a valid node?
     for i = 0, 7 do $
        if obj_valid(self.children[i]) && $
        ~self.children[i]->enforceAssertions(verbose = verbose) then return, 0
  endelse

  return, 1
end


;+
; PURPOSE:
;  This procedure determines which of the 8 sub-cells a point belongs
;  in
;
; INPUTS:
;  pos: The position of the point
; 
; OUTPUTS:
;  ind1: The 1-dimensional index of the subcell
;  ind3: The 3-dimensional index of the subcell
;-
pro otree::pickChild, pos, ind1, ind3
  ind3 = (pos gt total(self.bounds, 1)/2.)
  ind1 = ind3[0] + ind3[1] * 2 + ind3[2] * 4
end

;+
; PURPOSE:
;  Determines if the current node is a leaf
;
; OUTPUTS:
;  1 if a leaf, 0 otherwise
;-
function otree::isLeaf
  return, max(obj_valid(self.children)) eq 0
end

;+
; PURPOSE:
;  Returns the total mass of this cell and all sub-cells
;-
function otree::getTotalMass
  return, self.tot_mass
end


;+
; PURPOSE:
;  Returns the center of mass of this subcell
;-
function otree::getCOM
  return, self.com
end

;+
; PURPOSE: 
;  get the mass uniquely associated with this node. This is nonzero if
;  and only if the node is a leaf
;-
function otree::getLeafMass
  return, self.mass
end

;+
; PURPOSE:
;  Get the position of the point mass located in this leaf. If the
;  node is not a leaf, this number is meaningless (and probably [0,0,0])
;-
function otree::getPos
  return, self.pos
end

;+
; PURPOSE:
;  Defines the otree data structure
;-
pro otree__define
  data = {otree, pos : fltarr(3), $ ;- particle position
          mass : 0., $              ;- particle mass
          com : fltarr(3), $        ;- center of mass of this cell, and all sub-cells
          tot_mass: 0., $           ;- total mass of this subcell, and all sub-cells
          bounds : fltarr(2, 3), $  ;- the boundaries of this cell, along each dimension
          children : replicate(obj_new(), 8)} ;- the sub-cells of this cell
  return
end


pro test

  pos = randomu(seed, 3, 100)
  mass = randomu(seed, 100)
  tree = obj_new('otree', 0, 0, [[0, 1], [0, 1], [0, 1]])
  for i = 0, 99, 1 do begin
     tree->insert, pos[*,i], mass[i]
  endfor
  print, total(mass), tree->getTotalMass()
  print, 'com'
  print, tree->getCOM()
  print, [total(pos[0,*] * mass) / total(mass), $
          total(pos[1,*] * mass) / total(mass), $
          total(pos[2,*] * mass) / total(mass)]
  print, 'good tree? ', tree->enforceAssertions(/verbose)
  obj_destroy, tree

end

