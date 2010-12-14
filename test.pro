pro test

  o = obj_new('comparator', 1, 5)
  os = replicate(o, 5)

  for i = 0, 4, 1 do print, os[i]->getData()
  os[i]->setData, 2
  for i = 0, 4, 1 do print, os[i]->getData()
  obj_destroy, o
  obj_destroy, os

end
