;+
; PURPOSE:
;  The loopLister object class provides a standard way to provide
;  status updates during long looping operations. A loopLister object
;  is used as follows:
;    
;    ;- create the object
;    list = obj_new('looplister', numIterations, reportFrequency)
;    
;   for i = 0, numIterations, 1 do begin
;      list->report, i
;      <other code here>
;    endfor
;   
;    ;- destroy the object
;    obj_destroy, list
;
;  At each invocation of the method list->report, the list object
;  checks to see if more than reportFrequency seconds have passed
;  since its last report. If so, then it prints some information about
;  how much time has elapsed and remains in the looping process. Note
;  that, as with all objects, the obj_destroy procedure must be called
;  to dispose of the object when it is done. Otherwise, it hangs
;  around and can take up memory (use heap_gc to delete all unused
;  objects)
;
; CATEGORY:
;  Utilities
;
;-
 
;+ 
; PURPOSE:
;  Report how the loop is progressing, if enough time has passed since
;  the last report.
;
; CALLING SEQUENCE:
;  looplister->report, index
;
; INPUTS:
;  index: The current loop variable value
;
; KEYWORD PARAMETERS:
;  out: A variable to hold the reported message as a string. If this
;  keyword is present, then nothing will be printed to the screen.
;
; MODIFICATION HISTORY:
;  Early 2009: Written by Chris Beaumont
;  June 2009: Added out keyword. cnb.
;  Oct 2009: If it is not yet time to report, the OUT variable is
;            reset to an empty string. This was done to make
;            looplister work nicely with verbiage 
;-    
pro looplister::report, index, out = out
  noprint = arg_present(out)
  out = ''

  ;- do we want to report?
  currentTime = systime(/seconds)
  if currentTime lt self.nextTime then return

 
  ;- yes we do. Update self.nextTime
  self.nextTime = floor(1 + (currentTime - self.startTime) / self.frequency) * self.frequency + self.startTime
  
  ;- estimate the time remaining
  percentDone = 100D * index / self.iterLimit
  elapsedTime = currentTime - self.startTime
  estTime = elapsedTime / (percentDone / 100D) - elapsedTime
  
  ;- form the report message
  out = string(self.procedureName, self.label, percentDone, $
                  string(10B), $
                  format = '("Looping through ", a, a, ": ", ' + $
                  'f0.1, "% completed",a)')
  out += string(string(9B), time2string(elapsedTime), $
                string(10B), string(9B), time2string(estTime), $
                format = '(a, a, " elapsed, ", a, a, a, " remaining")')
  ;- print it?
  if (noprint) then return      ;- no
  print, out                    ;- yes

end
  

;+
; PURPOSE:
;  Setup the looplister object
;
; CALLING SEQUENCE:
;  lister = obj_new('looplister', iterLimit, frequency)
;
; INPUTS:
;  iterLimit: the value of the iteration variable which terminates
;             iteration
;  frequency: The number of seconds that should pass between each
;             progress report. If the report method is called more
;             frequently than this number, all intermediate
;             invocations will produce no output
;
; KEYWORD PARAMETERS:
;  label: A user supplied label, which looplister will produce at each
;         report.
;
; OUTPUTS:
;  1, for success
;
; MODIFICATION HISTORY:
;  Early 2009: Written by Chris Beaumont
;  June 2009: Fixed bug that caused a crash when obj_new was invoked
;             from the main level. cnb.
;  Oct 2009: Fixed bug that caused report times to drift around during
;            long loops. Removed START keyword
;-
function looplister::init, iterLimit, frequency, label = label
  ;- parse procedure name
  help,/trace, out=out
  ;- handle the case when the object was created at the main level
  sz = n_elements(out)
  if sz eq 3 then begin
     name = '$MAIN$'
  endif else begin
     name = out[3]
     name = strsplit(name, ' ', /extract)
     name = name[1]
  endelse
  self.procedureName=name
  self.startTime = systime(/seconds)
  self.iterLimit = iterLimit
  self.frequency = frequency
  self.nextTime = self.startTime + self.frequency
  if keyword_set(label) then self.label = ' --- ' + strupcase(label) $
  else self.label = ''
  return, 1
end


;+
; PURPOSE:
;  Definition of the looplister class data
;
; CALLING SEQUENCE:
;  lister = obj_new('looplister', iterLimit, frequency)
;-
pro looplister__define
  loop={looplister, $
        procedureName:'', $ ;- name of procedure looplister is invoked from
        startTime:0D, $     ;- system time at time of invocation
        nextTime: 0D, $     ;- system time at time of next report
        iterLimit: 0L, $    ;- number of iterations in the loop
        frequency : 0D, $   ;- how often to wait between reports
        label: '' $         ;- optional label produced by looplister
  }
end
