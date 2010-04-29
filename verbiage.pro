;+
; PURPOSE:
;  This procedure proivdes a mechanism to report messages of various
;  priorities within programs. A message is passed to verbiage, along
;  with the message's priority and the maximum priority to report. If the
;  message priority is equal to or less than the required report
;  priority, the message is printed to the terminal. Otherwise, the
;  program exits quietly.
;
;  For example, consider the following program:
;   pro talk, verbose = verbose
;     verbiage, 'report level is 1', 1, verbose
;     verbiage, 'report level is 2', 2, verbose
;   end
;
;   This program would produce the following messages:
;    IDL> talk, verbose =  0
;          [nothing happens]
;    IDL> talk, verbose = 1
;          'report level is 1'
;    IDL> talk, verbose = 2
;          'report level is 1'
;          'report level is 2'
;    IDL> talk
;          [nothing happens]
;
;  Note that, in the last case, verbiage handles the case where
;  'verbose' is undefined. This is nice, since keywords like 'verbose'
;  are usually optional.
;
;  The program is designed to facilitate a hierarchy of messages,
;  which the user can switch between. For example, verboose = 0 will
;  turn off all messages for running in batch mode. verbose = 4 might
;  produce very detailed output for debugging purposes.
;
;  When using verbiage in programs, I recommend the following
;  hierarchy for message priorities:
;   0: No messages have priority of 0
;   1: Messages indicate program failure
;   2: Messages tersely summarize program results
;   3: Detailed summary / progress report of a program
;   4: Debugging messages
;
; CATEGORY:
;  utilities
;
; CALLING SEQUENCE:
;  verbiage, msg, msg_lvl, report_lvl
;
; INPUTS:
;  msg: The message to (perhaps) print
;  msg_lvl: The priority of the message (see the recommendation above) 
;  report_lvl: The maximum priority to report. This is usually
;              provided by the end user, through a keyword like
;              verbose. The program gracefully handles the case where
;              report_lvl is missing. This means that, if report_lvl
;              is passed a value of a user defined keyword (like
;              verbose, for example), you don't need to check
;              to make sure that keyword was defined.
;
; SIDE EFFECTS:
;  The message is printed if report_lvl >= msg_lvl
;
; MODIFICATION HISTORY:
;  June 2009: Written by Chris Beaumont
;  December 2009: Messages are indented proportional to the
;                 calling function's depth in the stack.
;  April 2010: Fixed a bug that crashed verbiage if help, /trace
;              overflows onto multiple lines
;-
pro verbiage, msg, msg_lvl, report_lvl
  compile_opt idl2
  on_error, 2

  ;- message priority is too low to report. Exit.
  if ~n_elements(report_lvl) then return 
  if report_lvl lt msg_lvl then return
  if ~n_elements(msg) || msg eq '' then return
  ;- get the name of the procedure which called this one
  help, /trace, out = stack
  good = where(strmatch(stack, '%*'))
  stack = stack[good]
  depth = n_elements(stack)
  prog = stack[1]
  split = strsplit(prog, ' ', /extract)
  prog = split[1]
  print, strjoin(replicate(' ', depth/2)), '* '+prog+':  ' + msg
end

