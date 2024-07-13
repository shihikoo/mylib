
PRO write_text_to_file, filename, text, APPEND = APPEND
  spawn, 'mkdir -p ' + FILE_DIRNAME(filename)
  
  OPENU, unit, filename, APPEND = APPEND,/GET_LUN 
  PRINTF, unit, text
  FREE_LUN, unit ;  ,/FORCE
;  close, /all
END
