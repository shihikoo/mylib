; purpose: check the existence of varname. If doesn't exist,
; the write into log file.

pro log_error,   store_tplot, data_filename, log_filename, t_s,t_e

  COMMON SHARE1,ENERGY_BINS, DENERGY_BINS, PA_BINS, ERROR_MESSAGE

  
  IF KEYWORD_SET(store_tplot)  THEN  BEGIN             
     tplot_save, filename = data_filename  
     spawn,'gzip -9f '+data_filename+'.tplot'               
  ENDIF   
  
  IF ERROR_MESSAGE  NE ''  THEN BEGIN
     write_text_to_file, log_filename, TIME_STRING(t_s) + ' TO '+ TIME_STRING(t_e) + ERROR_MESSAGE, /APPEND
  ENDIF 

end 
