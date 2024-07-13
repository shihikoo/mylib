PRO validate_enspec_tplot, tplot_var_name, t_s, t_e, average_time, error_message = error_message

; wether energy data loaded or not  
  tplot_names, tplot_var_name, names = names   
  IF names(0) EQ '' OR names(0) EQ '*' THEN BEGIN 
     error_message = error_message + 'No data loaded '+'('+tplot_var_name+'). '
  ENDIF ELSE BEGIN 
; data during display time need to be more than 2 
     get_data, tplot_var_name,  data = data, dlim = dlim, lim = lim
     index = where(data.x GT (t_s+1) AND data.x LE t_e, ct)
     IF ct LT 2 THEN BEGIN 
        error_message = error_message + 'Data are less than 2 '+'('+tplot_var_name+'). '
     ENDIF ELSE BEGIN   
; original data interval need to be longer than average_time*3
        n_time = N_ELEMENTS(index)
        stime = data.x(index(n_time-1))
        etime = data.x(index(0))
        IF FLOOR((stime-etime)/average_time) LE 3 THEN BEGIN  
           error_message = error_message + 'Data are too short to be averaged '+'('+tplot_var_name+'). '
        ENDIF ELSE BEGIN
; data cannot be averaged if data.y are all 0
;           IF  TOTAL(data.y(index, *)) EQ 0 THEN BEGIN 
;              error_message = error_message + 'Empty energy data '+'('+tplot_var_name+'). '
;           ENDIF ELSE BEGIN
              str = {x:data.x(index), y:data.y(index, *), v:data.v(index, *)}
              store_data, tplot_var_name, data = str, dlim = dlim, lim = lim
;           ENDELSE 
        ENDELSE 
     ENDELSE
  ENDELSE
END
