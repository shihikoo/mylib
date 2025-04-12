PRO average_data_y, data, tag, average_time,time_avg, y_avg=y_avg, yfound=yfound, sumup = sumup
  n_avg = N_ELEMENTS(time_avg)
  
  str_element, data, tag, value, SUCCESS=yfound
  IF yfound EQ 0 THEN RETURN

  data_y = value
  
  index = where(data_y LE -9e10,ct)
  IF ct GT 0 THEN data_y[index] = !VALUES.F_NAN 

  size_y = SIZE(data_y)  
  dim_y = size_y[0]
  IF dim_y GT 3 THEN RETURN
  IF dim_y EQ 3 THEN y_avg = DBLARR(n_avg, size_y[2], size_y[3])
  IF dim_y EQ 2 THEN y_avg = DBLARR(n_avg, size_y[2]) 
  IF dim_y EQ 1 THEN y_avg = DBLARR(n_avg)
  
  if dim_y eq 0 then y_avg = DBLARR(n_avg)
  
; calculate mean of the data for time points
  FOR itime = 0l, n_avg-1 DO BEGIN
     IF dim_y EQ 3 THEN index = where(data.x GE time_avg[itime]-average_time/2 AND data.x LT time_avg[itime]+average_time/2 AND TOTAL(TOTAL(data_y,2,/NAN),2,/NAN) NE 0, ct)
     IF dim_y EQ 2 THEN index = where(data.x GE time_avg[itime]-average_time/2 AND data.x LT time_avg[itime]+average_time/2 AND TOTAL(data_y,2,/NAN) NE 0, ct)
     IF dim_y EQ 1 THEN index = where(data.x GE time_avg[itime]-average_time/2 AND data.x LT time_avg[itime]+average_time/2 AND TOTAL(data_y, /NAN) NE 0, ct)
     
     IF ct GT 0 AND TOTAL(ABS(data_y[index, *]) GE 0,/NAN) GT 0  THEN BEGIN
        IF KEYWORD_SET(sumup) THEN  BEGIN
           y_avg[itime, *] = TOTAL(data_y[index, *],1, /NAN)
        ENDIF  ELSE BEGIN
           y_avg[itime, *] = TOTAL(data_y[index, *], 1, /NAN)/TOTAL(FINITE(data_y[index, *]),1)
        ENDELSE
     ENDIF ELSE y_avg[itime, *] = !VALUES.F_NAN
  ENDFOR
;stop
END   


; PROCEDURE: average_tplot_variable with the given average time array
;
; PURPOSE: to time average any tplot variable with the given average
; time arrangement 
;
; INPUT:
;        var -> variable name - Array of strings or numbers. The '*'
;                               is also accepted.
;        avg -> averaging time in seconds
;        time_avg -> time array
;
; KEYWORD:
;         NEW_NAME -> the averaged variable is stored with a new name that
;                     includes the avereging time
;
; CREATED by: J. Liao
;
; LAST MODIFIED: 03/17/21


PRO average_tplot_variable_with_given_time, var, average_time, time_avg, NEW_NAME = NEW_NAME, sumup = sumup, keep_v = keep_v
  
  COMMON get_error, get_err_no, get_err_msg, default_verbose
  
  get_err_no = 0 & get_err_msg = ''
  
  IF average_time LE 0 THEN BEGIN
     PRINT, 'the avg time must be > 0'
     RETURN
  ENDIF

  at_str = STRCOMPRESS(ROUND(average_time),  /REMOVE_ALL) 

;--------------------------------------------------------------------
; Find the names of the variables to be averaged
;--------------------------------------------------------------------
  tplot_names, var, NAMES=var_names
  
  IF NOT KEYWORD_SET(var_names) THEN RETURN
  IF var_names[0] EQ '' THEN RETURN
  new_var_names = strarr(N_ELEMENTS(var_names))
;--------------------------------------------------------------------
; Loop through all variables
;--------------------------------------------------------------------
  FOR iv = 0, n_elements(var_names)-1 DO BEGIN
     IF strpos(var_names[iv], '_AVG') NE -1 THEN RETURN

;------------------------------------------------------------------
; Extract data information from tplot variable
;------------------------------------------------------------------
     
     time_trim_tplot_variable, var_names[iv], min(time_avg)-average_time/2, max(time_avg)+average_time/2,keep_v=keep_v

     get_data, var_names[iv], data=data, dlim=dlim, lim=lim

     average_data_y,data,'Y', average_time, time_avg, y_avg = y_avg, yfound = yfound
     
     IF yfound EQ 0 THEN BEGIN
        PRINT, 'data.y is not found'
        RETURN
     ENDIF
     
     average_data_y, data,'V', average_time, time_avg, y_avg=v_avg, yfound = vfound , sumup = sumup
     if keyword_set(keep_v) then v_avg = data.v
     
     average_data_y, data,'dy', average_time, time_avg,y_avg=dy_avg, yfound = dyfound, sumup = sumup

;------------------------------------------------------------------
; If keyword NEW_NAME is set the time averaged variable is stored
; with a new name that includes the averaging time
;------------------------------------------------------------------
     IF KEYWORD_SET(NEW_NAME) THEN BEGIN
        new_var_name = var_names[iv] + '_AVG' + at_str
     ENDIF ELSE BEGIN
        new_var_name = var_names[iv]
     ENDELSE
 
;     datastr = data
;     datastr.x = time_avg
;     datastr.y = y_avg
;     IF vfound EQ 1 THEN datastr.v = v_avg
;     IF dyfound EQ 1 THEN datastr.dy = dy_avg

     IF vfound EQ 0 AND dyfound EQ 0 THEN datastr = {x:time_avg, y:y_avg}
     IF vfound EQ 1 AND dyfound EQ 0 THEN datastr = {x:time_avg, y:y_avg, v:v_avg}
     IF vfound EQ 0 AND dyfound EQ 1 THEN datastr = {x:time_avg, y:y_avg, dy:dy_avg}
     IF vfound EQ 1 AND dyfound EQ 1 THEN datastr = {x:time_avg, y:y_avg, v:v_avg, dy:dy_avg}

     str_element, data, 'pabins', value, SUCCESS=ebfound
     IF ebfound EQ 1 THEN datastr = {x: time_avg, y:y_avg, v:v_avg, pabins:data.pabins}
     
     str_element, data, 'energybins', value, SUCCESS=ebfound
     IF ebfound EQ 1 THEN datastr = {x: time_avg, y:y_avg, energybins:data.energybins}

     str_element, data, 'average_time', value, SUCCESS=beamFound
     IF beamFound EQ 1 THEN BEGIN
        datastr = {x: time_avg, y:y_avg, v:v_avg, start_time:data.start_time, END_time: data.end_time, average_time: data.average_time}
     ENDIF 
 
     store_data, new_var_name, data=datastr, dlim=dlim, lim=lim

     new_var_names[iv] = new_var_name
;------------------------------------------------------------------
; Set plot attributes for new variable names
;------------------------------------------------------------------
     IF KEYWORD_SET(NEW_NAME) THEN BEGIN
        IF strpos(var_names[iv], '_SP') NE -1 THEN BEGIN
           specie = strmid(var_names[iv], strpos(var_names[iv], '_SP') + 3, 1)
        ENDIF
        sat = strmid(var_names[iv], strpos(var_names[iv], '_SC') + 3, 1)
        
        specie_str = ['H!U+!N','He!U++!N','He!U+!N','O!U+!N']
        
        IF strpos(var_names[iv], '_UN') NE -1 THEN BEGIN
           dumstr = strmid(var_names[iv], $
                           strpos(var_names[iv], '_UN')+3,$
                           strlen(var_names[iv])-1)
           units_name = strmid(dumstr, 0, strpos(dumstr, '_'))
           CASE STRUPCASE(units_name) OF
              'COUNTS': uname = 'COUNTS'
              'BCOUNTS': uname = '1/tof_bin'
              'NCOUNTS': uname = '1/bin'
              'RATE': uname = '1/s'
              'NRATE': uname = '1/s-bin'
              'EFLUX': uname = 'eV/cm!E2!N-s-sr-eV'
              'DIFFFLUX': uname = '1/cm!E2!N-s-sr-(eV/e)'
              'DISTFUNC': uname = 's!E3!N/cm!E3!N-km!E3!N'
           ENDCASE
           
        ENDIF
        
        IF STRMID(var_names[iv],0,6) EQ 'ENSPEC' THEN BEGIN
           options, new_var_name, 'spec',1
           options, new_var_name, 'x_no_interp',1
           options, new_var_name, 'y_no_interp',1
           options, new_var_name, 'ytitle', 'SC' + string(sat, format='(i1.1)') + '!C!C' +  specie_str(specie)+' (eV)'
           options, new_var_name, 'ztitle', uname
           ylim,    new_var_name,  20., 4.5e4, 1
        ENDIF
        
        IF STRMID(var_names[iv],0,6) EQ 'PASPEC' THEN BEGIN
           options, new_var_name, 'spec',1
           options, new_var_name, 'x_no_interp',1
           options, new_var_name, 'y_no_interp',1
           options, new_var_name, 'ytitle', 'SC' + string(sat, format='(i1.1)') + '!C!C' +  specie_str(specie)+' (deg)'
           options, new_var_name, 'ztitle', uname
           ylim,    new_var_name,  0., 360., 0
        ENDIF
        
        IF STRMID(var_names[iv],0,3) EQ 'MAG' THEN BEGIN
           ylim,    new_var_name,  0., 0., 0
        ENDIF
        
     ENDIF
     
     next:
  ENDFOR

END

