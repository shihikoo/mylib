PRO calculate_delayed_solarwind_parameter,varname, outvarname, min_time, time
  get_data, varname, data = data
   
  output = DBLARR(N_ELEMENTS(time))
  output(*) = !VALUES.F_NAN

  FOR ii = 0, N_ELEMENTS(time)-1 DO BEGIN 
     IF FINITE(min_time(ii)) THEN BEGIN 
        index = WHERE(data.x GE min_time(ii) and data.x LE time(ii),ct)
        IF ct GT 0 THEN output[ii] = TOTAL(data.y[index],/NAN)/ct
     ENDIF 
  ENDFOR 

  store_data, outvarname, data ={x:time, y:output}

END

