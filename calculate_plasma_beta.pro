;--------------------------------------------------------------
;Purpose: validate pressure tplot for plasma beta calculation
;--------------------------------------------------------------

PRO validate_pressure_tplot, tplot_var_name, error_message = error_message
  tplot_names, tplot_var_name, names = names
  IF names(0) EQ '' THEN BEGIN 
     error_message = error_message + 'No data loaded ' + '('+tplot_var_name+')'
  ENDIF  ELSE BEGIN 
     get_data, names(0), data = data
     IF N_ELEMENTS(data.x) LT 2 THEN BEGIN 
        error_message = error_message + 'Data are less than 2 elements '+'('+tplot_var_name+'). '
     ENDIF
     IF TOTAL(data.x,/NAN) EQ 0 THEN BEGIN 
        error_message = error_message + 'No valid data '+'('+tplot_var_name+'). '
     ENDIF
  ENDELSE
END

;---------------------------------------------------------------------------------
;Purpose: calculate plasma beta from H+, O+, and magnetic pressure
;
;Inputs: h1_pressure_name
;        mag_pressure_name
;        o1_pressure_name
;Keywords: beta_name
;          p_total_name
;          error_message
;
;Created by Jing Liao
;Created on 03/13/2021
;---------------------------------------------------------------------------------

PRO  calculate_plasma_beta, h1_pressure_name, o1_pressure_name, mag_pressure_name, o1_density_name, h1_density_name, beta_name, p_total_name, density_ratio_name, error_message=error_message

  IF NOT KEYWORD_SET(error_message) THEN error_message = ''
 
  validate_pressure_tplot, h1_pressure_name, error_message = error_message
  validate_pressure_tplot, o1_pressure_name, error_message = error_message
  validate_pressure_tplot, mag_pressure_name, error_message = error_message

  IF error_message NE '' THEN RETURN
 
  get_data, h1_pressure_name, data = h1_pressure_data
  get_data, o1_pressure_name, data = o1_pressure_data
  get_data, mag_pressure_name, data = mag_pressure_data
  get_data, o1_density_name, data = o1_density_data
  get_data, h1_density_name, data = h1_density_data

; The pressure data sometimes has -9e10 data, which is nan data. We
; are now handling this in average varible routine.
;  index = where(o1_pressure_data.y le -1e10,ct)
;  IF ct GT 0 THEN o1_pressure_data.y(index) = !VALUES.F_NAN

;  o1_pressure_data_int = interpol(o1_pressure_data.y, o1_pressure_data.x, h1_pressure_data.x)
;  mag_pressure_data_int = interpol(mag_pressure_data.y, mag_pressure_data.x, h1_pressure_data.x)
  
  store_data, beta_name, data={x:h1_pressure_data.x , y:(h1_pressure_data.y + o1_pressure_data.y) / mag_pressure_data.y}
  options, beta_name, 'ytitle', 'Plasma Beta' 
  ylim, beta_name, 0.001, 10, 1

  store_data, p_total_name, data={x:h1_pressure_data.x , y: h1_pressure_data.y + o1_pressure_data.y + mag_pressure_data.y}
  options, p_total_name, 'ytitle', 'Total Pressure' 
  ylim, p_total_name, 0.1, 100, 1

  store_data, density_ratio_name, data={x:h1_density_data.x , y: o1_density_data.y/h1_density_data.y}
  options, density_ratio_name, 'ytitle', 'Density Ratio' 
  ylim, density_ratio_name, 1e-3, 10., 1
  
END 


