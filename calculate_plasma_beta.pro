; --------------------------------------------------------------
; Purpose: validate pressure tplot for plasma beta calculation
; --------------------------------------------------------------

pro validate_pressure_tplot, tplot_var_name, error_message = error_message
  tplot_names, tplot_var_name, names = names
  if names[0] eq '' then begin
    error_message = error_message + 'No data loaded ' + '(' + tplot_var_name + ')'
  endif else begin
    get_data, names[0], data = data
    if n_elements(data.x) lt 2 then begin
      error_message = error_message + 'Data are less than 2 elements ' + '(' + tplot_var_name + '). '
    endif
    if total(data.x, /nan) eq 0 then begin
      error_message = error_message + 'No valid data ' + '(' + tplot_var_name + '). '
    endif
  endelse
end

; ---------------------------------------------------------------------------------
; Purpose: calculate plasma beta from H+, O+, and magnetic pressure
;
; Inputs: h1_pressure_name
; mag_pressure_name
; o1_pressure_name
; Keywords: beta_name
; p_total_name
; error_message
;
; Created by Jing Liao
; Created on 03/13/2021
; ---------------------------------------------------------------------------------

pro calculate_plasma_beta, h1_pressure_name, o1_pressure_name, mag_pressure_name, o1_density_name, h1_density_name, beta_name, p_total_name, density_ratio_name,pressure_ratio_name, error_message = error_message
  if ~keyword_set(error_message) then error_message = ''

  validate_pressure_tplot, h1_pressure_name, error_message = error_message
  validate_pressure_tplot, o1_pressure_name, error_message = error_message
  validate_pressure_tplot, mag_pressure_name, error_message = error_message

  if error_message ne '' then begin
    print, error_message
    RETURN
  endif

  get_data, h1_pressure_name, data = h1_pressure_data
  get_data, o1_pressure_name, data = o1_pressure_data
  get_data, mag_pressure_name, data = mag_pressure_data
  get_data, o1_density_name, data = o1_density_data
  get_data, h1_density_name, data = h1_density_data

  ; The pressure data sometimes has -9e10 data, which is nan data. We
  ; are now handling this in average varible routine.
  ; index = where(o1_pressure_data.y le -1e10,ct)
  ; IF ct GT 0 THEN o1_pressure_data.y(index) = !VALUES.F_NAN

  ; o1_pressure_data_int = interpol(o1_pressure_data.y, o1_pressure_data.x, h1_pressure_data.x)
  ; mag_pressure_data_int = interpol(mag_pressure_data.y, mag_pressure_data.x, h1_pressure_data.x)

  store_data, beta_name, data = {x: h1_pressure_data.x, y: (h1_pressure_data.y + o1_pressure_data.y) / mag_pressure_data.y}
  options, beta_name, 'ytitle', 'Plasma Beta'
  ylim, beta_name, 0.001, 10, 1

  store_data, p_total_name, data = {x: h1_pressure_data.x, y: h1_pressure_data.y + o1_pressure_data.y + mag_pressure_data.y}
  options, p_total_name, 'ytitle', 'Total Pressure'
  ylim, p_total_name, 0.1, 100, 1

  store_data, density_ratio_name, data = {x: h1_density_data.x, y: o1_density_data.y / h1_density_data.y}
  options, density_ratio_name, 'ytitle', 'Density Ratio'
  ylim, density_ratio_name, 1e-3, 10., 1

  store_data, pressure_ratio_name, data = {x: h1_pressure_data.x, y: o1_pressure_data.y / h1_pressure_data.y}
  options, pressure_ratio_name, 'ytitle', 'Pressure Ratio'
  ylim, pressure_ratio_name, 1e-3, 10., 1

; stop
end
