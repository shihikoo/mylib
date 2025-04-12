; --------------------------------------------------
; Purpose: load axis data for input plot_axis
; Inputs:  data, plot_axis, range,x_range,y_range,z_range,r_range
; Written by Jing Liao
; Written on 05/10/2021
; ------------------------------------------------
pro load_axis, data, plot_axis, data_pos, range, log, range_input = range_input
  ntime = n_elements(data.time)
  data_pos = dblarr(ntime, 3)
  range = fltarr(2, 3)
  log = intarr(3)

  for i = 0, 2 do begin
    if plot_axis[i] eq 'X_GSE' then begin
      data_pos[*, i]= data.gse_x
      range[*, i] = [15., -25.]
      log[i] = 0
    endif
    if plot_axis[i] eq 'Y_GSE' then begin
      data_pos[*, i]= data.gse_y
      range[*, i] = [25., -25.]
      log[i] = 0
    endif
    if plot_axis[i] eq 'Z_GSE' then begin
      data_pos[*, i]= data.gse_z
      range[*, i] = [-25., 25.]
      log[i] = 0
    endif
    if plot_axis[i] eq 'X_GSM' then begin
      data_pos[*, i]= data.gsm_x ;
      ; range[*, i] = [-30., 10.]
      range[*, i] = [-30., 10.]
      log[i] = 0
    endif
    if plot_axis[i] eq 'Y_GSM' then begin
      data_pos[*, i]= data.gsm_y
      ; range[*, i] = [-20., 20.]
      range[*, i] = [-20, 20.]
      log[i] = 0
    endif
    if plot_axis[i] eq 'Z_GSM' then begin
      data_pos[*, i]= data.gsm_z
      range[*, i] = [-20., 20.]
      log[i] = 0
    endif
    if plot_axis[i] eq 'MLT' then begin
      data_pos[*, i]= data.mlt
      range[*, i] = [0, 24]
      log[i] = 0
    endif
    if plot_axis[i] eq 'ILAT' then begin
      data_pos[*, i]= abs(data.ilat)
      range[*, i] = [0., 90.]
      log[i] = 0
    endif
    if plot_axis[i] eq 'DIST' then begin
      data_pos[*, i]= data.dist
      range[*, i] = [5., 25.]
      log[i] = 0
    endif
    if plot_axis[i] eq 'L' then begin
      data_pos[*, i]= data.l
      range[*, i] = [0., 30.]
      log[i] = 0
    endif
    if plot_axis[i] eq 'Beta' then begin
      data_pos[*, i]= data.beta
      range[*, i] = [1e-6, 1000]
      log[i] = 1
    endif

    if keyword_set(range_input) then range[*, 2] = range_input

    print, '    ' + plot_axis[i] + '     '
  endfor
end

; ------------------------------------------------------
; Purpose: load storm_phase flag for storm_phase_name
; Inputs:  data, storm_phase_name
; Written by Jing Liao
; Written on 05/10/2021
; ------------------------------------------------------
function load_storm_phase_flag, data, storm_phase_name
  if storm_phase_name eq 'initial_phase' then storm_phase = 1
  if storm_phase_name eq 'main_phase' then storm_phase = 2
  if storm_phase_name eq 'recovery' then storm_phase = 3
  if storm_phase_name eq 'storm_time' then storm_phase = [1, 2, 3]
  if storm_phase_name eq 'nonstorm_time' then storm_phase = 0
  if storm_phase_name eq 'all' then storm_phase = [0, 1, 2, 3, 4, 5]
  if storm_phase_name eq 'prestorm' then storm_phase = 5

  for i = 0, n_elements(storm_phase) - 1 do begin
    if i eq 0 then flag_phase = data.storm_phase eq storm_phase(i) $
    else flag_phase = (flag_phase + (data.storm_phase eq storm_phase(i))) gt 0
  endfor

  ; set all infinite flag value to nan
  flag_phase = float(flag_phase)
  index = where(flag_phase eq 0, ct)
  if ct gt 0 then flag_phase[index] = !values.f_nan

  RETURN, flag_phase
end

; -----------------------------------------------------------------
; Purpose: load substorm phase flag with input substorm phase name
; Inputs:  data, substorm_phase_name
; Written by Jing Liao
; Written on 05/10/2021
; -----------------------------------------------------------------
function load_substorm_phase_flag, data, substorm_phase_name
  if substorm_phase_name eq 'storm_time' then substorm_phase = [1]
  if substorm_phase_name eq 'nonstorm_time' then substorm_phase = [0]
  if substorm_phase_name eq 'all' then substorm_phase = [0, 1]

  ; flag_phase = FLOAT(flag_phase)
  ; index = WHERE(flag_phase EQ 0, ct)
  ; IF ct GT 0 THEN flag_phase(index) = !VALUES.F_NAN
  flag_phase = data.time
  flag_phase[*] = 1
  RETURN, flag_phase
end

; ------------------------------------------------------
; Purpose: load region flag for the input region (The region identification is in read_daily_data_multi.pro)
; Inputs:  data, region
; Written by Jing Liao
; Written on 05/10/2021
; ------------------------------------------------------
function load_region_flag, data, region
  case region of
    'all': flag_region = data.region lt 10
    'Lobe': flag_region = data.region eq 1
    'BL': flag_region = data.region eq 2
    'PS': flag_region = data.region eq 3
    'Dayside': flag_region = data.region eq 4
    'Tail': flag_region = data.region le 3
  endcase

  flag_region = float(flag_region)

  index = where(flag_region eq 0, ct)
  if ct gt 0 then flag_region[index] = !values.f_nan

  RETURN, flag_region
end

; ----------------------------------------------------------------
; Purpose: load all condition flags for o + beam mapping routine
; Inputs:
; Written by Jing Liao
; Written on 05/12/21
; ---------------------------------------------------------------
pro load_external_condition_flags, data, storm_phase, substorm_phase, region, flag_ext_condition
  flag_storm = load_storm_phase_flag(data, storm_phase)
  flag_substorm = load_substorm_phase_flag(data, substorm_phase)
  
  flag_region = load_region_flag(data, region)

  flag_ext_condition = flag_storm * flag_substorm * flag_region
end