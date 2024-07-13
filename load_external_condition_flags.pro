;--------------------------------------------------
; Purpose: load axis data for input plot_axis
; Inputs:  data, plot_axis, range,x_range,y_range,z_range,r_range
; Written by Jing Liao
; Written on 05/10/2021
;------------------------------------------------
PRO load_axis, data, plot_axis, data_pos, range, log, range_input = range_input
  ntime = N_ELEMENTS(data.time)
  data_pos = DBLARR(ntime, 3) 
  range = FLTARR(2, 3)
  log = INTARR(3)

  FOR i = 0, 2 DO BEGIN 
     IF PLOT_AXIS(i) EQ 'X_GSE' THEN BEGIN 
        data_pos(*, i) = data.gse_x
        range(*, i) = [15., -25.]
        log(i) = 0
     ENDIF 
     IF PLOT_AXIS(i) EQ 'Y_GSE' THEN BEGIN 
        data_pos(*, i) = data.gse_y 
        range(*, i) = [25., -25.]
        log(i) = 0
     ENDIF 
     IF PLOT_AXIS(i) EQ 'Z_GSE' THEN BEGIN 
        data_pos(*, i) = data.gse_z 
        range(*, i) = [-25., 25.] 
        log(i) = 0
     ENDIF 
     IF PLOT_AXIS(i) EQ 'X_GSM' THEN BEGIN 
        data_pos(*, i) = data.gsm_x ;
;        range(*, i) = [-30., 10.] 
        range(*,i) = [-30., 10.]
        log(i) = 0
     ENDIF 
     IF PLOT_AXIS(i) EQ 'Y_GSM' THEN BEGIN 
        data_pos(*, i) = data.gsm_y
;          range(*, i) = [-20., 20.]
        range(*, i) = [-20, 20.]
        log(i) = 0
     ENDIF 
     IF PLOT_AXIS(i) EQ 'Z_GSM' THEN BEGIN 
        data_pos(*, i) = data.gsm_z
        range(*, i) = [-20., 20.] 
        log(i) = 0
     ENDIF 
     IF PLOT_AXIS(i) EQ 'MLT' THEN BEGIN 
        data_pos(*, i) = data.mlt
        range(*, i) = [0,24]
        log(i) = 0
     ENDIF 
     IF PLOT_AXIS(i) EQ 'ILAT' THEN BEGIN 
        data_pos(*, i) = ABS(data.ilat)
        range(*, i) = [0.,40.]  
        log(i) = 0
     ENDIF 
     IF PLOT_AXIS(i) EQ 'DIST' THEN BEGIN 
        data_pos(*, i) = data.dist
        range(*, i) = [5., 25.] 
        log(i) = 0
     ENDIF 
     IF PLOT_AXIS(i) EQ 'L' THEN BEGIN 
        data_pos(*, i) = data.l
        range(*, i) =  [0.,30.]
        log(i) = 0
     ENDIF 
     IF PLOT_AXIS(i) EQ 'Beta' THEN BEGIN 
        data_pos(*, i) = data.beta
        range(*, i) = [1e-6, 1000]
        log(i) = 1
     ENDIF 

     if keyword_set(range_input) then range(*,2) = range_input
     
     PRINT, '    '+PLOT_AXIS(i)+'     '
  ENDFOR   
END

;------------------------------------------------------
; Purpose: load storm_phase flag for storm_phase_name
; Inputs:  data, storm_phase_name
; Written by Jing Liao
; Written on 05/10/2021
;------------------------------------------------------
FUNCTION load_storm_phase_flag, data, storm_phase_name
  IF storm_phase_name EQ 'initial_phase' THEN storm_phase = 1
  IF storm_phase_name EQ 'main_phase' THEN storm_phase = 2
  IF storm_phase_name EQ 'recovery' THEN storm_phase = 3
  IF storm_phase_name EQ 'storm_time' THEN storm_phase = [1,2,3]
  IF storm_phase_name EQ 'nonstorm_time' THEN storm_phase = 0
  IF storm_phase_name EQ 'all' THEN storm_phase = [0,1,2,3,4,5]
  IF storm_phase_name EQ 'prestorm' THEN storm_phase = 5

  FOR i = 0, N_ELEMENTS(storm_phase)-1 DO BEGIN
     IF i EQ 0 THEN flag_phase = data.storm_phase EQ storm_phase(i) $
     ELSE flag_phase = (flag_phase + (data.storm_phase EQ storm_phase(i))) GT 0 
  ENDFOR
  
; set all infinite flag value to nan 
  flag_phase = FLOAT(flag_phase)
  index = WHERE(flag_phase EQ 0, ct)
  IF ct GT 0 THEN flag_phase(index) = !VALUES.F_NAN
  
  RETURN, flag_phase
END  

;-----------------------------------------------------------------
; Purpose: load substorm phase flag with input substorm phase name
; Inputs:  data, substorm_phase_name
; Written by Jing Liao
; Written on 05/10/2021
;-----------------------------------------------------------------
FUNCTION load_substorm_phase_flag, data, substorm_phase_name
  IF substorm_phase_name EQ 'storm_time' THEN substorm_phase = [1]
  IF substorm_phase_name EQ 'nonstorm_time' THEN substorm_phase = [0]
  IF substorm_phase_name EQ 'all' THEN substorm_phase = [0,1]

; flag_phase = FLOAT(flag_phase)
;  index = WHERE(flag_phase EQ 0, ct)
;  IF ct GT 0 THEN flag_phase(index) = !VALUES.F_NAN
  flag_phase = data.time
  flag_phase(*) = 1 
  RETURN, flag_phase
END 

;------------------------------------------------------
; Purpose: load region flag for the input region
; Inputs:  data, region
; Written by Jing Liao
; Written on 05/10/2021
;------------------------------------------------------
FUNCTION load_region_flag, data, region

  CASE region OF
     'all'      : flag_region = data.region LT 10
     'Lobe'     : flag_region = (data.mlt GT 16 OR data.mlt LT 8) AND (data.region EQ 1)
     'BL'       : flag_region = (data.mlt GT 16 OR data.mlt LT 8) AND (data.region EQ 2)
     'PS'       : flag_region = (data.mlt GT 16 OR data.mlt LT 8) AND (data.region EQ 3)
     'Dayside'  : flag_region = (data.mlt GE 8) and (data.mlt LE 16) AND data.region LT 10    
     'Tail'     : flag_region = (data.mlt GT 16) OR (data.mlt LT 8) AND data.region LT 10
  ENDCASE
  
  flag_region = FLOAT(flag_region)

  index = where(flag_region EQ 0, ct)
  if ct gt 0 then flag_region[index]= !VALUES.F_NAN
  
  RETURN,flag_region 
END  

;----------------------------------------------------------------
; Purpose: load all condition flags for o + beam mapping routine
; Inputs:
; Written by Jing Liao
; Written on 05/12/21
;---------------------------------------------------------------
PRO load_external_condition_flags, data, storm_phase, substorm_phase, region, flag_ext_condition

  flag_storm = load_storm_phase_flag(data, storm_phase)
  flag_substorm = load_substorm_phase_flag(data, substorm_phase)
  flag_region = load_region_flag(data, region)
  
  flag_ext_condition = flag_storm * flag_substorm *  flag_region
END
