;-------------------------------------------------------------------
; Purpose: Adding Storm phase information. 
; Description: Read storm phase from file and deceide the storm phase
; for each data
; Inputs: time_avg - the array of time to check for the phase
; Keywords: storm_phase_filename - the csv file of storm phases
;         : storm_phase_tplot_name - save the result into a tplot variable
;
; Written by Jing Liao
; Written on 04/12/2021
;-------------------------------------------------------------------

PRO find_storm_phase, time_avg, storm_phase_filename = storm_phase_filename, storm_phase_tplot_name = storm_phase_tplot_name
  IF ~KEYWORD_SET(storm_phase_filename) THEN storm_phase_filename = 'data/storm_phase_2016_2017.csv'
  IF ~KEYWORD_SET(storm_phase_tplot_name) THEN storm_phase_tplot_name = 'storm_phase'

  storm_phase_data = READ_CSV(storm_phase_filename, HEADER = storm_phase_header)

  prestorm_start = time_double(storm_phase_data.FIELD1)
  storm_onset = time_double( storm_phase_data.FIELD2)
  min_dst_time = time_double(storm_phase_data.FIELD3)
  recovery_fast_end = time_double(storm_phase_data.FIELD4)
  recovery_early_end = time_double(storm_phase_data.FIELD5)
  recovery_long_end = time_double(storm_phase_data.FIELD6)
  min_dst = time_double(storm_phase_data.FIELD7)

  nstorm = N_ELEMENTS(min_dst)
  n_avg = N_ELEMENTS(time_avg)
  storm_phase = INTARR(n_avg)

  FOR  itime = 0, n_avg-1 DO BEGIN
     FOR istorm = 0, nstorm-1 DO BEGIN 
     
     IF time_avg(itime) LT prestorm_start(istorm) OR time_avg(itime) GT recovery_long_end(istorm) THEN CONTINUE
        belong = 0
        IF time_avg(itime) GE prestorm_start(istorm) AND $
           time_avg(itime) LT storm_onset(istorm) THEN BEGIN 
           storm_phase(itime) = 1 ;initial phase
           belong = belong+1
        ENDIF 
        IF time_avg(itime) GE storm_onset(istorm) AND $
           time_avg(itime)  LT min_dst_time(istorm) THEN BEGIN 
           storm_phase(itime) = 2 ; mian phase
           belong = belong+1
        ENDIF 
        IF time_avg(itime) GE min_dst_time(istorm) AND $
           time_avg(itime) LT recovery_early_end(istorm) THEN BEGIN 
           storm_phase(itime) = 3 ; recovery phase
           belong = belong+1
        ENDIF
        IF time_avg(itime) GE recovery_early_end(istorm) AND $
           time_avg(itime) LT recovery_long_end(istorm) THEN BEGIN 
           storm_phase(itime) = 4 ; later recovery phase (not used for any map)
           belong = belong+1
        ENDIF

        IF belong GT 1 THEN stop
        if belong eq 0 then begin 
           IF time_avg(itime) GE (prestorm_start(istorm)-60.*60.*4) AND $
              time_avg(itime) LT prestorm_start(istorm) THEN BEGIN 
              storm_phase(itime) = 5 ; pre storm
              belong = belong+1
           endif 
        endif  
     ENDFOR 
  ENDFOR  
  store_data, storm_phase_tplot_name, data = {x:time_avg, y:storm_phase}
  ylim, storm_phase_tplot_name, -1, 6

END 
