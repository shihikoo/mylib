;--------------------------------------------------------------------------
; Purpose: calculate sample counts, event counts and event ratio for map
; Inputs: x_range, y_range, z_range, grid_x, grid_y, grid_z, total_counts,event_counts,event_ratio1
;--------------------------------------------------------------------------
PRO calculate_ratio_for_map, data_pos, x_range, y_range, z_range, x_log, y_log, grid_x, grid_y, grid_z, flag_para, flag_anti, total_counts, event_counts, event_ratio, x_axis, y_axis, z_axis, x_cuttings, y_cuttings, z_cuttings, slice_mlt = slice_mlt
 
   COMMON SHARE1,ENERGY_BINS, DENERGY_BINS, PA_BINS, ERROR_MESSAGE, RATIO_CORRECTION
 
  IF x_log EQ 1 THEN BEGIN 
     grid_x = grid_x/2.
     nx = CEIL(ABS(alog10(x_range(1)) - alog10(x_range(0)))/grid_x) 
     x_axis = 10^(INDGEN(nx)*grid_x + grid_x*0.5 + alog10(x_range(0) < x_range(1)))
     x_cuttings = [[x_axis*10^(-0.5*grid_x)], [x_axis*10^(0.5*grid_x)]]
  ENDIF ELSE BEGIN
     nx = CEIL(ABS(x_range(1) - x_range(0))/grid_x)
     x_axis = INDGEN(nx)*grid_x + (x_range(0) < x_range(1)) + grid_x*0.5
     x_cuttings = [[x_axis-0.5*grid_x], [x_axis+0.5*grid_x]]
  ENDELSE

  IF y_log EQ 1 THEN BEGIN
     grid_y = grid_y/2.
     ny = CEIL(ABS(alog10(y_range(1)) - alog10(y_range(0)))/grid_y)
     y_axis = 10^(INDGEN(ny)*grid_y + grid_y*0.5 + alog10(y_range(0) < y_range(1)))
     y_cuttings = [[y_axis*10^(-0.5*grid_y)], [y_axis*10^(0.5*grid_y)]]
  ENDIF ELSE BEGIN
     ny = CEIL(ABS(y_range(1) - y_range(0))/grid_y)
     y_axis = INDGEN(ny)*grid_y + (y_range(0) < y_range(1)) + grid_y*0.5
     y_cuttings = [[y_axis-0.5*grid_y], [y_axis+0.5*grid_y]]
  ENDELSE 

  IF KEYWORD_SET(slice_mlt) THEN BEGIN 
     z_axis = [0, 4, 8, 12, 16, 20]
     z_cuttings = [[22,2,6,10,14,18],[2,6,10,14,18,22]]
     nz = N_ELEMENTS(z_axis)
  ENDIF ELSE BEGIN
     nz = CEIL(ABS(z_range(1) - z_range(0))/grid_z) 
; IF z_log EQ 1 THEN z_axis = 10^(INDGEN(nz)*grid_z + grid_z*0.5 + z_range(0) < z_range(1)) ELSE
     z_axis = INDGEN(nz)*grid_z + (z_range(0) < z_range(1)) + grid_z*0.5
     z_cuttings = [[z_axis-0.5*grid_z], [z_axis+0.5*grid_z]]
;  IF z_log EQ 1 THEN nz = CEIL(ABS(alog10(z_range(1)) - alog10(z_range(0)))/grid_z) ELSE 
  ENDELSE 

  event_counts = fltarr(nx, ny, nz)
  total_counts = fltarr(nx, ny, nz)
  
  FOR ix = 0, n_elements(x_axis)-1 DO BEGIN
      FOR iy = 0, n_elements(y_axis)-1 DO BEGIN
         FOR iz = 0, n_elements(z_axis)-1 DO BEGIN
            IF z_cuttings[iz,0] GT z_cuttings[iz,1] AND KEYWORD_SET(slice_mlt) THEN BEGIN 
               index1 = where(data_pos(*,0) GE x_cuttings[ix,0] AND data_pos(*,0) LT x_cuttings[ix,1] AND $
                              data_pos(*,1) GE y_cuttings[iy,0] AND data_pos(*,1) LT y_cuttings[iy,1] AND $
                              ((data_pos(*,2) GE z_cuttings[iz,0] AND data_pos(*,2) LT 24.)  OR  (data_pos(*,2) GE 0 AND data_pos(*,2) LT z_cuttings[iz,1])) AND $
                              (FINITE(flag_para) OR FINITE(flag_anti)), ct1)

            ENDIF ELSE BEGIN
               index1 = where(data_pos(*,0) GE x_cuttings[ix,0] AND data_pos(*,0) LT x_cuttings[ix,1] AND $
                              data_pos(*,1) GE y_cuttings[iy,0] AND data_pos(*,1) LT y_cuttings[iy,1] AND $
                              data_pos(*,2) GE z_cuttings[iz,0] AND data_pos(*,2) LT z_cuttings[iz,1] AND $
                              (FINITE(flag_para) OR FINITE(flag_anti)), ct1)
            ENDELSE             

            total_counts(ix,iy,iz) = ct1
            
            IF ct1 GT 0  THEN BEGIN
               index2 = where(ABS(flag_para(index1)) GE 1 OR ABS(flag_anti(index1)) GE 1, ct2)   
               event_counts(ix,iy,iz) = ct2
            ENDIF
         ENDFOR
      ENDFOR
  ENDFOR 

  event_ratio = event_counts/total_counts
  
  index = where(total_counts eq 0, ct)
  if ct gt 0 then event_ratio[index] = !values.f_nan
 
END

pro apply_correction,x, ratio, x_low,x_high, x_corrected
  lowest_edge = 0
  high_edge = 1
  
  lower_edge = min(x_low)
  nbins = n_elements(x_low)
  x_center = (x_low + x_high)/2
  x_center = [x_center,1]
  ratio = [ratio,1]

  size_x = size(x)
  
  x_corrected = dblarr(size_x[1], size_x[2])
  correction = dblarr(size_x[1], size_x[2])

  k = dblarr(nbins)
  m = dblarr(nbins)

  for i = 0, nbins-1 do begin
     index = where(x ge x_center[i] and x le x_center[i+1], ct)
     if ct gt 0 then begin
        k[i] = (ratio[i+1] - ratio[i])/(x_center[i+1] - x_center[i])
        m[i] = (x_center[i]*ratio[i+1] - x_center[i+1]*ratio[i])/(x_center[i]-x_center[i+1])
        correction[index] = x[index]*k[i]+m[i]
        x_corrected[index] = x[index]*correction[index]

        index = where(x lt x_center[0], ct)
        if ct gt 0 then begin
           correction[index] = x[index]*k[0] + m[0]
           x_corrected[index] = x[index] * correction[index]
           
        endif 
     endif 
  endfor 

  index = where(~finite(x),ct)
  if ct gt 0 then x_corrected[index] = !values.f_nan
  
end 
;----------------------------------------------------------------------------
; Purpose: make 2d heat map of number of samples, events and ratio
; Inputs:  total_counts, event_counts, event_ratio, filepath, ts_date,
; te_date, plot_axis,  x_axis, y_axis, x_range, y_range,filename,
; events_v_log, events_v_range, events_unit
; , samples_v_log, samples_v_range,samples_unit, ratio_v_log, ratio_v_range, ratio_unit
; 
;---------------------------------------------------------------------------------------------------------
PRO make_2d_heat_map, total_counts, event_counts, event_ratio, filepath,ext_condition_str, int_condition_str , ts_date, te_date, plot_axis, x_axis, y_axis, x_range, y_range, xlog, ylog, filename, events_v_log, events_v_range, events_unit, samples_v_log, samples_v_range,samples_unit, ratio_v_log, ratio_v_range, ratio_unit, ps_plot = ps_plot, threshold = threshold, region= region, ratio_correction = ratio_correction
  
;--- filepath ----
  path_2d = filepath+'2d/'
  
  total_counts_2d = TOTAL(total_counts, 3, /nan)
  index=where(total_counts_2d eq 0,ct)
  if ct gt 0 then  total_counts_2d[index] =  !VALUES.F_NAN 
  
  event_counts_2d =  TOTAL(event_counts, 3, /nan)
  index=where(event_counts_2d eq 0,ct)
  if ct gt 0 then  event_counts_2d[index] =  !VALUES.F_NAN 

  event_ratio_2d = TOTAL(event_counts, 3, /nan)/TOTAL(total_counts, 3, /nan)
  if keyword_set(ratio_correction) then begin
     ratio_correction, event_ratio_2d, region, ratio_corrected
     event_ratio_2d = ratio_corrected
  endif
  
  if keyword_set(threshold) then begin
     index = where(total_counts_2d le threshold, ct)
     if ct gt 0 then begin 
        total_counts_2d[index] = !VALUES.F_NAN
        event_ratio_2d[index] = !VALUES.F_NAN
        event_counts_2d[index] = !VALUES.F_NAN
     endif 
  endif

;-- draw heat map for samples
  filename = path_2d + ext_condition_str +'_'+ int_condition_str +'_samples_' + ts_date+'_to_' + te_date + '_' + PLOT_AXIS[0]+ '_vs_'+PLOT_AXIS[1]+'.ps'
  title = ext_condition_str+'!C'+int_condition_str +'Samples!Cfrom ' + ts_date+' to ' +te_date
  make_heat_map, x_axis, y_axis, total_counts_2d, filename, title, plot_axis, unit = samples_unit, xrange = x_range, yrange = y_range, zrange= samples_v_range, xlog = xlog, ylog =ylog, zlog = samples_v_log, ps_plot = ps_plot
  
;-- draw heat map for events                                              
  filename = path_2d + ext_condition_str +'_' + int_condition_str + '_events_' + ts_date+'_to_' + te_date+'_' + PLOT_AXIS[0]+ '_vs_'+PLOT_AXIS[1]+'.ps'
  title =ext_condition_str+'!C'+int_condition_str + ' O!U+!N Beam Events!Cfrom ' + ts_date+' to ' +te_date
  make_heat_map, x_axis, y_axis, event_counts_2d, filename, title, plot_axis, unit = events_unit, xrange = x_range, yrange = y_range, zrange = events_v_range, xlog=xlog, ylog = ylog, zlog = events_v_log, ps_plot = ps_plot
  
;-- draw heat map for event ratio  
  filename = path_2d + ext_condition_str + '_' + int_condition_str + '_ratio_' + ts_date+'_to_' + te_date+'_' + PLOT_AXIS[0]+ '_vs_'+PLOT_AXIS[1]+'.ps'
  title = ext_condition_str+'!C'+int_condition_str + ' O!U+!N Beam Ratio!Cfrom ' + ts_date+' to ' +te_date
  make_heat_map, x_axis, y_axis, event_ratio_2d, filename, title, plot_axis, unit = ratio_unit, xrange = x_range, yrange = y_range, zrange = ratio_v_range, xlog= xlog, ylog = ylog, zlog = ratio_v_log, ps_plot = ps_plot

; write ratio into a csv file     
  fln_data = path_2d + ext_condition_str + '_' + int_condition_str + '_ratio_' + ts_date+'_to_' + te_date+'_' + PLOT_AXIS[0]+ '_vs_'+PLOT_AXIS[1]+'.csv'
  data_csv = [reform(y_axis,1,n_elements(y_axis)) ,event_ratio_2d]       

  header = strarr(n_elements(x_axis)+1)
  for i = 0, n_elements(x_axis)-1 do header[i+1] = string(x_axis[i])
  
  write_csv, fln_data, data_csv, header = header
END

;------------------------------------------------------------------------------------------------------------------
; Purpose: make sliced heat map for samples, events and ratios for
; different z range
; Inputs: total_counts, event_counts, event_ratio, filepath 
;------------------------------------------------------------------------------------------------------------------
PRO make_slice_heat_map,  total_counts, event_counts, event_ratio, filepath,ext_condition_str, int_condition_str , ts_date, te_date, plot_axis, x_axis, y_axis, z_axis, z_cuttings, x_range, y_range, xlog, ylog, slice_grid,filename, events_v_log, events_v_range, events_unit, samples_v_log, samples_v_range,samples_unit, ratio_v_log, ratio_v_range, ratio_unit, ps_plot=ps_plot, slice_mlt = slice_mlt, threshold = threshold,region = region, ratio_correction = ratio_correction

  nz = N_ELEMENTS(z_axis)

  slice_grid_str = STRING(slice_grid, format = '(i2.2)')
  path_slice = filepath+'slice/slice_zgrid_'+slice_grid_str+'/'

  FOR iz = 0, nz-1 DO BEGIN 
; calculation
     slice_block = [strcompress(STRING(z_cuttings[iz,0], format = '(f5.1)'),/remove_all), $
                    strcompress(STRING(z_cuttings[iz,1], format = '(f5.1)'),/remove_all)]
     
     slice_total_counts = total_counts[*,*,iz]
     slice_event_counts = event_counts[*,*,iz]
     slice_event_ratio = event_ratio[*,*,iz]
     
      if keyword_set(ratio_correction) then ratio_correction, slice_event_ratio, region, ratio_corrected
      slice_event_ratio = ratio_corrected
  
     index= where(slice_event_counts eq 0, ct)
     if ct gt 0 then slice_event_counts[index] = !VALUES.F_NAN
     index= where(slice_total_counts eq 0, ct)
     if ct gt 0 then slice_total_counts[index] = !VALUES.F_NAN
     
     if keyword_set(threshold) then begin
        index = where(slice_total_counts le threshold, ct)
        if ct gt 0 then begin 
           slice_total_counts[index] = !VALUES.F_NAN
           slice_event_ratio[index] = !VALUES.F_NAN
           slice_event_counts[index] = !VALUES.F_NAN
        endif 
     endif

;  samples
     filename = path_slice + ext_condition_str + '_' + int_condition_str + '_samples_' + ts_date+'_to_' + te_date+'_' + PLOT_AXIS[0]+'_vs_' +PLOT_AXIS[1]+'_at_' + plot_axis[2]+'_' +slice_block[0]+'_'+slice_block[1] +'.ps'
     title = ext_condition_str+'!C'+int_condition_str +' O!U+!N BEAM SAMPLES' +'!Cat ' + PLOT_AXIS[2]+ ': [' +slice_block[0]+',' +slice_block[1]+']' + '!CFROM ' + ts_date+' TO '  +te_date
     make_heat_map, x_axis, y_axis, slice_total_counts, filename, title, plot_axis, unit = samples_unit, xrange = x_range, yrange = y_range, zrange= samples_v_range, xlog = xlog, ylog = ylog, zlog = samples_v_log, ps_plot = ps_plot

; events                                              
     filename = path_slice + ext_condition_str + '_' + int_condition_str + '_events_' + ts_date+'_to_' + te_date+'_' + PLOT_AXIS[0]+'_vs_' +PLOT_AXIS[1]+'_at_' + plot_axis[2]+'_' + slice_block[0]+'_'+slice_block[1] +'.ps'

     title = ext_condition_str+'!C'+int_condition_str +' O!U+!N BEAM EVENTS'+'!Cat ' + PLOT_AXIS[2]+ ': [' +slice_block[0]+',' +slice_block[1]+']' + '!CFROM ' + ts_date+' TO '  +te_date

     make_heat_map, x_axis, y_axis, slice_event_counts, filename, title, plot_axis, unit = events_unit, xrange = x_range, yrange = y_range, zrange = 
     events_v_range, xlog = xlog, ylog = ylog, zlog = events_v_log, ps_plot = ps_plot

; ratio  
     filename =  path_slice + ext_condition_str + '_' + int_condition_str + '_ratio_' + ts_date+'_to_' + te_date+'_' + PLOT_AXIS[0]+'_vs_' + PLOT_AXIS[1]+'_at_' + plot_axis[2]+'_' + slice_block[0]+'_'+slice_block[1] +'.ps'

     title = ext_condition_str+'!C'+int_condition_str+' O!U+!N BEAM RATIO'+'!Cat ' + PLOT_AXIS[2]+ ': [' +slice_block[0]+',' +slice_block[1]+']' + '!CFROM ' + ts_date +' TO ' +te_date

     make_heat_map, x_axis, y_axis, slice_event_ratio, filename, title, plot_axis , unit = ratio_unit, xrange = x_range, yrange = y_range, zrange = ratio_v_range, xlog = xlog, ylog = ylog, zlog = ratio_v_log, ps_plot = ps_plot

; write ratio into a csv file     
     fln_data = path_slice + ext_condition_str + '_' + int_condition_str + '_ratio_' + ts_date+'_to_' + te_date+'_' + PLOT_AXIS[0]+'_vs_' + PLOT_AXIS[1]+'_at_' + plot_axis[2]+'_' + slice_block[0]+'_'+slice_block[1] +'.csv'
     
     data_csv = [reform(y_axis,1,n_elements(y_axis)), slice_event_ratio]
   
     header = strarr(n_elements(x_axis)+1)

     for i = 0, n_elements(x_axis)-1 do header[i+1] = string(x_axis[i])

     write_csv, fln_data, data_csv, header = header

   ENDFOR 
      
END

PRO make_events_map, data_pos, flag_para, flag_anti, filepath, ts_date, te_date, plot_axis, ext_condition_str, int_condition_str,range, log, grid, slice_grid, filename, plot_2d, plot_slice, make_table, ps_plot = ps_plot, sample_v_range = sample_v_range,threshold = threshold,total_counts = total_counts,region = region, ratio_correction = ratio_correction,samples_v_range=samples_v_range
  X_RANGE = range[*, 0]
  Y_RANGE = range[*, 1]
  Z_RANGE = range[*, 2]
  r_range = ABS(y_range[0]-y_RANGE[1])
  xlog = log[0]
  ylog = log[1]  

; reset grid according to plot_axis
  IF PLOT_AXIS[0] EQ 'MLT' THEN BEGIN
     grid_x = 0.5*grid & grid_y = 1.5*grid
  ENDIF ELSE BEGIN
     grid_x = grid & grid_y = grid
  ENDELSE
  
  IF PLOT_AXIS[2] EQ 'MLT' THEN BEGIN
     slice_mlt = 1     
  ENDIF 
 
; Calculation
  calculate_ratio_for_map, data_pos, x_range, y_range, z_range, xlog, ylog, grid_x, grid_y, slice_grid, flag_para, flag_anti, total_counts, event_counts, event_ratio, x_axis, y_axis, z_axis, x_cuttings, y_cuttings, z_cuttings, slice_mlt = slice_mlt

 ; Graph settings
  EVENTS_V_LOG = 0 & EVENTS_V_RANGE = [1, 100.] & events_unit = '# of events'
  samples_v_log = 1 
  if ~keyword_set(sample_v_range) then samples_v_range = [1, 100.] 
  samples_unit = '# of samples'
  ratio_V_LOG = 0  & RATIO_V_RANGE = [0, 1.] & ratio_unit = 'Occurance Frequency'

; Draw 2d maps
  IF KEYWORD_SET(PLOT_2D) THEN make_2d_heat_map, total_counts, event_counts, event_ratio, filepath, ext_condition_str, int_condition_str, ts_date, te_date, plot_axis, x_axis, y_axis,  x_range, y_range, xlog, ylog, filename, events_v_log, events_v_range, events_unit, samples_v_log, samples_v_range, samples_unit, ratio_v_log, ratio_v_range, ratio_unit, ps_plot = ps_plot,threshold = threshold,region = region, ratio_correction = ratio_correction

; Draw slice maps
  IF KEYWORD_SET(PLOT_SLICE) THEN make_slice_heat_map, total_counts, event_counts, event_ratio, filepath, ext_condition_str, int_condition_str, ts_date, te_date, plot_axis, x_axis, y_axis, z_axis, z_cuttings, x_range, y_range, xlog, ylog, slice_grid,filename, events_v_log, events_v_range, events_unit, samples_v_log, samples_v_range, samples_unit, ratio_v_log, ratio_v_range, ratio_unit, ps_plot = ps_plot, slice_mlt = slice_mlt,threshold = threshold,region = region, ratio_correction = ratio_correction
  
  IF KEYWORD_SET(make_table) THEN  stop

END
