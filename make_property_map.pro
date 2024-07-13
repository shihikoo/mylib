;----------------------------------------------
; Purpose: Calculate property value in axis for map
; Inputs:
; Written by Jing Liao
; Written on 05/12/2021
;----------------------------------------------------
PRO calculate_property_map, property_para, property_anti, data_pos, x_range, y_range, z_range, x_log, y_log, grid_x, grid_y, grid_z, flag_para, flag_anti, property_value, x_axis, y_axis, z_axis, x_cuttings, y_cuttings, z_cuttings, slice_mlt = slice_mlt

  ntime = N_ELEMENTS(property_para)
  
 IF x_log EQ 1 THEN BEGIN 
    grid_x = grid_x/2.
     nx = CEIL(ABS(alog10(x_range[1]) - alog10(x_range[0]))/grid_x) 
     x_axis = 10^(INDGEN(nx)*grid_x + grid_x*0.5 + alog10(x_range[0] < x_range[1]))
     x_cuttings = [[x_axis*10^(-0.5*grid_x)], [x_axis*10^(0.5*grid_x)]]
  ENDIF ELSE BEGIN
     nx = CEIL(ABS(x_range[1] - x_range[0])/grid_x)
     x_axis = INDGEN(nx)*grid_x + (x_range[0] < x_range[1]) + grid_x*0.5
     x_cuttings = [[x_axis-0.5*grid_x], [x_axis+0.5*grid_x]]
  ENDELSE

  IF y_log EQ 1 THEN BEGIN
     grid_y = grid_y/2.
     ny = CEIL(ABS(alog10(y_range[1]) - alog10(y_range[0]))/grid_y)
     y_axis = 10^(INDGEN(ny)*grid_y + grid_y*0.5 + alog10(y_range[0] < y_range[1]))
     y_cuttings = [[y_axis*10^(-0.5*grid_y)], [y_axis*10^(0.5*grid_y)]]
  ENDIF ELSE BEGIN
     ny = CEIL(ABS(y_range[1] - y_range[0])/grid_y)
     y_axis = INDGEN(ny)*grid_y + (y_range[0] < y_range[1]) + grid_y*0.5
     y_cuttings = [[y_axis-0.5*grid_y], [y_axis+0.5*grid_y]]
  ENDELSE 

  IF KEYWORD_SET(slice_mlt) THEN BEGIN 
     z_axis = [0, 4, 8, 12, 16, 20]
     z_cuttings = [[22,2,6,10,14,18],[2,6,10,14,18,22]]
     nz = N_ELEMENTS(z_axis)
  ENDIF ELSE BEGIN
     nz = CEIL(ABS(z_range[1] - z_range[0])/grid_z) 
; IF z_log EQ 1 THEN z_axis = 10^(INDGEN(nz)*grid_z + grid_z*0.5 + z_range[0] < z_range[1]) ELSE
     z_axis = INDGEN(nz)*grid_z + (z_range[0] < z_range[1]) + grid_z*0.5
     z_cuttings = [[z_axis-0.5*grid_z], [z_axis+0.5*grid_z]]
;  IF z_log EQ 1 THEN nz = CEIL(ABS(alog10(z_range[1]) - alog10(z_range[0]))/grid_z) ELSE 
  ENDELSE 

  property_value = REPLICATE(!VALUES.F_NAN, ntime, 2, nx, ny, nz)
  
  FOR ix = 0, n_elements(x_axis)-1 DO BEGIN
     FOR iy = 0, n_elements(y_axis)-1 DO BEGIN
        FOR iz = 0, n_elements(z_axis)-1 DO BEGIN           
           IF z_cuttings[iz,0] GT z_cuttings[iz,1] AND KEYWORD_SET(slice_mlt) THEN BEGIN
              index_para = where(data_pos[*,0] GE x_cuttings[ix,0] AND data_pos[*,0] LT x_cuttings[ix,1] AND $
                                 data_pos[*,1] GE y_cuttings[iy,0] AND data_pos[*,1] LT y_cuttings[iy,1] AND $
                                 ((data_pos[*,2] GE z_cuttings[iz,0] AND data_pos[*,2] LT 24.) $
                                  OR  (data_pos[*,2] GE 0 AND data_pos[*,2] LT z_cuttings[iz,1])) AND $
                                 ABS(flag_para) ge 1, ct_para)  ;FINITE(flag_para), ct_para)
           ENDIF ELSE BEGIN
              index_para = where(data_pos[*,0] GE x_cuttings[ix,0] AND data_pos[*,0] LT x_cuttings[ix,1] AND $
                                 data_pos[*,1] GE y_cuttings[iy,0] AND data_pos[*,1] LT y_cuttings[iy,1] AND $
                                 data_pos[*,2] GE z_cuttings[iz,0] AND data_pos[*,2] LT z_cuttings[iz,1] AND $
                                 ABS(flag_para) ge 1, ct_para) ;FINITE(flag_para), ct_para)
           ENDELSE           
           
           IF ct_para gt 1 THEN property_value[index_para,0,ix,iy,iz] = property_para[index_para]
           
           IF z_cuttings[iz,0] GT z_cuttings[iz,1] AND KEYWORD_SET(slice_mlt) THEN BEGIN 
              index_anti = where(data_pos[*,0] GE x_cuttings[ix,0] AND data_pos[*,0] LT x_cuttings[ix,1] AND $
                                 data_pos[*,1] GE y_cuttings[iy,0] AND data_pos[*,1] LT y_cuttings[iy,1] AND $
                                 ((data_pos[*,2] GE z_cuttings[iz,0] AND data_pos[*,2] LT 24.) $
                                  OR  (data_pos[*,2] GE 0 AND data_pos[*,2] LT z_cuttings[iz,1])) AND $
                                 ABS(flag_anti) ge 1, ct_anti)  ;FINITE(flag_anti), ct_anti)
           ENDIF ELSE BEGIN
              index_anti = where(data_pos[*,0] GE x_cuttings[ix,0] AND data_pos[*,0] LT x_cuttings[ix,1] AND $
                                 data_pos[*,1] GE y_cuttings[iy,0] AND data_pos[*,1] LT y_cuttings[iy,1] AND $
                                 data_pos[*,2] GE z_cuttings[iz,0] AND data_pos[*,2] LT z_cuttings[iz,1] AND $
                                 ABS(flag_anti) ge 1, ct_anti)  ;FINITE(flag_anti), ct_anti)
           ENDELSE 
           IF ct_anti gt 1 THEN property_value[index_anti,1,ix,iy,iz] = property_anti[index_anti]
        ENDFOR
     ENDFOR
  ENDFOR

END

;------------
;Purpose: calculate quartile output depending on requested value
;Inputs: 
; -----------------
FUNCTION calcuate_quartile, input_array, output_type
   index = where(FINITE(input_array),ct)
   if ct eq 0 then return, !values.f_nan
   input_array = input_array[index]
   sortedData = input_array[Sort(input_array)]
   IF N_Elements(sortedData) MOD 2 EQ 0 THEN BEGIN
      index = N_Elements(sortedData)/2
      medianData = (sortedData[index-1] + sortedData[index]) / 2.0
      lowerGroup = sortedData[0:index-1]
      higherGroup = sortedData[index:N_Elements(data)-1]
   ENDIF ELSE BEGIN
      index = N_Elements(sortedData)/2
      medianData = sortedData[index]
      lowerGroup = sortedData[0:index-1]
      higherGroup = sortedData[index+1:N_Elements(data)-1]
   ENDELSE
   
   if output_type eq "q1" then return, Median(lowerGroup, /EVEN)
   if output_type eq "q3" then return, Median(higherGroup, /EVEN)
   if output_type eq "iqr" then return, Median(higherGroup, /EVEN) - Median(lowerGroup, /EVEN)

  return, Median(sortedData, /EVEN)
end 

;----------------------------------------------------------
; Purpose: calculate property map based on type required
; Inputs: property_value, property_map_type
;------------------------------------------------------------
FUNCTION aggregate_property_value, property_value, property_map_type, threshold = threshold, total_counts = total_counts
  IF size(property_value, /n_dim) NE 4 AND size(property_value, /n_dim) NE 5 THEN stop
  IF size(property_value, /n_dim) EQ 5 THEN BEGIN 
     nx = n_elements(property_value[0, 0, *, 0, 0])
     ny = n_elements(property_value[0, 0, 0, *, 0])
  ENDIF ELSE BEGIN 
     nx = n_elements(property_value[0, 0, *, 0])
     ny = n_elements(property_value[0, 0, 0, *])
  ENDELSE
  property_result = REPLICATE(!VALUES.F_NAN, nx, ny)
  
;-------- mean
  IF property_map_type EQ 'mean' THEN BEGIN
     IF size(property_value, /n_dim) EQ 5  THEN BEGIN
        property_result = TOTAL(TOTAL(TOTAL(property_value, 2, /NAN), 1, /NAN), 3, /NAN)/ $
                          TOTAL(TOTAL(TOTAL(ABS(property_value) GE 0, 2, /NAN), 1, /NAN), 3, /NAN)
     ENDIF ELSE BEGIN
        property_result = TOTAL(TOTAL(property_value, 2, /NAN), 1, /NAN)/ $
                          TOTAL(TOTAL(ABS(property_value) GE 0, 2, /NAN), 1, /NAN)
     ENDELSE 
  ENDIF 
  
;-------- median
  IF property_map_type EQ 'median' THEN BEGIN
     FOR ix = 0, nx-1 DO BEGIN
        FOR iy = 0, ny-1 DO BEGIN 
           IF size(property_value, /n_dim) EQ 5 THEN dummy = property_value[*, *, ix, iy, *] ELSE  dummy = property_value[*, *, ix, iy]
           index = where(finite(dummy), ct)
           IF ct GT 0 THEN dummy = dummy[index]
           property_result[ix, iy] = MEDIAN(dummy,/even)
        ENDFOR
     ENDFOR
  ENDIF 

;-------- minimum
  IF property_map_type EQ 'minimum' THEN BEGIN
     FOR ix = 0, nx-1 DO BEGIN
        FOR iy = 0, ny-1 DO BEGIN 
           IF size(property_value, /n_dim) EQ 5 THEN dummy = property_value[*, *, ix, iy, *] ELSE  dummy = property_value[*, *, ix, iy]
           property_result[ix, iy] = MIN(dummy,/NAN)
        ENDFOR
     ENDFOR
  ENDIF 

;-------- maximum
  IF property_map_type EQ 'maximum' THEN BEGIN
     FOR ix = 0, nx-1 DO BEGIN
        FOR iy = 0, ny-1 DO BEGIN 
           IF size(property_value, /n_dim) EQ 5 THEN dummy = property_value[*, *, ix, iy, *] ELSE  dummy = property_value[*, *, ix, iy]
           property_result[ix, iy] = MAX(dummy,/NAN)
        ENDFOR
     ENDFOR
  ENDIF
 
;-------- difference
IF property_map_type EQ 'difference' THEN BEGIN
   FOR ix = 0, nx-1 DO BEGIN
      FOR iy = 0, ny-1 DO BEGIN 
         IF size(property_value, /n_dim) EQ 5 THEN dummy = property_value[*, *, ix, iy, *] ELSE  dummy = property_value[*, *, ix, iy]
         property_result[ix, iy] = ABS(MAX(dummy,/NAN) - MIN(dummy,/NAN))
      ENDFOR
   ENDFOR
ENDIF

;-------- standard deviation
IF property_map_type EQ 'sd' THEN BEGIN
   FOR ix = 0, nx-1 DO BEGIN
      FOR iy = 0, ny-1 DO BEGIN 
         IF size(property_value, /n_dim) EQ 5 THEN dummy = property_value[*, *, ix, iy, *] ELSE  dummy = property_value[*, *, ix, iy]
         property_result[ix, iy] = STDDEV(dummy,/NAN)
      ENDFOR
   ENDFOR
ENDIF

;-------- upper quartile
IF property_map_type EQ 'q3' THEN BEGIN
   FOR ix = 0, nx-1 DO BEGIN
      FOR iy = 0, ny-1 DO BEGIN 
         IF size(property_value, /n_dim) EQ 5 THEN dummy = property_value[*, *, ix, iy, *] ELSE  dummy = property_value[*, *, ix, iy]
         property_result[ix, iy] = calcuate_quartile(dummy, "q3")
      ENDFOR
   ENDFOR
ENDIF

;-------- lower quartile
IF property_map_type EQ 'q1' THEN BEGIN
   FOR ix = 0, nx-1 DO BEGIN
      FOR iy = 0, ny-1 DO BEGIN 
         IF size(property_value, /n_dim) EQ 5 THEN dummy = property_value[*, *, ix, iy, *] ELSE  dummy = property_value[*, *, ix, iy]
         property_result[ix, iy] = calcuate_quartile(dummy, "q1")
      ENDFOR
   ENDFOR
ENDIF

;-------- lower quartile
IF property_map_type EQ 'iqr' THEN BEGIN
   FOR ix = 0, nx-1 DO BEGIN
      FOR iy = 0, ny-1 DO BEGIN 
         IF size(property_value, /n_dim) EQ 5 THEN dummy = property_value[*, *, ix, iy, *] ELSE  dummy = property_value[*, *, ix, iy]
         property_result[ix, iy] = calcuate_quartile(dummy, "iqr")
      ENDFOR
   ENDFOR
ENDIF

  index = where(total_counts le threshold,ct)
  if ct gt 0 then property_result[index] = !VALUES.F_NAN

  return, property_result
END 

;----------------------------------------------------
; Purpose: make 2d property map
; Inputs: property_value, property_map_type, property_name, filepath, ts_date, te_date, plot_axis, x_axis, y_axis, x_range, y_range,filename,  property_v_log, property_v_range, property_unit
; Keywords:ps_plot
;----------------------------------------------------
PRO make_2d_property_map, property_value, property_map_type, property_name, filepath, ext_condition_str, int_condition_str, ts_date, te_date, plot_axis, x_axis, y_axis, x_range, y_range, xlog, ylog, filename,  property_v_log, property_v_range, property_unit, ps_plot = ps_plot, threshold = threshold, total_counts = total_counts

; filepath
  path_pp_2d = filepath+'2d/'

; calculation
  property_map_2d = aggregate_property_value( property_value, property_map_type, threshold = threshold, total_counts = TOTAL(total_counts,3,/nan))

; draw heat map
  filename = path_pp_2d+ ext_condition_str + '_' + int_condition_str  +'_'+property_map_type+'_'+property_name + '_'  + '_'+ ts_date+'_to_' + te_date+'_' + PLOT_AXIS[0]+'_vs_'+PLOT_AXIS[1] +'.ps'
  title = ext_condition_str+'!C'+int_condition_str + ' O!U+!N beam!C' +property_map_type +' ' +PROPERTY_name +'!Cfrom ' + ts_date+' TO ' +te_date
  make_heat_map, x_axis, y_axis, property_map_2d, filename, title, plot_axis, unit = property_unit, xrange = x_range, yrange = y_range, zrange= property_v_range, xlog = xlog, ylog = ylog, zlog = property_v_log, ps_plot = ps_plot
  
; write ratio into a csv file     
   fln_data = path_pp_2d+ ext_condition_str + '_' + int_condition_str  +'_'+property_map_type+'_'+property_name + '_'  + '_'+ ts_date+'_to_' + te_date+'_' + PLOT_AXIS[0]+'_vs_'+PLOT_AXIS[1] +'.csv'
         
   data_csv = [reform(y_axis,1,n_elements(y_axis)), property_map_2d]

   header = strarr(n_elements(x_axis)+1)

   for i = 0, n_elements(x_axis)-1 do header[i+1] = string(x_axis[i])

   write_csv, fln_data, data_csv, header = header  

END


PRO make_slice_property_map, property_value, property_map_type, property_name, filepath, ext_condition_str, int_condition_str, ts_date, te_date, plot_axis, x_axis, y_axis, z_axis, z_cuttings, x_range, y_range, xlog, ylog, slice_grid,filename,  property_v_log, property_v_range, property_unit, ps_plot = ps_plot, slice_mlt = slice_mlt, threshold = threshold, total_counts = total_counts
  
  nz = N_ELEMENTS(z_axis)
  
  slice_grid_str = STRING(slice_grid, format = '(i2.2)')
  path_pp_slice = filepath +'slice/slice_zgrid_'+slice_grid_str+'/'

  FOR iz = 0, nz-1 DO BEGIN 
; calculation
     property_map_slice = aggregate_property_value(property_value[*, *, *, *, iz], property_map_type, threshold = threshold, total_counts = total_counts[*,*,iz])

; draw the plot
     slice_block = [strcompress(STRING(z_cuttings[iz,0], format = '(f5.1)'),/remove_all), $
                    strcompress(STRING(z_cuttings[iz,1], format = '(f5.1)'),/remove_all)]

     filename = path_pp_slice+ ext_condition_str + '_' + int_condition_str  +'_'+property_map_type+'_' + property_name+'_'+'_' + ts_date+'_to_' + te_date+'_' + PLOT_AXIS[0]+'_vs_'+PLOT_AXIS[1] +'_at_' + plot_axis[2]+'_' +slice_block[0]+'_'+slice_block[1] +'.ps'
     title = ext_condition_str+'!C'+int_condition_str +' O!U+!N beam!C' +property_map_type +' '+ PROPERTY_name+' at '+ plot_axis[2] + ':[ ' +slice_block[0]+','  +slice_block[1]+' ] '+ '!CFROM ' + ts_date  +' TO ' +te_date
     
     make_heat_map, x_axis, y_axis, property_map_slice, filename, title, plot_axis, unit = property_unit, xrange = x_range, yrange = y_range, zrange= property_v_range, xlog = xlog, ylog = ylog, zlog = property_v_log, ps_plot = ps_plot

; write ratio into a csv file     
      fln_data = path_pp_slice+ ext_condition_str + '_' + int_condition_str  +'_'+property_map_type+'_' + property_name+'_'+'_' + ts_date+'_to_' + te_date+'_' + PLOT_AXIS[0]+'_vs_'+PLOT_AXIS[1] +'_at_' + plot_axis[2]+'_' +slice_block[0]+'_'+slice_block[1] + '.csv'
               
      data_csv = [reform(y_axis,1,n_elements(y_axis)), property_map_slice]

      header = strarr(n_elements(x_axis)+1)

      for i = 0, n_elements(x_axis)-1 do header[i+1] = string(x_axis[i])

      write_csv, fln_data, data_csv, header = header  

  ENDFOR     
END

;------------------------------------------------------------------------------
; Purpose: make property map
; Inputs: data, data_pos, property_name, property_map_type, flag, filepath, ts_date, te_date, plot_axis,  x_range, y_range, z_range, grid, slice_grid, filename, plot_2d, plot_slice, make_table
; Keywords: ps_plot 
;------------------------------------------------------------------------------
PRO make_property_map, data, data_pos, property_name, property_map_type, flag_para, flag_anti, filepath, ts_date, te_date, plot_axis, ext_condition_str, int_condition_str, range, log, grid, slice_grid, filename, plot_2d, plot_slice, make_table, ps_plot = ps_plot, threshold = threshold, total_counts = total_counts
  
  X_RANGE = range[*, 0]
  Y_RANGE = range[*, 1]
  Z_RANGE = range[*, 2]
;   r_range=ABS(y_range[0]-y_RANGE[1])
  xlog = log[0]
  ylog = log[1]

; load property data and property graphing settings
  load_property_data, data, property_name, property_para, property_anti, property_v_log, property_v_range, property_unit, property_map_type = property_map_type
  
; reset grid according to plot_axis
  IF PLOT_AXIS[0] EQ 'MLT' THEN BEGIN
     grid_x = 0.5*grid & grid_y = 1.25*grid
  endif  else begin
     grid_x = grid & grid_y = grid
  endelse 
  grid_z = slice_grid
  
  IF PLOT_AXIS[2] EQ 'MLT' THEN BEGIN
     slice_mlt = 1
  ENDIF 

; Calculation
  calculate_property_map, property_para, property_anti, data_pos, x_range, y_range, z_range, xlog,ylog, grid_x, grid_y, grid_z, flag_para, flag_anti, property_value, x_axis, y_axis, z_axis, x_cuttings, y_cuttings, z_cuttings, slice_mlt = slice_mlt

; Draw 2d maps
  IF KEYWORD_SET(PLOT_2D) THEN make_2d_property_map, property_value, property_map_type,property_name,filepath, ext_condition_str, int_condition_str, ts_date, te_date, plot_axis, x_axis, y_axis, x_range, y_range, xlog, ylog, filename, property_v_log, property_v_range, property_unit, ps_plot=ps_plot, threshold = threshold, total_counts = total_counts

; Draw slice maps
  IF KEYWORD_SET(PLOT_SLICE) THEN make_slice_property_map, property_value, property_map_type, property_name, filepath, ext_condition_str, int_condition_str, ts_date, te_date, plot_axis, x_axis, y_axis, z_axis, z_cuttings, x_range, y_range, xlog, ylog, slice_grid,filename,  property_v_log, property_v_range, property_unit, ps_plot=ps_plot, slice_mlt = slice_mlt, threshold = threshold, total_counts = total_counts
  
; make historgram 
  IF KEYWORD_SET(make_table) THEN  stop

END


