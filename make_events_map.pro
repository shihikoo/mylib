; --------------------------------------------------------------------------
; Purpose: write data into csv in x and y axis
; Inputs:  fln_data, x_axis, y_axis, data
; --------------------------------------------------------------------------
; 
pro write_map_csv, fln_data, x_axis, y_axis, data
    data_csv = [reform(y_axis, 1, n_elements(y_axis)), data]

    header = strarr(n_elements(x_axis) + 1)

    for i = 0, n_elements(x_axis) - 1 do header[i + 1] = string(x_axis[i])

    write_csv, fln_data, data_csv, header = header
end

; --------------------------------------------------------------------------
; Purpose: calculate sample counts, event counts and event ratio for map
; Inputs: x_range, y_range, z_range, grid_x, grid_y, grid_z, total_counts,event_counts,event_ratio1
; --------------------------------------------------------------------------
pro calculate_ratio_for_map, data_pos, x_range, y_range, z_range, x_log, y_log, grid_x, grid_y, grid_z, flag_para, flag_anti, total_counts, event_counts, event_ratio, x_axis, y_axis, z_axis, x_cuttings, y_cuttings, z_cuttings, slice_mlt = slice_mlt
  common SHARE1, ENERGY_BINS, DENERGY_BINS, PA_BINS, ERROR_MESSAGE, RATIO_CORRECTION

  if x_log eq 1 then begin
    grid_x = grid_x / 2.
    nx = ceil(abs(alog10(x_range[1]) - alog10(x_range[0])) / grid_x)
    x_axis = 10 ^ (indgen(nx) * grid_x + grid_x * 0.5 + alog10(x_range[0] < x_range[1]))
    x_cuttings = [[x_axis * 10 ^ (-0.5 * grid_x)], [x_axis * 10 ^ (0.5 * grid_x)]]
  endif else begin
    nx = ceil(abs(x_range[1] - x_range[0]) / grid_x)
    x_axis = indgen(nx) * grid_x + (x_range[0] < x_range[1]) + grid_x * 0.5
    x_cuttings = [[x_axis - 0.5 * grid_x], [x_axis + 0.5 * grid_x]]
  endelse

  if y_log eq 1 then begin
    grid_y = grid_y / 2.
    ny = ceil(abs(alog10(y_range[1]) - alog10(y_range[0])) / grid_y)
    y_axis = 10 ^ (indgen(ny) * grid_y + grid_y * 0.5 + alog10(y_range[0] < y_range[1]))
    y_cuttings = [[y_axis * 10 ^ (-0.5 * grid_y)], [y_axis * 10 ^ (0.5 * grid_y)]]
  endif else begin
    ny = ceil(abs(y_range[1] - y_range[0]) / grid_y)
    y_axis = indgen(ny) * grid_y + (y_range[0] < y_range[1]) + grid_y * 0.5
    y_cuttings = [[y_axis - 0.5 * grid_y], [y_axis + 0.5 * grid_y]]
  endelse
  
  if keyword_set(slice_mlt) then begin
    z_axis = [0, 4, 8, 12, 16, 20]
    z_cuttings = [[22, 2, 6, 10, 14, 18], [2, 6, 10, 14, 18, 22]]
    nz = n_elements(z_axis)
  endif else begin
    nz = ceil(abs(z_range[1] - z_range[0]) / grid_z)
    ; IF z_log EQ 1 THEN z_axis = 10^(INDGEN(nz)*grid_z + grid_z*0.5 + z_range[0] < z_range[1]) ELSE
    z_axis = indgen(nz) * grid_z + (z_range[0] < z_range[1]) + grid_z * 0.5
    z_cuttings = [[z_axis - 0.5 * grid_z], [z_axis + 0.5 * grid_z]]
    ; IF z_log EQ 1 THEN nz = CEIL(ABS(alog10(z_range[1]) - alog10(z_range[0]))/grid_z) ELSE
  endelse

  event_counts = fltarr(nx, ny, nz)
  total_counts = fltarr(nx, ny, nz)

  for ix = 0, n_elements(x_axis) - 1 do begin
    for iy = 0, n_elements(y_axis) - 1 do begin
      for iz = 0, n_elements(z_axis) - 1 do begin
        if z_cuttings[iz, 0] gt z_cuttings[iz, 1] and keyword_set(slice_mlt) then begin
          index1 = where(data_pos[*, 0]ge x_cuttings[ix, 0] and data_pos[*, 0]lt x_cuttings[ix, 1] and $
            data_pos[*, 1] ge y_cuttings[iy, 0] and data_pos[*, 1] lt y_cuttings[iy, 1] and $
            ((data_pos[*, 2] ge z_cuttings[iz, 0] and data_pos[*, 2] lt 24.) or (data_pos[*, 2] ge 0 and data_pos[*, 2] lt z_cuttings[iz, 1])) and $
            (finite(flag_para) or finite(flag_anti)), ct1)
        endif else begin
          index1 = where(data_pos[*, 0]ge x_cuttings[ix, 0] and data_pos[*, 0]lt x_cuttings[ix, 1] and $
            data_pos[*, 1] ge y_cuttings[iy, 0] and data_pos[*, 1] lt y_cuttings[iy, 1] and $
            data_pos[*, 2] ge z_cuttings[iz, 0] and data_pos[*, 2] lt z_cuttings[iz, 1] and $
            (finite(flag_para) or finite(flag_anti)), ct1)
        endelse

        total_counts[ix, iy, iz] = ct1

        if ct1 gt 0 then begin
          index2 = where(abs(flag_para[index1]) ge 1 or abs(flag_anti[index1]) ge 1, ct2)
          event_counts[ix, iy, iz] = ct2
        endif
      endfor
    endfor
  endfor

  event_ratio = event_counts / total_counts

  index = where(total_counts eq 0, ct)
  if ct gt 0 then event_ratio[index] = !values.f_nan
end

; -------------------------------------------------------------------------------------------------------
; Purpose: make 2d heat map of number of samples, events and ratio
; Inputs:  total_counts, event_counts, event_ratio, filepath, ts_date,
; te_date, plot_axis,  x_axis, y_axis, x_range, y_range,filename,
; events_v_log, events_v_range, events_unit
; , samples_v_log, samples_v_range,samples_unit, ratio_v_log, ratio_v_range, ratio_unit
;
; ---------------------------------------------------------------------------------------------------------
pro make_2d_heat_map, total_counts, event_counts, event_ratio, filepath, ext_condition_str, int_condition_str, ts_date, te_date, plot_axis, x_axis, y_axis, x_range, y_range, xlog, ylog, filename, events_v_log, events_v_range, events_unit, samples_v_log, samples_v_range, samples_unit, ratio_v_log, ratio_v_range, ratio_unit, ps_plot = ps_plot, threshold = threshold, region = region, ratio_correction = ratio_correction,remove_ps = remove_ps
  ; --- filepath ----
  path_2d = filepath + '2d/'

  total_counts_2d = total(total_counts, 3, /nan)
  index = where(total_counts_2d eq 0, ct)
  if ct gt 0 then total_counts_2d[index] = !values.f_nan

  event_counts_2d = total(event_counts, 3, /nan)
  index = where(event_counts_2d eq 0, ct)
  if ct gt 0 then event_counts_2d[index] = !values.f_nan

  event_ratio_2d = total(event_counts, 3, /nan) / total(total_counts, 3, /nan)
  if keyword_set(ratio_correction) then begin
    ratio_correction, event_ratio_2d, region, ratio_corrected
    event_ratio_2d = ratio_corrected
  endif

  if keyword_set(threshold) then begin
    index = where(total_counts_2d le threshold, ct)
    if ct gt 0 then begin
      total_counts_2d[index] = !values.f_nan
      event_ratio_2d[index] = !values.f_nan
      event_counts_2d[index] = !values.f_nan
    endif
  endif

  ; -- draw heat map for samples
  filename = path_2d + ext_condition_str + '_' + int_condition_str + '_samples_' + ts_date + '_to_' + te_date + '_' + plot_axis[0] + '_vs_' + plot_axis[1] + '.ps'
  title = ext_condition_str + '!C' + int_condition_str + ' Samples!Cfrom ' + ts_date + ' to ' + te_date

  make_heat_map, x_axis, y_axis, total_counts_2d, filename, title, plot_axis, unit = samples_unit, xrange = x_range, yrange = y_range, zrange = samples_v_range, xlog = xlog, ylog = ylog, zlog = samples_v_log, ps_plot = ps_plot,remove_ps = remove_ps

  ; -- draw heat map for events
  filename = path_2d + ext_condition_str + '_' + int_condition_str + '_events_' + ts_date + '_to_' + te_date + '_' + plot_axis[0] + '_vs_' + plot_axis[1] + '.ps'
  title = ext_condition_str + '!C' + int_condition_str + ' Events!Cfrom ' + ts_date + ' to ' + te_date

  make_heat_map, x_axis, y_axis, event_counts_2d, filename, title, plot_axis, unit = events_unit, xrange = x_range, yrange = y_range, zrange = events_v_range, xlog = xlog, ylog = ylog, zlog = events_v_log, ps_plot = ps_plot,remove_ps = remove_ps

  ; -- draw heat map for event ratio
  filename = path_2d + ext_condition_str + '_' + int_condition_str + '_ratio_' + ts_date + '_to_' + te_date + '_' + plot_axis[0] + '_vs_' + plot_axis[1] + '.ps'
  title = ext_condition_str + '!C' + int_condition_str + ' Ratio!Cfrom ' + ts_date + ' to ' + te_date

  make_heat_map, x_axis, y_axis, event_ratio_2d, filename, title, plot_axis, unit = ratio_unit, xrange = x_range, yrange = y_range, zrange = ratio_v_range, xlog = xlog, ylog = ylog, zlog = ratio_v_log, ps_plot = ps_plot,remove_ps = remove_ps

  ; write ratio into a csv file
  fln_data = path_2d + ext_condition_str + '_' + int_condition_str + '_ratio_' + ts_date + '_to_' + te_date + '_' + plot_axis[0] + '_vs_' + plot_axis[1] + '.csv'
  data_csv = [reform(y_axis, 1, n_elements(y_axis)), event_ratio_2d]

  header = strarr(n_elements(x_axis) + 1)
  for i = 0, n_elements(x_axis) - 1 do header[i + 1] = string(x_axis[i])

  write_csv, fln_data, data_csv, header = header

end

; ------------------------------------------------------------------------------------------------------------------
; Purpose: make sliced heat map for samples, events and ratios for
; different z range
; Inputs: total_counts, event_counts, event_ratio, filepath
; ------------------------------------------------------------------------------------------------------------------
pro make_slice_heat_map, total_counts, event_counts, event_ratio, filepath, ext_condition_str, int_condition_str, ts_date, te_date, plot_axis, x_axis, y_axis, z_axis, z_cuttings, x_range, y_range, xlog, ylog, slice_grid, filename, events_v_log, events_v_range, events_unit, samples_v_log, samples_v_range, samples_unit, ratio_v_log, ratio_v_range, ratio_unit, ps_plot = ps_plot, slice_mlt = slice_mlt, threshold = threshold, region = region, ratio_correction = ratio_correction,remove_ps = remove_ps
  nz = n_elements(z_axis)

  slice_grid_str = string(slice_grid, format = '(i2.2)')
  path_slice = filepath + 'slice/slice_zgrid_' + slice_grid_str + '/'

  for iz = 0, nz - 1 do begin
    ; calculation
    slice_block = [strcompress(string(z_cuttings[iz, 0], format = '(f5.1)'), /remove_all), $
      strcompress(string(z_cuttings[iz, 1], format = '(f5.1)'), /remove_all)]

    slice_total_counts = total_counts[*, *, iz]
    slice_event_counts = event_counts[*, *, iz]
    slice_event_ratio = event_ratio[*, *, iz]

    if keyword_set(ratio_correction) then ratio_correction, slice_event_ratio, region, ratio_corrected
    slice_event_ratio = ratio_corrected

    index = where(slice_event_counts eq 0, ct)
    if ct gt 0 then slice_event_counts[index] = !values.f_nan
    index = where(slice_total_counts eq 0, ct)
    if ct gt 0 then slice_total_counts[index] = !values.f_nan

    if keyword_set(threshold) then begin
      index = where(slice_total_counts le threshold, ct)
      if ct gt 0 then begin
        slice_total_counts[index] = !values.f_nan
        slice_event_ratio[index] = !values.f_nan
        slice_event_counts[index] = !values.f_nan
      endif
    endif

    ; samples
    filename = path_slice + ext_condition_str + '_' + int_condition_str + '_samples_' + ts_date + '_to_' + te_date + '_' + plot_axis[0] + '_vs_' + plot_axis[1] + '_at_' + plot_axis[2] + '_' + slice_block[0] + '_' + slice_block[1]
    title = ext_condition_str + '!C' + int_condition_str + ' SAMPLES' + '!Cat ' + plot_axis[2] + ': [' + slice_block[0] + ',' + slice_block[1] + ']' + '!CFROM ' + ts_date + ' TO ' + te_date

    make_heat_map, x_axis, y_axis, slice_total_counts, filename + '.ps', title, plot_axis, unit = samples_unit, xrange = x_range, yrange = y_range, zrange = samples_v_range, xlog = xlog, ylog = ylog, zlog = samples_v_log, ps_plot = ps_plot,remove_ps = remove_ps
    
    ; write events counts into a csv file
    write_map_csv, filename + '.csv' , x_axis, y_axis, slice_total_counts

    ; events
    filename = path_slice + ext_condition_str + '_' + int_condition_str + '_events_' + ts_date + '_to_' + te_date + '_' + plot_axis[0] + '_vs_' + plot_axis[1] + '_at_' + plot_axis[2] + '_' + slice_block[0] + '_' + slice_block[1]
    title = ext_condition_str + '!C' + int_condition_str + ' EVENTS' + '!Cat ' + plot_axis[2] + ': [' + slice_block[0] + ',' + slice_block[1] + ']' + '!CFROM ' + ts_date + ' TO ' + te_date

    make_heat_map, x_axis, y_axis, slice_event_counts, filename + '.ps', title, plot_axis, unit = events_unit, xrange = x_range, yrange = y_range, zrange = events_v_range, xlog = xlog, ylog = ylog, zlog = events_v_log, ps_plot = ps_plot,remove_ps = remove_ps

    ; write events counts into a csv file
    write_map_csv, filename + '.csv' , x_axis, y_axis, slice_event_counts

    ; ratio
    filename = path_slice + ext_condition_str + '_' + int_condition_str + '_ratio_' + ts_date + '_to_' + te_date + '_' + plot_axis[0] + '_vs_' + plot_axis[1] + '_at_' + plot_axis[2] + '_' + slice_block[0] + '_' + slice_block[1] 
    title = ext_condition_str + '!C' + int_condition_str + ' RATIO' + '!Cat ' + plot_axis[2] + ': [' + slice_block[0] + ',' + slice_block[1] + ']' + '!CFROM ' + ts_date + ' TO ' + te_date

    make_heat_map, x_axis, y_axis, slice_event_ratio, filename+ '.ps', title, plot_axis, unit = ratio_unit, xrange = x_range, yrange = y_range, zrange = ratio_v_range, xlog = xlog, ylog = ylog, zlog = ratio_v_log, ps_plot = ps_plot,remove_ps = remove_ps

    ; write ratio into a csv file
    write_map_csv, filename + '.csv' , x_axis, y_axis, slice_event_ratio

  endfor
end

pro make_events_map, data_pos, flag_para, flag_anti, filepath, ts_date, te_date, plot_axis, ext_condition_str, int_condition_str, range, log, grid, slice_grid, filename, plot_2d, plot_slice, make_table, ps_plot = ps_plot, sample_v_range = sample_v_range, threshold = threshold, region = region, ratio_correction = ratio_correction, samples_v_range = samples_v_range, RATIO_V_RANGE = RATIO_V_RANGE, energy_range = energy_range,remove_ps = remove_ps

  x_range = range[*, 0]
  y_range = range[*, 1]
  z_range = range[*, 2]
  ; r_range = abs(y_range[0] - y_range[1])
  xlog = log[0]
  ylog = log[1]

  ; reset grid according to plot_axis
  if plot_axis[0] eq 'MLT' then begin
    grid_x = 0.5 * grid
    if abs(y_range[1]-y_range[0]) gt 10 then grid_y = grid else grid_y = 0.5 * grid
  endif else begin
    grid_x = grid
    grid_y = grid
  endelse

  if plot_axis[2] eq 'MLT' then  slice_mlt = 1

  ; Calculation
  calculate_ratio_for_map, data_pos, x_range, y_range, z_range, xlog, ylog, grid_x, grid_y, slice_grid, flag_para, flag_anti, total_counts, event_counts, event_ratio, x_axis, y_axis, z_axis, x_cuttings, y_cuttings, z_cuttings, slice_mlt = slice_mlt
  
  ; Graph settings
  EVENTS_V_LOG = 0
  EVENTS_V_RANGE = [10, 50.]
  events_unit = '# of events'
  samples_v_log = 0
  if ~keyword_set(sample_v_range) then samples_v_range = [10, 50.]
  samples_unit = '# of samples'
  ratio_V_LOG = 0
  if ~keyword_set(RATIO_V_RANGE) then RATIO_V_RANGE = [0, 1.]
  ratio_unit = 'Occurance Frequency'

  ; if ARRAY_EQUAL(energy_range,[1,40000]) then  RATIO_V_RANGE = [0, 1.] else  RATIO_V_RANGE = [0, 0.6]

  ; Draw 2d maps
  if keyword_set(plot_2d) then make_2d_heat_map, total_counts, event_counts, event_ratio, filepath, ext_condition_str, int_condition_str, ts_date, te_date, plot_axis, x_axis, y_axis, x_range, y_range, xlog, ylog, filename, EVENTS_V_LOG, EVENTS_V_RANGE, events_unit, samples_v_log, samples_v_range, samples_unit, ratio_V_LOG, RATIO_V_RANGE, ratio_unit, ps_plot = ps_plot, threshold = threshold, region = region, ratio_correction = ratio_correction,remove_ps = remove_ps

  ; Draw slice maps
  if keyword_set(plot_slice) then make_slice_heat_map, total_counts, event_counts, event_ratio, filepath, ext_condition_str, int_condition_str, ts_date, te_date, plot_axis, x_axis, y_axis, z_axis, z_cuttings, x_range, y_range, xlog, ylog, slice_grid, filename, EVENTS_V_LOG, EVENTS_V_RANGE, events_unit, samples_v_log, samples_v_range, samples_unit, ratio_V_LOG, RATIO_V_RANGE, ratio_unit, ps_plot = ps_plot, slice_mlt = slice_mlt, threshold = threshold, region = region, ratio_correction = ratio_correction,remove_ps = remove_ps

  if keyword_set(make_table) then stop
  
end