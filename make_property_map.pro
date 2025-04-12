; ----------------------------------------------
; Purpose: Calculate property value in axis for map
; Inputs:
; Written by Jing Liao
; Written on 05/12/2021
; ----------------------------------------------------
pro calculate_property_map, property_para, property_anti, data_pos, x_range, y_range, z_range, x_log, y_log, grid_x, grid_y, grid_z, flag_para, flag_anti, property_value, x_axis, y_axis, z_axis, x_cuttings, y_cuttings, z_cuttings, slice_mlt = slice_mlt
  ntime = n_elements(property_para)

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

  property_value = replicate(!values.f_nan, ntime, 2, nx, ny, nz)

  for ix = 0, n_elements(x_axis) - 1 do begin
    for iy = 0, n_elements(y_axis) - 1 do begin
      for iz = 0, n_elements(z_axis) - 1 do begin
        if z_cuttings[iz, 0] gt z_cuttings[iz, 1] and keyword_set(slice_mlt) then begin
          index_para = where(data_pos[*, 0] ge x_cuttings[ix, 0] and data_pos[*, 0] lt x_cuttings[ix, 1] and $
            data_pos[*, 1] ge y_cuttings[iy, 0] and data_pos[*, 1] lt y_cuttings[iy, 1] and $
            ((data_pos[*, 2] ge z_cuttings[iz, 0] and data_pos[*, 2] lt 24.) $
              or (data_pos[*, 2] ge 0 and data_pos[*, 2] lt z_cuttings[iz, 1])) and $
            abs(flag_para) ge 1, ct_para) ; FINITE(flag_para), ct_para)
        endif else begin
          index_para = where(data_pos[*, 0] ge x_cuttings[ix, 0] and data_pos[*, 0] lt x_cuttings[ix, 1] and $
            data_pos[*, 1] ge y_cuttings[iy, 0] and data_pos[*, 1] lt y_cuttings[iy, 1] and $
            data_pos[*, 2] ge z_cuttings[iz, 0] and data_pos[*, 2] lt z_cuttings[iz, 1] and $
            abs(flag_para) ge 1, ct_para) ; FINITE(flag_para), ct_para)
        endelse

        if ct_para gt 1 then property_value[index_para, 0, ix, iy, iz] = property_para[index_para]

        if z_cuttings[iz, 0] gt z_cuttings[iz, 1] and keyword_set(slice_mlt) then begin
          index_anti = where(data_pos[*, 0] ge x_cuttings[ix, 0] and data_pos[*, 0] lt x_cuttings[ix, 1] and $
            data_pos[*, 1] ge y_cuttings[iy, 0] and data_pos[*, 1] lt y_cuttings[iy, 1] and $
            ((data_pos[*, 2] ge z_cuttings[iz, 0] and data_pos[*, 2] lt 24.) $
              or (data_pos[*, 2] ge 0 and data_pos[*, 2] lt z_cuttings[iz, 1])) and $
            abs(flag_anti) ge 1, ct_anti) ; FINITE(flag_anti), ct_anti)
        endif else begin
          index_anti = where(data_pos[*, 0] ge x_cuttings[ix, 0] and data_pos[*, 0] lt x_cuttings[ix, 1] and $
            data_pos[*, 1] ge y_cuttings[iy, 0] and data_pos[*, 1] lt y_cuttings[iy, 1] and $
            data_pos[*, 2] ge z_cuttings[iz, 0] and data_pos[*, 2] lt z_cuttings[iz, 1] and $
            abs(flag_anti) ge 1, ct_anti) ; FINITE(flag_anti), ct_anti)
        endelse
        if ct_anti gt 1 then property_value[index_anti, 1, ix, iy, iz] = property_anti[index_anti]
      endfor
    endfor
  endfor
end

; ------------
; Purpose: calculate quartile output depending on requested value
; Inputs:
; -----------------
function calcuate_quartile, input_array, output_type
  index = where(finite(input_array), ct)
  if ct eq 0 then return, !values.f_nan
  input_array = input_array[index]
  sortedData = input_array[sort(input_array)]
  if n_elements(sortedData) mod 2 eq 0 then begin
    index = n_elements(sortedData) / 2
    medianData = (sortedData[index - 1] + sortedData[index]) / 2.0
    lowerGroup = sortedData[0 : index - 1]
    higherGroup = sortedData[index : n_elements(data) - 1]
  endif else begin
    index = n_elements(sortedData) / 2
    medianData = sortedData[index]
    lowerGroup = sortedData[0 : index - 1]
    higherGroup = sortedData[index + 1 : n_elements(data) - 1]
  endelse

  if output_type eq 'q1' then return, median(lowerGroup, /even)
  if output_type eq 'q3' then return, median(higherGroup, /even)
  if output_type eq 'iqr' then return, median(higherGroup, /even) - median(lowerGroup, /even)

  return, median(sortedData, /even)
end

; ----------------------------------------------------------
; Purpose: calculate property map based on type required
; Inputs: property_value, property_map_type
; ------------------------------------------------------------
function aggregate_property_value, property_value, property_map_type, threshold = threshold, total_counts = total_counts
  if size(property_value, /n_dim) ne 4 and size(property_value, /n_dim) ne 5 then stop
  if size(property_value, /n_dim) eq 5 then begin
    nx = n_elements(property_value[0, 0, *, 0, 0])
    ny = n_elements(property_value[0, 0, 0, *, 0])
  endif else begin
    nx = n_elements(property_value[0, 0, *, 0])
    ny = n_elements(property_value[0, 0, 0, *])
  endelse
  
  property_result = replicate(!values.f_nan, nx, ny)

  ; -------- mean
  if property_map_type eq 'mean' then begin
    if size(property_value, /n_dim) eq 5 then begin
      property_result = total(total(total(property_value, 2, /nan), 1, /nan), 3, /nan) / $
        total(total(total(abs(property_value) ge 0, 2, /nan), 1, /nan), 3, /nan)
    endif else begin
      property_result = total(total(property_value, 2, /nan), 1, /nan) / $
        total(total(abs(property_value) ge 0, 2, /nan), 1, /nan)
    endelse
  endif

  ; -------- median
  if property_map_type eq 'median' then begin
    for ix = 0, nx - 1 do begin
      for iy = 0, ny - 1 do begin
        if size(property_value, /n_dim) eq 5 then dummy = property_value[*, *, ix, iy, *] else dummy = property_value[*, *, ix, iy]
        index = where(finite(dummy), ct)
        if ct gt 0 then dummy = dummy[index]
        property_result[ix, iy] = median(dummy, /even)
      endfor
    endfor
  endif

  ; -------- minimum
  if property_map_type eq 'minimum' then begin
    for ix = 0, nx - 1 do begin
      for iy = 0, ny - 1 do begin
        if size(property_value, /n_dim) eq 5 then dummy = property_value[*, *, ix, iy, *] else dummy = property_value[*, *, ix, iy]
        property_result[ix, iy] = min(dummy, /nan)
      endfor
    endfor
  endif

  ; -------- maximum
  if property_map_type eq 'maximum' then begin
    for ix = 0, nx - 1 do begin
      for iy = 0, ny - 1 do begin
        if size(property_value, /n_dim) eq 5 then dummy = property_value[*, *, ix, iy, *] else dummy = property_value[*, *, ix, iy]
        property_result[ix, iy] = max(dummy, /nan)
      endfor
    endfor
  endif

  ; -------- difference
  if property_map_type eq 'difference' then begin
    for ix = 0, nx - 1 do begin
      for iy = 0, ny - 1 do begin
        if size(property_value, /n_dim) eq 5 then dummy = property_value[*, *, ix, iy, *] else dummy = property_value[*, *, ix, iy]
        property_result[ix, iy] = abs(max(dummy, /nan) - min(dummy, /nan))
      endfor
    endfor
  endif

  ; -------- standard deviation
  if property_map_type eq 'sd' then begin
    for ix = 0, nx - 1 do begin
      for iy = 0, ny - 1 do begin
        if size(property_value, /n_dim) eq 5 then dummy = property_value[*, *, ix, iy, *] else dummy = property_value[*, *, ix, iy]
        property_result[ix, iy] = stddev(dummy, /nan)
      endfor
    endfor
  endif

  ; -------- upper quartile
  if property_map_type eq 'q3' then begin
    for ix = 0, nx - 1 do begin
      for iy = 0, ny - 1 do begin
        if size(property_value, /n_dim) eq 5 then dummy = property_value[*, *, ix, iy, *] else dummy = property_value[*, *, ix, iy]
        property_result[ix, iy] = calcuate_quartile(dummy, 'q3')
      endfor
    endfor
  endif

  ; -------- lower quartile
  if property_map_type eq 'q1' then begin
    for ix = 0, nx - 1 do begin
      for iy = 0, ny - 1 do begin
        if size(property_value, /n_dim) eq 5 then dummy = property_value[*, *, ix, iy, *] else dummy = property_value[*, *, ix, iy]
        property_result[ix, iy] = calcuate_quartile(dummy, 'q1')
      endfor
    endfor
  endif

  ; -------- lower quartile
  if property_map_type eq 'iqr' then begin
    for ix = 0, nx - 1 do begin
      for iy = 0, ny - 1 do begin
        if size(property_value, /n_dim) eq 5 then dummy = property_value[*, *, ix, iy, *] else dummy = property_value[*, *, ix, iy]
        property_result[ix, iy] = calcuate_quartile(dummy, 'iqr')
      endfor
    endfor
  endif

  index = where(total_counts le threshold, ct)
  if ct gt 0 then property_result[index] = !values.f_nan

  return, property_result
end

; ----------------------------------------------------
; Purpose: make 2d property map
; Inputs: property_value, property_map_type, property_name, filepath, ts_date, te_date, plot_axis, x_axis, y_axis, x_range, y_range,filename,  property_v_log, property_v_range, property_unit
; Keywords:ps_plot
; ----------------------------------------------------
pro make_2d_property_map, property_value, property_map_type, property_name, filepath, ext_condition_str, int_condition_str, ts_date, te_date, plot_axis, x_axis, y_axis, x_range, y_range, xlog, ylog, filename, property_v_log, property_v_range, property_unit, ps_plot = ps_plot, threshold = threshold, total_counts = total_counts
  ; filepath
  path_pp_2d = filepath + '2d/'

  ; calculation
  property_map_2d = aggregate_property_value(property_value, property_map_type, threshold = threshold, total_counts = total(total_counts, 3, /nan))

  ; draw heat map
  filename = path_pp_2d + ext_condition_str + '_' + int_condition_str + '_' + property_map_type + '_' + property_name + '_' + '_' + ts_date + '_to_' + te_date + '_' + plot_axis[0] + '_vs_' + plot_axis[1] + '.ps'
  title = ext_condition_str + '!C' + int_condition_str + ' O!U+!N beam!C' + property_map_type + ' ' + property_name + '!Cfrom ' + ts_date + ' TO ' + te_date
  make_heat_map, x_axis, y_axis, property_map_2d, filename, title, plot_axis, unit = property_unit, xrange = x_range, yrange = y_range, zrange = property_v_range, xlog = xlog, ylog = ylog, zlog = property_v_log, ps_plot = ps_plot

  ; write ratio into a csv file
  fln_data = path_pp_2d + ext_condition_str + '_' + int_condition_str + '_' + property_map_type + '_' + property_name + '_' + '_' + ts_date + '_to_' + te_date + '_' + plot_axis[0] + '_vs_' + plot_axis[1] + '.csv'

  data_csv = [reform(y_axis, 1, n_elements(y_axis)), property_map_2d]

  header = strarr(n_elements(x_axis) + 1)

  for i = 0, n_elements(x_axis) - 1 do header[i + 1] = string(x_axis[i])

  write_csv, fln_data, data_csv, header = header
end

pro make_slice_property_map, property_value, property_map_type, property_name, filepath, ext_condition_str, int_condition_str, ts_date, te_date, plot_axis, x_axis, y_axis, z_axis, z_cuttings, x_range, y_range, xlog, ylog, slice_grid, filename, property_v_log, property_v_range, property_unit, ps_plot = ps_plot, slice_mlt = slice_mlt, threshold = threshold, total_counts = total_counts
  nz = n_elements(z_axis)

  slice_grid_str = string(slice_grid, format = '(i2.2)')
  path_pp_slice = filepath + 'slice/slice_zgrid_' + slice_grid_str + '/'

  for iz = 0, nz - 1 do begin
    ; calculation
    property_map_slice = aggregate_property_value(property_value[*, *, *, *, iz], property_map_type, threshold = threshold, total_counts = total_counts[*, *, iz])

    ; draw the plot
    slice_block = [strcompress(string(z_cuttings[iz, 0], format = '(f5.1)'), /remove_all), $
      strcompress(string(z_cuttings[iz, 1], format = '(f5.1)'), /remove_all)]

    filename = path_pp_slice + ext_condition_str + '_' + int_condition_str + '_' + property_map_type + '_' + property_name + '_' + '_' + ts_date + '_to_' + te_date + '_' + plot_axis[0] + '_vs_' + plot_axis[1] + '_at_' + plot_axis[2] + '_' + slice_block[0] + '_' + slice_block[1] + '.ps'
    title = ext_condition_str + '!C' + int_condition_str + ' O!U+!N beam!C' + property_map_type + ' ' + property_name + ' at ' + plot_axis[2] + ':[ ' + slice_block[0] + ',' + slice_block[1] + ' ] ' + '!CFROM ' + ts_date + ' TO ' + te_date

    make_heat_map, x_axis, y_axis, property_map_slice, filename, title, plot_axis, unit = property_unit, xrange = x_range, yrange = y_range, zrange = property_v_range, xlog = xlog, ylog = ylog, zlog = property_v_log, ps_plot = ps_plot

    ; write ratio into a csv file
    fln_data = path_pp_slice + ext_condition_str + '_' + int_condition_str + '_' + property_map_type + '_' + property_name + '_' + '_' + ts_date + '_to_' + te_date + '_' + plot_axis[0] + '_vs_' + plot_axis[1] + '_at_' + plot_axis[2] + '_' + slice_block[0] + '_' + slice_block[1] + '.csv'

    data_csv = [reform(y_axis, 1, n_elements(y_axis)), property_map_slice]

    header = strarr(n_elements(x_axis) + 1)

    for i = 0, n_elements(x_axis) - 1 do header[i + 1] = string(x_axis[i])

    write_csv, fln_data, data_csv, header = header
  endfor
end

; ------------------------------------------------------------------------------
; Purpose: make property map
; Inputs: data, data_pos, property_name, property_map_type, flag, filepath, ts_date, te_date, plot_axis,  x_range, y_range, z_range, grid, slice_grid, filename, plot_2d, plot_slice, make_table
; Keywords: ps_plot
; ------------------------------------------------------------------------------
pro make_property_map, data, data_pos, property_name, property_map_type, flag_para, flag_anti, filepath, ts_date, te_date, plot_axis, ext_condition_str, int_condition_str, range, log, grid, slice_grid, filename, plot_2d, plot_slice, make_table, ps_plot = ps_plot, threshold = threshold, total_counts = total_counts
  X_RANGE = range[*, 0]
  Y_RANGE = range[*, 1]
  Z_RANGE = range[*, 2]
  ; r_range=ABS(y_range[0]-y_RANGE[1])
  xlog = log[0]
  ylog = log[1]

  ; load property data and property graphing settings
  load_property_data, data, property_name, property_para, property_anti, property_v_log, property_v_range, property_unit, property_map_type = property_map_type

  ; reset grid according to plot_axis
  if plot_axis[0] eq 'MLT' then begin
    grid_x = 0.5 * grid
    grid_y = 1.25 * grid
  endif else begin
    grid_x = grid
    grid_y = grid
  endelse
  grid_z = slice_grid

  if plot_axis[2] eq 'MLT' then begin
    slice_mlt = 1
  endif

  ; Calculation
  calculate_property_map, property_para, property_anti, data_pos, X_RANGE, Y_RANGE, Z_RANGE, xlog, ylog, grid_x, grid_y, grid_z, flag_para, flag_anti, property_value, x_axis, y_axis, z_axis, x_cuttings, y_cuttings, z_cuttings, slice_mlt = slice_mlt

  ; Draw 2d maps
  if keyword_set(plot_2d) then make_2d_property_map, property_value, property_map_type, property_name, filepath, ext_condition_str, int_condition_str, ts_date, te_date, plot_axis, x_axis, y_axis, X_RANGE, Y_RANGE, xlog, ylog, filename, property_v_log, property_v_range, property_unit, ps_plot = ps_plot, threshold = threshold, total_counts = total_counts

  ; Draw slice maps
  if keyword_set(plot_slice) then make_slice_property_map, property_value, property_map_type, property_name, filepath, ext_condition_str, int_condition_str, ts_date, te_date, plot_axis, x_axis, y_axis, z_axis, z_cuttings, X_RANGE, Y_RANGE, xlog, ylog, slice_grid, filename, property_v_log, property_v_range, property_unit, ps_plot = ps_plot, slice_mlt = slice_mlt, threshold = threshold, total_counts = total_counts

  ; make historgram
  if keyword_set(make_table) then stop
end