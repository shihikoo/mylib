pro filter_spectra_with_regions, spectra_tplot_name, filter_tplot_name
  get_data, spectra_tplot_name, data = data
  time_avg = data.x
  y_avg = data.y
  v_avg = data.v

  get_data, filter_tplot_name, data = data
  time_avg = data.x
  filter_avg = data.y

  for idim = 0, n_elements(y_avg[0, *]) - 1 do begin
    index_invalid = where(~finite(filter_avg), ct_invalid)
    if ct_invalid gt 0 then y_avg[index_invalid, idim] = !values.f_nan

    index_outside_magentosphere = where(filter_avg gt 3, ct_outside_magentosphere)
    if ct_outside_magentosphere gt 0 then y_avg[index_outside_magentosphere, idim] = !values.f_nan

    ; y_avg[*, idim] = y_avg[*, idim] * (filter_avg le 3.)
  endfor

  str = {x: time_avg, y: y_avg, v: v_avg}
  store_data, spectra_tplot_name, data = str
end