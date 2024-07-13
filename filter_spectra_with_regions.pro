PRO filter_spectra_with_regions, spectra_tplot_name, filter_tplot_name

  get_data, spectra_tplot_name, data = data
  time_avg = data.x
  y_avg = data.y
  v_avg = data.v

  get_data, filter_tplot_name, data = data
  time_avg = data.x
  filter_avg = data.y

  FOR idim = 0, N_ELEMENTS(y_avg(0,*))-1 DO BEGIN
     y_avg(*,idim) = y_avg(*,idim) * (filter_avg LE 3.)
  ENDFOR 
  
  str = {x:time_avg, y:y_avg, v:v_avg}
  store_data, spectra_tplot_name, data = str

END
