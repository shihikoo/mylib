PRO calculate_solarwind_delayed_1h, o1_energy_name, dist_name, varnames, outvarnames 
  get_data, o1_energy_name, data = data
  time_avg = data.x
  
  min_time = time_avg - 3600.

  FOR ii = 0, N_ELEMENTS(varnames)-1 DO calculate_delayed_solarwind_parameter, varnames(ii), outvarnames(ii), min_time, time_avg

END
