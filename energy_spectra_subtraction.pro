pro energy_spectra_subtraction, enspec_name, enspec_name_2, enspec_name_new

  get_data,enspec_name, data = data,dlim=dlim,lim=lim
  get_data,enspec_name_2, data = data2

  x = data.x
  y = (data.y - (data2.y>0)) > 0 
  v = data.v
  
  store_data, enspec_name_new, data={x:x, y:y, v:v}, dlim=dlim,lim=lim

  ;stop
end
