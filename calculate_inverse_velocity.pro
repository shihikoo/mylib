;----------------------------------------------------------------------
; Purpose: calculate inverse of the velocity from given energy cut
; tplot
; Inputs: epcut_beam_name, inverse_velocity_name
; Written by Jing Liao
; Written on 04/22/2021
;-----------------------------------------------------------------------

PRO calculate_inverse_velocity, sp, epcut_beam_name, epcut_beam_denergy_name, inverse_v_name

;-- Calculation --
; read in energy
  get_data, epcut_beam_name, data = data
  data_energy = data.y
  time_avg = data.x

  get_data, epcut_beam_denergy_name, data = data
  data_denergy = data.y
  
; energy in eV times electron_charge is enregy in joule amu divide by avogadro constant is mass in g, then divide 1e3 to get mass in kg Avogadro_constant 6.02214086e23 divide by 1e3 in the end to get velocity in km/s
  calculate_velocity_from_energy, data_energy, sp, data_vel

  data_1_of_vel = 1/data_vel

  calculate_velocity_from_energy, data_energy-data_denergy, 3, data_vel_low
  calculate_velocity_from_energy, data_energy+data_denergy, 3, data_vel_high
 
; inverse change the low / high direction  
  data_1_of_vel_low = -1./data_vel_high + data_1_of_vel
  data_1_of_vel_high = -data_1_of_vel + 1./data_vel_low

;store into tplot variable
  store_data, inverse_v_name , data= {x: time_avg, y: data_1_of_vel, dy:[[data_1_of_vel_low],[data_1_of_vel_high]]}

END
