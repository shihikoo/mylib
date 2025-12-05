PRO  find_substorm_phase, time_avg, substorm_phase_filename = substorm_phase_filename, substorm_phase_tplot_name = substorm_phase_tplot_name

  IF ~KEYWORD_SET(substorm_phase_filename) THEN substorm_phase_filename = 'data/substorm_list_2016_2017.csv'
  IF ~KEYWORD_SET(substorm_phase_tplot_name) THEN substorm_phase_tplot_name = 'substorm_phase'

  phase_time_period = 30. * 60

  substorm_phase_data = READ_CSV(substorm_phase_filename, HEADER = substorm_phase_header)
  substorm_onset = TIME_DOUBLE(substorm_phase_data.FIELD1[1:N_ELEMENTS(substorm_phase_data.FIELD1)-1])

  index = where(substorm_onset gt (min(time_avg) - phase_time_period*2) and substorm_onset lt (max(time_avg) + phase_time_period*2),ct)
  substorm_onset = substorm_onset[index]

  nsubstorm = N_ELEMENTS(substorm_onset)
  n_time = N_ELEMENTS(time_avg)

  substorm_phase = DBLARR(n_time)
  substorm_phase[*] = !VALUES.F_NAN
  FOR isubstorm = 0, nsubstorm -1 DO BEGIN
     index = WHERE(time_avg GE (substorm_onset[isubstorm] - phase_time_period) AND time_avg LE (substorm_onset[isubstorm]) AND ~FINITE(substorm_phase), ct)
     IF ct GT 0 THEN substorm_phase[index] = time_avg[index] - substorm_onset[isubstorm]

     index = WHERE(time_avg LE (substorm_onset[isubstorm] + phase_time_period) AND time_avg GE (substorm_onset[isubstorm]), ct)
     IF ct GT 0 THEN substorm_phase[index] = time_avg[index] - substorm_onset[isubstorm]

    ;  print, substorm_phase
     
  ENDFOR 

  store_data, substorm_phase_tplot_name, data = {x:time_avg, y:substorm_phase}
  ylim, substorm_phase_tplot_name, -phase_time_period, phase_time_period

END
