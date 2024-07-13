PRO linear_regression_multi, x, y, m, b, chisq, yerr, yerror = yerror, yfit = yfit, status = status, dof = dof, merror = merror, berror = berror, rsquare = rsquare

  expr = 'p[0] + p[1]*X'
  IF ~KEYWORD_SET(yerror) THEN yerror = sqrt(y)
  start = [1463371950.0, 60000.]
  n_time = N_ELEMENTS(x)
  yerr = DBLARR(n_time)
  ; sometimes the error is different for upper and lower boundary
  IF N_ELEMENTS(yerror)/n_time EQ 2 THEN FOR i = 0, n_time-1 DO yerr[i] = MIN(yerror[i,*]) else yerr = yerror
  
  p = MPFITEXPR(expr, x, y, yerr, start, status = status, bestnorm = chisq, dof = dof,yfit=yfit, perror = perror)
  
  b = p[0]
  m = p[1]
  berror = perror[0]
  merror = perror[1]
  rsquare = CORRELATE(x,y)
  
END

PRO adjust_range, x_range, y_range, x_ticks, x_tickname,x, dispersion_m_array = dispersion_m_array

  time_m = dispersion_m_array[*,1]
  inverse_v_m = dispersion_m_array[*,2]
  inverse_v_error_m = dispersion_m_array[*,4]

  x_range = [min([time_m-100, x_range[0]]), max([time_m+100, x_range[1]])]
  y_range = [0, max(dispersion_m_array[*,2] + [inverse_v_error_m, y_range[1]])]
  x_ticks = N_ELEMENTS(x) + 2

  x_tickname = [time_double(x_tickname), min(time_m), max(time_m)]
  x_tickname = time_string(x_tickname[sort(x_tickname)])
END

PRO time_range_overlap_check, st1,et1,st2,et2, overlap
  overlap = ~(et1 LT st2 OR st1 GT et2)
END

PRO fit_dispersion_manual, filename, dispersion_list, energy_peak_names, x, to_plot, dispersion_m_array,  dispersion_m_fitting_array, rsquare = rsquare
  to_plot = 0
  IF ~KEYWORD_SET(dispersion_list) THEN RETURN
  ; Load dispersion_list
  m_t1 = dispersion_list[*,0]
  m_t2 = dispersion_list[*,1]
  manual_filename = dispersion_list[*,2]
  inverse_maxV = 1/dispersion_list[*,3]
  inverse_minV = 1/dispersion_list[*,4]
  
  ;use filename to filter the disperison list. Only work on those
  ;dispersions with the same filenames
  IF filename EQ 'PARA' THEN index = WHERE(manual_filename EQ 3, ct)
  IF filename EQ 'ANTI' THEN index = WHERE(manual_filename EQ 1 OR manual_filename EQ 4, ct)
  IF ct EQ 0 THEN RETURN 
                           
  to_plot_array = [] &  time_m_array = [] &   inverse_v_array = [] & inverse_v_low_array = [] & inverse_v_high_array = []
  m_array = [] &   b_array = [] &   merror_array = [] & berror_array = []
  FOR implot = 0, N_ELEMENTS(m_t1)-1 DO BEGIN
  ;     IF (m_t1[implot] LT max(x) AND m_t1[implot] GT min(x)) OR (m_t1[implot] LT max(x) AND m_t2[implot] GT min(x)) THEN BEGIN 
     time_range_overlap_check,m_t1[implot] , m_t2[implot], min(x), max(x), overlap
     IF overlap THEN BEGIN        
  ;        adjust_range, x_range, y_range, x_ticks, x_tickname, x, m_t1[implot], m_t2[implot], inverse_minV[implot], y, yerr
        get_data, energy_peak_names, data = data
        index = WHERE(data.x GT m_t1[implot] AND data.x LT m_t2[implot], ct)
        ; if ct eq 1 then stop else print,ct
  ;        to_plot_array = make_array(ct, value=to_plot)
        time_m = data.x[index]
        epcut_m = data.y[index]

        calculate_velocity_from_energy, epcut_m, 3, vel
        inverse_v =  1/vel      ;1/(sqrt(2.*data.y*electron_charge/(ion_mass/Avogadro_constant/1e3))/1e3) 
        read_in_denergy, epcut_m, epcut_denergy 

        calculate_velocity_from_energy, epcut_m + epcut_denergy, 3, vel_high
        calculate_velocity_from_energy, epcut_m - epcut_denergy, 3, vel_low

        inverse_v_low = -1./vel_high + inverse_v 
        inverse_v_high = -inverse_v + 1./vel_low

        yerror = [[inverse_v_low],[inverse_v_high]]

        linear_regression_multi, time_m, inverse_v, m, b, chisq, yerr, yerror = yerror, yfit = yfit, status = status, dof = dof, merror = merror, berror = berror , rsquare = rsquare

        to_plot_array = [to_plot_array, make_array(ct, value=to_plot)]
        time_m_array = [time_m_array, time_m]
        inverse_v_array = [inverse_v_array, inverse_v]
        inverse_v_low_array = [inverse_v_low_array, inverse_v_low]
        inverse_v_high_array = [inverse_v_high_array, inverse_v_high]

        m_array = [m_array, m]
        b_array = [b_array, b]
        merror_array = [merror_array, merror]
        berror_array = [berror_array, berror]
        to_plot = to_plot + 1
     ENDIF
  ENDFOR
  dispersion_m_array = [[ to_plot_array],[time_m_array],[ inverse_v_array], [inverse_v_low_array ],[ inverse_v_high_array ]]
  dispersion_m_fitting_array = [[m_array],[b_array],[merror_array], [berror_array]]
END

PRO make_lm_plot, filename, x, y, yerr, chisq, m, b, status, dof, berror, merror, to_plot $
                  , dispersion_m_array = dispersion_m_array,  dispersion_m_fitting_array = dispersion_m_fitting_array $
                  , ps_plot = ps_plot, idl_plot = idl_plot, dispersion_folder = dispersion_folder,rsquare = rsquare

  ; Set up time and time string for the plot
  t_s = min(x)
  t_e = max(x)
  ts_plot = time_string(t_s)
  te_plot = time_string(t_e)
  date_s_plot = STRMID(ts_plot, 0, 4) + STRMID(ts_plot, 5, 2) + STRMID(ts_plot, 8, 2)
  time_s_plot = STRMID(ts_plot, 11, 2) + STRMID(ts_plot, 14, 2) + STRMID(ts_plot, 17, 2)
  date_e_plot = STRMID(te_plot, 0, 4) + STRMID(te_plot, 5, 2) + STRMID(te_plot, 8, 2)
  time_e_plot = STRMID(te_plot, 11, 2) + STRMID(te_plot, 14, 2) + STRMID(te_plot, 17, 2)
  ps_folder = dispersion_folder 
  spawn, 'mkdir -p ' + ps_folder
  fln = ps_folder + 'o_beam'+ date_s_plot + '_' + time_s_plot + '_to_'+  date_e_plot + '_' + time_e_plot + 'dispersion_fitting_'+filename +'.ps'
  
  x_range = [min(x)-100, max(x)+100]
  y_range = [0, max(y+yerr)]
  x_ticks = N_ELEMENTS(x)
  x_tickname = time_string(x)
 
  ; plot the graph
  IF KEYWORD_SET(ps_plot) THEN POPEN, fln,/land
  
  IF to_plot gt 0 THEN adjust_range,  x_range, y_range, x_ticks, x_tickname, x, dispersion_m_array = dispersion_m_array

  plot, x, y, psym=7, xtickname = x_tickname $
        , xticks = x_ticks, xtitle = 't', ytitle='1/v', title='chisq:' + STRING(chisq, format='(d5.2)') +'  distance estimation: ' + STRING(1/m/6371., format='(d5.2)')+'  status: ' + STRING(status, format='(i1.1)') + ' dof: '+STRING(dof, format='(i1.1)') + ' r^2: '+STRING(rsquare,format='(d5.2)'), symsize=2, xstyle = 1, xrange= x_range, yrange = y_range
  
  errplot, x, y - yerr, y + yerr
  oplot, x, b+x*m, color = 2
  oplot, x, (b+berror)+x*(m+merror), color = 3
  oplot, x, (b-berror)+x*(m-merror), color = 3

  ;  IF KEYWORD_SET(dispersion_list) THEN  FOR implot = 0, N_ELEMENTS(m_t1)-1 DO oplot, [m_t1[implot], m_t2[implot]],[inverse_maxV[implot], inverse_minV[implot]], color = 4, psym = -1
  IF KEYWORD_SET(to_plot) THEN  BEGIN
     FOR iplot = 0, to_plot-1 DO BEGIN 
        index = WHERE(dispersion_m_array[*,0] EQ iplot, ct)
        
  ;        oplot, dispersion_m_array[index,1], dispersion_m_array[index,2], color = 4, psym = -1
        errplot, dispersion_m_array[index,1], dispersion_m_array[index,2] - dispersion_m_array[index,3] , dispersion_m_array[index,2] + dispersion_m_array[index,3], color = 1
        oplot,  dispersion_m_array[index,1],  dispersion_m_fitting_array[iplot,1] + dispersion_m_array[index,1]*dispersion_m_fitting_array[iplot,0] , color = 4
        oplot,  dispersion_m_array[index,1], (dispersion_m_fitting_array[iplot,1]+dispersion_m_fitting_array[iplot,3])+ dispersion_m_array[index,1]*(dispersion_m_fitting_array[iplot,0]+dispersion_m_fitting_array[iplot,2]), color = 5
        oplot,  dispersion_m_array[index,1], (dispersion_m_fitting_array[iplot,1]-dispersion_m_fitting_array[iplot,3])+ dispersion_m_array[index,1]*(dispersion_m_fitting_array[iplot,0]-dispersion_m_fitting_array[iplot,2]), color = 5
     ENDFOR 
  ENDIF
  IF KEYWORD_SET(ps_plot) THEN BEGIN 
     PCLOSE 
     png_fln = STRMID(fln, 0, STRPOS(fln,'.ps')) + '.png'
     spawn, 'mogrify -format png -alpha opaque -density 150 ' + fln
     spawn, 'mogrify -rotate -90 ' + png_fln
     spawn, 'rm -f '+fln
  ENDIF ELSE IF KEYWORD_SET(idl_plot) THEN stop
  
END

;perform dispersion on the dispersion 
pro process_dispersion_multi, dispersion_number, dispersion_flag, time_avg, inverse_v, inverse_v_error, filename, start_time_points, estimated_distance, chisq, yfit, status, dof, estimation_error, rsquare, dispersion_list, energy_peak_names, ps_plot = ps_plot, idl_plot = idl_plot, dispersion_folder = dispersion_folder
  ; dispersion_flag_1d = where(total(dispersion_flag eq dispersion_number, 2, /nan), ct)

  index_dispersion = where(total(dispersion_flag eq dispersion_number, 2, /nan), ct_dispersion) 

  x = time_avg[index_dispersion]
  start_time_points = x[0]

  inverse_v_1d = total(inverse_v*(dispersion_flag eq dispersion_number), 2, /nan)
  y = inverse_v_1d[index_dispersion]

  n_energy = n_elements(inverse_v[0,*])
  inverse_v_error_1d = [[total(inverse_v_error[*,0:(n_energy-1)]*(dispersion_flag eq dispersion_number), 2, /nan)],[total(inverse_v_error[*,n_energy:(n_energy*2-1)]*(dispersion_flag eq dispersion_number), 2, /nan)]]

  lower_error = inverse_v_error_1d[index_dispersion,0] ;total(inverse_v_error[index_dispersion, 0:(n_energy-1)], 2 ,/nan)
  upper_error= inverse_v_error_1d[index_dispersion,1] ;total(inverse_v_error[index_dispersion, n_energy:(n_energy*2-1)], 2 ,/nan)
  yerror = [[lower_error ], [upper_error]]
  ; if y[0] eq 0 then stop
  ;fitting
  linear_regression_multi, x, y, m, b, chisq, yerr, yerror = yerror, yfit = yfit_1d, status = status, dof = dof, merror = merror, berror = berror, rsquare = rsquare

  estimated_distance = 1./m
  estimation_error = MEAN([1./m-1./(m+merror), 1./(m-merror)-1./m])

  for iindex=0, ct_dispersion -1 do begin 
    jindex = where(dispersion_flag[index_dispersion[iindex], *] eq dispersion_number)
    yfit[index_dispersion[iindex], jindex] = yfit_1d[iindex]
  endfor 

  ; fit manual dispersion 
  fit_dispersion_manual, filename, dispersion_list, energy_peak_names, x, to_plot, dispersion_m_array,  dispersion_m_fitting_array

  ; plot the fitting
  IF KEYWORD_SET(ps_plot) OR KEYWORD_SET(idl_plot) THEN make_lm_plot, filename, x, y, yerr, chisq, m, b, status, dof, berror, merror, to_plot, dispersion_m_array = dispersion_m_array,  dispersion_m_fitting_array = dispersion_m_fitting_array, ps_plot = ps_plot, idl_plot = idl_plot, dispersion_folder = dispersion_folder, rsquare = rsquare

end

; the check_continuity_for_one function to check each point qualified for the basic dispersion shape: the later time energy peak is smaller. If dispersion patter is found then assign the dispersion_number in the dispersion_flag, if not return nothing changes
PRO check_continuity_for_one, i_time, i_energy, inverse_v, continuous_number, maximum_dispersion_energy_difference, dispersion_flag, dispersion_number, dispersion_length

    n_time = N_ELEMENTS(inverse_v[*,0])
  ; whether a flat energy is allowed. the number is 0 for not allowed and 1 for allowed
    flat_energy_allowed = 0
  ; mark the start point -1
    dispersion_flag[i_time, i_energy] = -1

    i_this_energy = i_energy
    for i_this_time = i_time + 1, n_time - 1 do begin   
      if i_this_time EQ (i_time + 1) then begin
        this_flat_energy_allowed = 0
        this_maximum_dispersion_energy_difference = maximum_dispersion_energy_difference
      endif else begin
        ;this_flat_energy_allowed = flat_energy_allowed
        this_maximum_dispersion_energy_difference = maximum_dispersion_energy_difference - 1
      endelse

      index_energy = (i_this_energy - 1 + this_flat_energy_allowed > 0) - indgen(this_maximum_dispersion_energy_difference + this_flat_energy_allowed < (i_this_energy + this_flat_energy_allowed))

      index = where(FINITE(inverse_v[i_this_time, index_energy]), ct)
      if ct gt 0 then begin
        i_this_energy = index_energy[index[0]]
        dispersion_flag[i_this_time, i_this_energy] = -1
        if this_flat_energy_allowed eq flat_energy_allowed then this_flat_energy_allowed = 0 else this_flat_energy_allowed = flat_energy_allowed
      endif else begin
        this_last_time = i_this_time - 1
        BREAK
      endelse
    endfor
    
    index = where(dispersion_flag eq -1, ct)
    if ct gt 0 then begin 
      dispersion_length = this_last_time - i_time + 1
;      if dispersion_length ge 4 then stop
      
      if (dispersion_length LT continuous_number) then dispersion_flag[index] = 0 else dispersion_flag[index] = dispersion_number
    endif
    
end

; find disperison from one data point at i_time and i_energy
pro identify_dispersions_for_one_multi, i_time, i_energy, filename, time_avg, inverse_v, inverse_v_error, dispersion_flag, continuous_number, maximum_dispersion_energy_difference, dispersion_number, start_time_points, estimated_distance, chisq, yfit, status, dof, estimation_error, dispersion_length, energy_peak_names, ps_plot = ps_plot, idl_plot = idl_plot, dispersion_folder = dispersion_folder, dispersion_list = dispersion_list, rsquare = rsquare

  ; dispersion_flag is  the output. It there is dispersion, the corresponding position (time and energy) will be marked as dispersion number. If there is no dispersion, nothing changes.
    check_continuity_for_one, i_time, i_energy, inverse_v, continuous_number, maximum_dispersion_energy_difference, dispersion_flag, dispersion_number, dispersion_length
    
  ; if the dispresion continuity requirement satisfied, we now calculate the linear regressions
    if dispersion_flag[i_time, i_energy] eq  dispersion_number then begin 
    process_dispersion_multi, dispersion_number, dispersion_flag, time_avg, inverse_v, inverse_v_error, filename, start_time_points, estimated_distance, chisq, yfit, status, dof, estimation_error, rsquare, dispersion_list, energy_peak_names, ps_plot = ps_plot, idl_plot = idl_plot, dispersion_folder = dispersion_folder
    endif
end

; identify dispersion from data
PRO identify_dispersions_from_array_multi, filename, time_avg, inverse_v, inverse_v_error, continuous_number, maximum_dispersion_energy_difference, dispersion_flag, dispersion_number, start_time_points, estimated_distance, chisq, yfit, status, dof, estimation_error,  energy_peak_names,  ps_plot = ps_plot, idl_plot = idl_plot, dispersion_folder = dispersion_folder, dispersion_list = dispersion_list, rsquare = rsquare

  ; initialize the variables
  ; create dispersion flag and set the initial as 0 for those that have peaks identified and Nan for those doesn't have energy peak
  dispersion_flag = inverse_v < 0
  yfit = inverse_v < 0
  start_time_points = []
  estimated_distance = []
  chisq = []
  rsquare = []
  status = []
  dof = []
  estimation_error = []
  dispersion_length = []

  n_time = N_ELEMENTS(inverse_v[*,0])
  n_energy = N_ELEMENTS(inverse_v[0,*])
  dispersion_number = 1

  for i_time = 0, n_time - continuous_number  do begin 
    for i_energy = 0, n_energy - continuous_number do begin
  ; if the energy peak is valid  then test whehter there is disperison starting at this point
        ;if finite(inverse_v[i_time, i_energy]) &
        if dispersion_flag[i_time, i_energy] eq 0 then begin
          identify_dispersions_for_one_multi, i_time, i_energy, filename, time_avg, inverse_v, inverse_v_error, dispersion_flag, continuous_number, maximum_dispersion_energy_difference, dispersion_number, ii_start_time_points, ii_estimated_distance, ii_chisq, yfit, ii_status, ii_dof, ii_estimation_error, ii_dispersion_length, energy_peak_names, ps_plot = ps_plot, idl_plot = idl_plot, dispersion_folder = dispersion_folder, dispersion_list = dispersion_list, rsquare = ii_rsquare

          if dispersion_flag[i_time, i_energy] eq dispersion_number  then begin 
            dispersion_number++
            start_time_points = [start_time_points, ii_start_time_points]
            estimated_distance = [estimated_distance,ii_estimated_distance]
            chisq = [chisq,ii_chisq]
            status = [status,ii_status]
            dof = [dof,ii_dof]
            estimation_error = [estimation_error,ii_estimation_error]
            dispersion_length = [dispersion_length,ii_dispersion_length]
            rsquare = [rsquare,ii_rsquare]

            i_time = i_time + ii_dispersion_length - 1
          endif 
        endif
    endfor
  endfor
  dispersion_number = dispersion_number - 1

END


PRO identify_dispersions_multi, average_time, filename, epcut_beam_name, dispersion_name, beam_inverse_v_name, dispersion_inverse_v_name, estimated_dist_name, estimated_dist_error_name,  dispersion_inverse_v_fitting_name, dispersion_inverse_v_fitting_chisq_name, dispersion_inverse_v_fitting_status_name, dispersion_inverse_v_fitting_dof_name, dispersion_n_name, energy_peak_names, ps_plot = ps_plot, idl_plot = idl_plot, dispersion_folder = dispersion_folder, dispersion_list = dispersion_list, rsquare = rsquare, dispersion_inverse_v_fitting_rsquare_name = dispersion_inverse_v_fitting_rsquare_name, maximum_dispersion_energy_difference = maximum_dispersion_energy_difference, continuous_time = continuous_time, reverse_v = reverse_v
  
  earth_radius = 6371.

  ; set up the minimum dispersion time if not given
  if ~KEYWORD_SET(continuous_time) then continuous_time = 8. * 60
  continuous_number = ceil(continuous_time / average_time)
  ; set up the maximum energy differences for two time point
  if ~KEYWORD_SET(maximum_dispersion_energy_difference) then maximum_dispersion_energy_difference = 2
  
  get_data, epcut_beam_name, data = data
  time_avg = data.x
  epcut_beam_dispersion = data.y
  n_energy = N_ELEMENTS(epcut_beam_dispersion[0,*])

  get_data, beam_inverse_v_name, data = data
  inverse_v = data.y
  inverse_v_error = data.dy

  if keyword_set(reverse_v) then begin
    inverse_v = REVERSE(inverse_v,2)
    inverse_v_error = REVERSE(inverse_v_error,2)
  endif 

  identify_dispersions_from_array_multi, filename, time_avg, inverse_v, inverse_v_error, continuous_number, maximum_dispersion_energy_difference, dispersion_flag, dispersion_number, start_time_points, estimated_distance, chisq, yfit, status, dof, estimation_error, energy_peak_names,  ps_plot = ps_plot, idl_plot = idl_plot, dispersion_folder = dispersion_folder, dispersion_list = dispersion_list, rsquare = rsquare

  if keyword_set(reverse_v) then begin
    inverse_v = REVERSE(inverse_v,2)
    inverse_v_error = REVERSE(inverse_v_error,2)
    dispersion_flag = REVERSE(dispersion_flag,2)

  endif

  ; index = where(dispersion_flag eq 0, ct)
  ; if ct gt 0 then begin
  ;   epcut_beam_dispersion[index] = !values.F_nan
  ;   inverse_v[index] = !values.F_nan
  ;   inverse_v_error[index] = !values.F_nan
  ;   dispersion_flag[index] = !values.F_nan
  ;   yfit[index] = !values.F_nan      
  ; endif 
  
  dispersion_flag_1d = total(dispersion_flag, 2, /nan)
  epcut_beam_dispersion_1d = total(epcut_beam_dispersion*(dispersion_flag gt 0), 2, /nan)
  inverse_v_1d = total(inverse_v*(dispersion_flag gt 0), 2, /nan)
  inverse_v_error_1d = [[total(inverse_v_error[*,0:(n_energy-1)]*(dispersion_flag gt 0), 2, /nan)],[total(inverse_v_error[*,n_energy:(n_energy*2-1)]*(dispersion_flag gt 0), 2, /nan)]]
  yfit_1d = total(yfit*(dispersion_flag gt 0), 2, /nan)

  index = where(dispersion_flag_1d eq 0, ct)
  if ct gt 0 then begin
    epcut_beam_dispersion_1d[index] = !values.F_nan
    inverse_v_1d[index] = !values.F_nan
    inverse_v_error_1d[index,*] = !values.F_nan
    yfit_1d[index] = !values.F_nan
  endif    
  
  store_data, dispersion_name, data = {x:time_avg, y:epcut_beam_dispersion_1d}
  store_data, dispersion_inverse_v_name, data = {x:time_avg, y:inverse_v_1d, dy:inverse_v_error_1d}
  store_data, dispersion_inverse_v_fitting_name, data = {x:time_avg, y:yfit_1d}

  if dispersion_number EQ 0 then begin
    start_time_points = !values.f_nan
    chisq = !values.f_nan
    status = !values.f_nan
    dof = !values.f_nan
    rsquare = !values.f_nan
    estimated_distance = !values.f_nan
    estimation_error = !values.f_nan
    dispersion_numbers = 0
  endif else dispersion_numbers = indgen(dispersion_number)+1
  
  store_data, dispersion_inverse_v_fitting_chisq_name, data = {x:start_time_points, y:chisq}
  store_data, dispersion_inverse_v_fitting_status_name, data = {x:start_time_points, y:status}
  store_data, dispersion_inverse_v_fitting_dof_name, data = {x:start_time_points, y:dof}
  store_data, dispersion_inverse_v_fitting_rsquare_name, data= {x:start_time_points, y:rsquare}  
  store_data, estimated_dist_name, data = {x:start_time_points, y:estimated_distance/earth_radius}  
  store_data, estimated_dist_error_name, data = {x:start_time_points, y:estimation_error/earth_radius}
  store_data, dispersion_n_name, data = {x:start_time_points, y:dispersion_numbers}

  options, dispersion_inverse_v_fitting_name, 'color', 2
  options, dispersion_inverse_v_name, 'psym', 7
  
END 
