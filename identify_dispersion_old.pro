PRO linear_regression_old, x, y, m, b, chisq, yerr, yerror = yerror, yfit = yfit, status = status, dof = dof, merror = merror, berror = berror, rsquare = rsquare

  expr = 'p[0] + p[1]*X'
  IF ~KEYWORD_SET(yerror) THEN yerror = sqrt(y)
  start = [1463371950.0, 60000.]
  n_time = N_ELEMENTS(x)
  yerr = DBLARR(n_time)
  IF N_ELEMENTS(yerror)/n_time EQ 2 THEN FOR i = 0, n_time-1 DO yerr[i] = MIN(yerror[i,*])
  
  p = MPFITEXPR(expr, x, y, yerr, start, status = status, bestnorm = chisq, dof = dof,yfit=yfit, perror = perror)
  
  b = p[0]
  m = p[1]
  berror = perror[0]
  merror = perror[1]
  rsquare = CORRELATE(x,y)
END

PRO adjust_range_old, x_range, y_range, x_ticks, x_tickname,x, dispersion_m_array = dispersion_m_array

  time_m = dispersion_m_array[*,1]
  ; inverse_v_m = dispersion_m_array[*,2]
  inverse_v_error_m = dispersion_m_array[*,4]

  x_range = [min([time_m-100, x_range[0]]), max([time_m+100, x_range[1]])]
  y_range = [0, max(dispersion_m_array[*,2] + [inverse_v_error_m, y_range[1]])]
  x_ticks = N_ELEMENTS(x) + 2

  x_tickname = [time_double(x_tickname), min(time_m), max(time_m)]
  x_tickname = time_string(x_tickname[sort(x_tickname)])
END

PRO time_range_overlap_check_old, st1,et1,st2,et2, overlap
  overlap = ~(et1 LT st2 OR st1 GT et2)
END

PRO fit_dispersion_manual_old, direction, dispersion_list, energy_peak_names, x, to_plot, dispersion_m_array,  dispersion_m_fitting_array, rsquare = rsquare
  to_plot = 0
  IF ~KEYWORD_SET(dispersion_list) THEN RETURN
; Load dispersion_list
  m_t1 = dispersion_list[*,0]
  m_t2 = dispersion_list[*,1]
  manual_direction = dispersion_list[*,2]
  inverse_maxV = 1/dispersion_list[*,3]
  inverse_minV = 1/dispersion_list[*,4]
  
;use direction to filter the disperison list. Only work on those
;dispersions with the same directions
  IF direction EQ 'PARA' THEN index = WHERE(manual_direction EQ 3, ct)
  IF direction EQ 'ANTI' THEN index = WHERE(manual_direction EQ 1 OR manual_direction EQ 4, ct)
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
      
        linear_regression, time_m, inverse_v, m, b, chisq, yerr, yerror = yerror, yfit = yfit, status = status, dof = dof, merror = merror, berror = berror , rsquare = rsquare

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


PRO make_lm_plot_old, direction, x, y, yerr, chisq, m, b, status, dof, berror, merror, to_plot $
                  , dispersion_m_array = dispersion_m_array,  dispersion_m_fitting_array = dispersion_m_fitting_array $
                  , ps_plot = ps_plot, idl_plot = idl_plot, output_folder = output_folder,rsquare = rsquare
 
; Set up time and time string for the plot
  t_s = min(x)
  t_e = max(x)
  ts_plot = time_string(t_s)
  te_plot = time_string(t_e)
  date_s_plot = STRMID(ts_plot, 0, 4) + STRMID(ts_plot, 5, 2) + STRMID(ts_plot, 8, 2)
  time_s_plot = STRMID(ts_plot, 11, 2) + STRMID(ts_plot, 14, 2) + STRMID(ts_plot, 17, 2)
  date_e_plot = STRMID(te_plot, 0, 4) + STRMID(te_plot, 5, 2) + STRMID(te_plot, 8, 2)
  time_e_plot = STRMID(te_plot, 11, 2) + STRMID(te_plot, 14, 2) + STRMID(te_plot, 17, 2)
  ps_folder = output_folder + 'plots/dispersion/'
  spawn, 'mkdir -p ' + ps_folder
  fln = ps_folder + 'o_beam'+ date_s_plot + '_' + time_s_plot + '_to_'+  date_e_plot + '_' + time_e_plot + 'dispersion_fitting_'+direction +'.ps'
  
  x_range = [min(x)-100, max(x)+100]
  y_range = [0, max(y+yerr)]
  x_ticks = N_ELEMENTS(x)
  x_tickname = time_string(x)
 
; plot the graph
  IF KEYWORD_SET(ps_plot) THEN POPEN, fln,/land
  
  IF to_plot > 0 THEN adjust_range,  x_range, y_range, x_ticks, x_tickname, x, dispersion_m_array = dispersion_m_array

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

PRO process_dispersion_old,  time_avg, inverse_v, inverse_v_error, start_index, end_index, estimated_distance, chisq, yfit, status, dof, estimation_error, direction, dispersion_list, energy_peak_names, ps_plot = ps_plot, idl_plot = idl_plot, output_folder = output_folder, rsquare = rsquare

;set inputs from start_index to end_index
  x = time_avg[start_index:end_index]
  y = inverse_v[start_index:end_index]
  yerror = inverse_v_error[start_index:end_index, *]

;fitting
  linear_regression, x, y, m, b, this_chisq, yerr, yerror = yerror, yfit=this_yfit, status = this_status, dof = this_dof, merror = merror, berror = berror, rsquare = this_rsquare

; fit manual dispersion 
  fit_dispersion_manual, direction, dispersion_list, energy_peak_names, x, to_plot, dispersion_m_array,  dispersion_m_fitting_array

; plot the fitting
  IF KEYWORD_SET(PS_plot) OR KEYWORD_SET(idl_plot) THEN make_lm_plot, direction, x, y, yerr, this_chisq, m, b, this_status, this_dof, berror, merror, to_plot $
     , dispersion_m_array = dispersion_m_array,  dispersion_m_fitting_array = dispersion_m_fitting_array $
     , ps_plot = ps_plot, idl_plot = idl_plot, output_folder = output_folder,rsquare = this_rsquare

; store output into full array
  estimated_distance[start_index:end_index] = 1./m
  chisq[start_index:end_index] = this_chisq
  rsquare[start_index:end_index] = this_rsquare
  yfit[start_index:end_index] = this_yfit
  status[start_index:end_index] = this_status
  dof[start_index:end_index] = this_dof
  estimation_error[start_index:end_index] = MEAN([1./m-1./(m+merror), 1./(m-merror)-1./m])
END

PRO identify_dispersion_for_data, direction, time_avg, energy_peak, inverse_v, estimated_distance, chisq, yfit, status, dof, estimation_error, n_dispersions, inverse_v_error,energy_peak_names,  ps_plot = ps_plot, idl_plot = idl_plot, output_folder = output_folder, dispersion_list = dispersion_list,rsquare = rsquare
;  IF ~KEYWORD_SET(continuous_time) THEN continuous_time = 10. * 60.
  n_time = N_ELEMENTS(energy_peak)

  energy_peak_change = [energy_peak[0:(n_time-2)] - energy_peak[1:(n_time-1)], !VALUES.F_NAN]

  energy_peak_change_flag = energy_peak_change

  index = WHERE(ROUND(energy_peak_change) LT 0 AND ROUND(energy_peak_change) GT -4.e5, ct)
  IF ct GT 0 THEN energy_peak_change_flag[index] = !VALUES.F_NAN

  energy_peak_change_flag = energy_peak_change_flag < 1
  
  continuous_number = 4. 
  continuity_flag = FLTARR(n_time- continuous_number)
  
  FOR icount = 0,  continuous_number-2 DO continuity_flag = continuity_flag + energy_peak_change_flag[(icount):(n_time-1- continuous_number+icount)]
  
  dispersion_flag = FLTARR(n_time)
  index = WHERE(continuity_flag GE  continuous_number-2, ct)
  IF ct GT 0 THEN FOR icount = 0,  continuous_number-1 DO dispersion_flag[index+icount] = 1

  index = WHERE(dispersion_flag EQ 0,ct)
  IF ct GT 0 THEN BEGIN
     energy_peak[index] = !VALUES.F_NAN
     inverse_v[index] = !VALUES.F_NAN
     IF KEYWORD_SET(inverse_v_error) THEN inverse_v_error[index, *] = !VALUES.F_NAN
  ENDIF

  itime = 0
  start_index = 0
  end_index = 0
  estimated_distance = DBLARR(n_time, /NOZERO)
  estimated_distance(*) = !VALUES.F_NAN
  chisq = DBLARR(n_time,/NOZERO)
  chisq(*) =  !VALUES.F_NAN
  rsquare = DBLARR(n_time,/NOZERO)
  rsquare(*) =  !VALUES.F_NAN
  yfit = DBLARR(n_time,/NOZERO)
  yfit(*) =  !VALUES.F_NAN
  status = DBLARR(n_time,/NOZERO)
  status(*) =  !VALUES.F_NAN
  dof = DBLARR(n_time,/NOZERO)
  dof(*) =  !VALUES.F_NAN
  estimation_error = DBLARR(n_time,/NOZERO)
  estimation_error(*) = !VALUES.F_NAN
  n_dispersions = DBLARR(n_time, /NOZERO)
  n_dispersions(*) = !VALUES.F_NAN
  n_dispersion = 0

  WHILE itime LT n_time DO BEGIN 
     IF FINITE(inverse_v[itime]) THEN BEGIN
        start_index = itime 
        n_dispersion++
        WHILE start_index NE 0 DO BEGIN
           IF ~FINITE(inverse_v[itime]) THEN BEGIN  
              end_index = itime - 1

              process_dispersion,  time_avg, inverse_v, inverse_v_error, start_index, end_index, estimated_distance, chisq, yfit, status, dof, estimation_error, direction, dispersion_list, energy_peak_names, ps_plot = ps_plot, idl_plot = idl_plot, output_folder = output_folder, rsquare = rsquare
             
              n_dispersions[start_index:end_index] = n_dispersion

              start_index = 0
              end_index = 0
           ENDIF ELSE BEGIN
              IF itime GT start_index AND inverse_v[itime] LT inverse_v[itime-1] THEN BEGIN 
                 end_index = itime - 1
                 process_dispersion,  time_avg, inverse_v, inverse_v_error, start_index, end_index, estimated_distance, chisq, yfit, status, dof, estimation_error, direction, dispersion_list, energy_peak_names, ps_plot = ps_plot, idl_plot = idl_plot, output_folder = output_folder, rsquare = rsquare
;                 fit_dispersion, direction, time_avg, inverse_v, start_index, end_index, estimated_distance, chisq, yfit, status, dof, estimation_error, inverse_v_error = inverse_v_error, ps_plot = ps_plot, output_folder = output_folder, dispersion_list = dispersion_list, energy_peak_names = energy_peak_names

                 n_dispersions[start_index:end_index] = n_dispersion

                 start_index = 0
                 end_index = 0
                 itime = itime - 1
              ENDIF ELSE itime++
           ENDELSE 
        ENDWHILE
     ENDIF
     itime++
  ENDWHILE

END


PRO identify_dispersions, direction, epcut_beam_name, dispersion_name, beam_inverse_v_name, dispersion_inverse_v_name, estimated_dist_name, estimated_dist_error_name,  dispersion_inverse_v_fitting_name, dispersion_inverse_v_fitting_chisq_name, dispersion_inverse_v_fitting_status_name, dispersion_inverse_v_fitting_dof_name, dispersion_n_name, energy_peak_names, ps_plot = ps_plot, idl_plot = idl_plot, output_folder = output_folder, dispersion_list = dispersion_list, rsquare = rsquare, dispersion_inverse_v_fitting_rsquare_name = dispersion_inverse_v_fitting_rsquare_name
  
  earth_radius = 6371.
;  continuous_time = 20. * 60
  
  get_data, epcut_beam_name, data = data
  time_avg = data.x
  epcut_beam = data.y

  get_data, beam_inverse_v_name, data = data
  beam_inverse_v = data.y
  inverse_v_error = data.dy
  
  identify_dispersion_for_data, direction,time_avg, epcut_beam, beam_inverse_v, estimated_dist, chisq, yfit, status, dof, estimation_error, n_dispersions, inverse_v_error, energy_peak_names,  ps_plot = ps_plot, idl_plot = idl_plot, output_folder = output_folder, dispersion_list = dispersion_list,rsquare = rsquare
  stop
  store_data, dispersion_name, data = {x:time_avg, y:epcut_beam}

  store_data, dispersion_inverse_v_name, data = {x:time_avg, y:beam_inverse_v, dy:inverse_v_error}
  
  store_data, dispersion_inverse_v_fitting_name, data = {x:time_avg, y:yfit}

  store_data, dispersion_inverse_v_fitting_chisq_name, data = {x:time_avg, y:chisq}

  store_data, dispersion_inverse_v_fitting_status_name, data = {x:time_avg, y:status}

  store_data, dispersion_inverse_v_fitting_dof_name, data = {x:time_avg, y:dof}

  store_data, dispersion_inverse_v_fitting_rsquare_name, data = {x:time_avg, y:rsquare}
  
  store_data, estimated_dist_name, data = {x:time_avg, y:estimated_dist/earth_radius}
  
  store_data, estimated_dist_error_name, data = {x:time_avg, y:estimation_error/earth_radius}

  store_data, dispersion_n_name, data = {x:time_avg, y:n_dispersions}

  options, dispersion_inverse_v_fitting_name, 'color', 2
  options, dispersion_inverse_v_name, 'psym', 7
  
END 
