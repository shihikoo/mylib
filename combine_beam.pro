; purpose: combine parallel pitch angle beam with antiparallel pitch
;angle beam
;written by Jing Liao

PRO combine_beam, all_tplot_names, start_time = start_time, END_time = END_time, average_time = average_time

;Load data into arries
; pap beam
get_data,  all_tplot_names.parallel_pap_beam_name, data = data, dlim = dlim, lim = lim

time_avg = data.x
n_avg = N_ELEMENTS(time_avg)
n_pa_bin = N_ELEMENTS(data.y(0,*))
flux_para_half = data.y(*, 0:(n_pa_bin/2-1))
pap_para_half = data.v(*, 0:(n_pa_bin/2-1))

IF KEYWORD_SET(dlim) THEN  get_data,  all_tplot_names.antiparallel_pap_beam_name, data = data ELSE   get_data, all_tplot_names.antiparallel_pap_beam_name, data = data,  dlim = dlim, lim = lim
flux_anti_half = data.y(*, (n_pa_bin/2):(n_pa_bin-1))
pap_anti_half = data.v(*, (n_pa_bin/2):(n_pa_bin-1))

;set the arraies
flux_c = DBLARR(n_avg, n_pa_bin)
pap_c = DBLARR(n_avg, n_pa_bin)

; 2nd combine : combine tail and earth pap result into one
flux_c(*,  0:(n_pa_bin/2-1)) = flux_para_half
flux_c(*, (n_pa_bin/2):(n_pa_bin-1)) = flux_anti_half
pap_c(*, 0:(n_pa_bin/2-1)) = pap_para_half
pap_c(*, (n_pa_bin/2):(n_pa_bin-1)) = pap_anti_half

;save the combined data into string
str = {x:time_avg, y:flux_c, v:pap_c, start_time:start_time, END_time:end_time, average_time:average_time}
store_data,  all_tplot_names.pap_beam_combine_name, data = str, dlim = dlim, lim = lim
options,  all_tplot_names.pap_beam_combine_name, 'ytitle', 'COMBINED!C!CPitch Angle!CBeams'


END 
