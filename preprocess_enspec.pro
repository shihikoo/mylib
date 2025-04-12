;-----------------------------------------------------------------------
;Purpose: preprocess energy spectra
;Description: This function preprocess energy spectra:
;             1. validate the energy spectra
;             2. average the energy spectra with given average time
;             3. filter the energy spectra with region
;             4. filter the energy spectra with low count cut
;
;Input:   flux_name              : tplot name of diff flux - energy spectrum
;         counts_name            : tplot name of counts - energy spectrum
;         t_s                    : start of the time
;         t_e                    : end of the time
;         average_time           : the average time
;         pa_range               : pitch angle range matching flux_name
;         bx_name                : magentic field x component
;         x_gse_name             : tplot name of the empemeris x component
;         z_gsm_name             :  tplot name of the empemeris z component in gsm
;         low_count_line        : statistics threshold in counts
;         plot_low_count_filter
;           low_count_filtername
; Created by Jing Liao
; Created on 04/26/2022
;-----------------------------------------------------------------------
pro preprocess_enspec, sat, specie, flux_name, counts_name,  average_time, time_avg  $   
                       , magnetosphere_region_name $
                       , t_s = t_s, t_e = t_e $
                       , low_count_line = low_count_line, plot_low_count_filter = plot_low_count_filter, plot_path= plot_path

  COMMON SHARE1,ENERGY_BINS, DENERGY_BINS, PA_BINS, ERROR_MESSAGE
  
; Keywords handling
  IF ~KEYWORD_SET(t_s) OR ~KEYWORD_SET(t_e) THEN BEGIN
     get_timespan, interval
     t_s = interval[0]
     t_e = interval[1]
  ENDIF

  IF ~KEYWORD_SET(low_count_line) THEN low_count_line = 800.

  ts = EXTRACT_DATE_STRING(time_string(t_s))  
  te = EXTRACT_TIME_STRING(time_string(t_e))
  low_count_filename = plot_path + 'low_count_filter/' + 'low_count_filter_' + ts + '_' + te + '.ps'

;---------------------------------------------------------------------------------------------
; Validate the energy spectra data for the calculation time period. Keep data between t_s and t_e and save them again in original names  
; Validate the energy spectra. If not valid, record the error_message in the log and return
;-----------------------------------------------------------------------------------------------
  validate_enspec_tplot, flux_name, t_s, t_e, average_time,  error_message = error_message
  validate_enspec_tplot, counts_name, t_s, t_e, average_time, error_message = error_message
  
  IF error_message NE ''  THEN RETURN

;------------------------------------------------------------------------------------
; Average the energy spectra flux and counts/eflux
;------------------------------------------------------------------------------------
; Average the flux data into new name and sumup the counts data into new name
  average_tplot_variable_with_given_time, flux_name, average_time, time_avg          
  average_tplot_variable_with_given_time, counts_name, average_time, time_avg, /sumup 
  
  flux_avg_name = flux_name 
  counts_avg_name = counts_name 

; filter energy spectra with regions.  
  filter_spectra_with_regions, flux_avg_name, magnetosphere_region_name
  filter_spectra_with_regions, counts_avg_name, magnetosphere_region_name
  
; Clean up the low counts data in flux data, for energy data in all dirctions
  tplot_names, flux_name+'_Original',names=names
  if names[0] eq '' then filter_enspec, counts_avg_name, flux_avg_name, low_count_line, plot_low_count_filter = plot_low_count_filter, filename = low_count_filename

end
