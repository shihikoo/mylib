;--------------------------------------------------------------
; merge external data into beam data
;--------------------------------------------------------------
function merge_external_to_beam, external_data, beam_data
  
  beam_matrix = convert_structure_to_matrix(beam_data, header = tagnames_beam)
  external_matrix = convert_structure_to_matrix(external_data, header = tagnames_external)

  n_term_external = n_elements(tagnames_external)
;  n_term_beam = n_elements(tagnames_beam)

  index = where(beam_data.flux_para gt 0 or beam_data.flux_anti gt 0, ct)
  output_beam = beam_matrix[index, *]
  
  index_time = where(tagnames_beam eq "TIME")
  index_en = where(tagnames_beam eq 'EN')
  time = output_beam[*,index_time]
  en =  output_beam[*,index_en]
  n_time = n_elements(time)
  
  unique_time = time[uniq(time, sort(time))]
  n_unique_time = n_elements(unique_time)
  
  output = dblarr(n_time, n_term_external)
  output[*] = !values.f_nan

  for itime = 0, n_unique_time-1 do begin
     index_beam = where(output_beam[*,index_time] eq unique_time[itime], ct_beam)
     index_external = where(external_data.Time eq unique_time[itime],ct_external)
     if ct_external gt 0 then begin
        output[index_beam,*] = transpose(cmreplicate(reform(external_matrix[index_external,*],n_term_external),ct_beam))
     endif else begin
        print,'no external data found'
        stop
     endelse 
  endfor
  
  index_time_external = where(tagnames_external eq "TIME")
  if index_time_external ne 0 then stop
  tagnames_external = tagnames_external[1:n_term_external-1]
  output = output[*, 1:n_term_external-1]
;  stop
  str_output = convert_matrix_to_structure([[output_beam], [output], [en], [en]], [tagnames_beam, tagnames_external, 'EN_PARA','EN_ANTI'])
  
  return, str_output
end 

;-------------------------------------------------------------
; Merge beam data into external data (need to be tested)
;-------------------------------------------------------------
function new_merge_beam_to_external, external_data, beam_data
  beam_matrix = convert_structure_to_matrix(beam_data, header = tagnames_beam)
  ; external_matrix = convert_structure_to_matrix(external_data, header = tagnames_external)

  ; n_term_external = n_elements(tagnames_external)
  n_term_beam = n_elements(tagnames_beam)

  index = where(beam_data.flux_para gt 0 or beam_data.flux_anti gt 0, ct)
  valid_beam_matrix = beam_matrix[index, *]
  
  index_time = where(tagnames_beam eq "TIME")
  time = external_data.time  
  n_time = n_elements(time)

  output = dblarr(n_time, n_term_beam)
  output[*] = !values.f_nan
  
  for itime = 0, n_time-1 do begin
     index = where(valid_beam_matrix[*,index_time] eq time[itime], ct)
     if ct gt 0 then output[itime, *] = total(valid_beam_matrix[index, *], 1,/nan)/ct
  endfor
  
  str_output = convert_matrix_to_structure(output, tagnames_beam)  
  
  return, str_output
end

;-------------------------------------------------------------
; Merge beam data into external data
;-------------------------------------------------------------
function merge_beam_to_external, external_data, beam_data
  time = external_data.time  
  n_time = n_elements(time)
;   tagnames_external = tag_names(external_data)
  tagnames_beam = tag_names(beam_data)
;   n_term_external = n_elements(tagnames_external)
  n_term_beam = n_elements(tagnames_beam)
  output = dblarr(n_time, n_term_beam+1)
  output[*] = !values.f_nan
  for itime = 0, n_time-1 do begin 
     en_para = !values.f_nan &  pa_para = !values.f_nan &  flux_para = !values.f_nan &  eflux_para = !values.f_nan & int_flux_para = !values.f_nan & pa_range_para = !values.f_nan & n_para = !values.f_nan & n_para_weighted = !values.f_nan & p_para = !values.f_nan & t_para = !values.f_nan;& v_para = !values.f_nan
     en_anti = !values.f_nan &  pa_anti = !values.f_nan &  flux_anti = !values.f_nan &  eflux_anti = !values.f_nan & int_flux_anti = !values.f_nan & pa_range_anti = !values.f_nan & n_anti = !values.f_nan  & n_anti_weighted = !values.f_nan & p_anti = !values.f_nan & t_anti = !values.f_nan; & v_anti = !values.f_nan
    
     index_time = where(beam_data.Time eq time[itime],ct_time)
     if ct_time gt 0 then begin 
        index = where(finite(beam_data.flux_para[index_time]), ct)
        if ct gt 0 then begin 
           ens = beam_data.en[index_time[index]]
           ens = ens[UNIQ(ens, SORT(ens))]
           en_para = total(ens)/N_ELEMENTS(ens)
           pas = beam_data.pa_para[index_time[index]]
           pas = pas[UNIQ(pas, SORT(pas))]
           pa_para = total(pas)/N_ELEMENTS(pas)
           flux_para = total(beam_data.flux_para[index_time[index]])/ct
           eflux_para = total(beam_data.eflux_para[index_time[index]])
           int_flux_para = total(beam_data.int_flux_para[index_time[index]])
           pa_range_para = total(beam_data.pa_range_para[index_time[index]])/ct
           n_para = total(beam_data.n_para[index_time[index]])
           n_para_weighted = total(beam_data.n_para_weighted[index_time[index]])

          ;  v_para = total(beam_data.v_para[index_time[index]])/ct
           p_para = total(beam_data.p_para[index_time[index]])
           t_para = total(beam_data.t_para[index_time[index]])/ct
        endif
        
        index = where(finite(beam_data.flux_anti[index_time,*]), ct)
        if ct gt 0 then begin
          ens = beam_data.en[index_time[index]]
          ens = ens[UNIQ(ens, SORT(ens))]
           en_anti = total(ens)/N_ELEMENTS(ens)
           pas = beam_data.pa_anti[index_time[index]]
           pas = pas[UNIQ(pas, SORT(pas))]
           pa_anti = total(pas)/N_ELEMENTS(pas)
           flux_anti = total(beam_data.flux_anti[index_time[index]])/ct
           eflux_anti = total(beam_data.eflux_anti[index_time[index]])
           int_flux_anti = total(beam_data.int_flux_anti[index_time[index]])
           pa_range_anti = total(beam_data.pa_range_anti[index_time[index]])/ct  
           n_anti = total(beam_data.n_anti[index_time[index]])
           n_anti_weighted = total(beam_data.n_anti_weighted[index_time[index]])

          ;  v_anti = total(beam_data.v_anti[index_time[index]])/ct
           p_anti = total(beam_data.p_anti[index_time[index]]) 
           t_anti = total(beam_data.t_anti[index_time[index]])/ct
        endif
        
        output[itime,0:n_term_beam] = [time[itime], en_para, pa_para, flux_para, eflux_para, int_flux_para, pa_range_para, n_para, n_para_weighted, p_para, t_para, en_anti, pa_anti, flux_anti, eflux_anti, int_flux_anti, pa_range_anti, n_anti, n_anti_weighted, p_anti, t_anti]
        
     endif 
  endfor
  
  str_output = external_data

  str_output =CREATE_STRUCT(str_output, 'en_para', output[*,1])
  str_output =CREATE_STRUCT(str_output, 'pa_para', output[*,2])
  str_output =CREATE_STRUCT(str_output, 'flux_para', output[*,3])
  str_output =CREATE_STRUCT(str_output, 'eflux_para', output[*,4])
  str_output =CREATE_STRUCT(str_output, 'int_flux_para', output[*,5])
  str_output =CREATE_STRUCT(str_output, 'pa_range_para', output[*,6])
  str_output =CREATE_STRUCT(str_output, 'n_para', output[*,7])
  str_output =CREATE_STRUCT(str_output, 'n_para_weighted', output[*,8])
  str_output =CREATE_STRUCT(str_output, 'p_para', output[*,9])
  str_output =CREATE_STRUCT(str_output, 't_para', output[*,10])

  str_output =CREATE_STRUCT(str_output, 'en_anti', output[*,11])
  str_output =CREATE_STRUCT(str_output, 'pa_anti', output[*,12])
  str_output =CREATE_STRUCT(str_output, 'flux_anti', output[*,13])
  str_output =CREATE_STRUCT(str_output, 'eflux_anti', output[*,14])
  str_output =CREATE_STRUCT(str_output, 'int_flux_anti', output[*,15])
  str_output =CREATE_STRUCT(str_output, 'pa_range_anti', output[*,16])
  str_output =CREATE_STRUCT(str_output, 'n_anti', output[*,17])
  str_output =CREATE_STRUCT(str_output, 'n_anti_weighted', output[*,18])
  str_output =CREATE_STRUCT(str_output, 'p_anti', output[*,19])
  str_output =CREATE_STRUCT(str_output, 't_anti', output[*,20])

  return, str_output
end 

function combine_data, external_data, beam_data, type = type
  if ~keyword_set(type) then type = 'beam_data'

  if type eq 'beam_data' then output = merge_external_to_beam(external_data, beam_data)

  if type eq 'external_data' then output = merge_beam_to_external(external_data, beam_data)
  
;   if type eq 'external_data' then new_output = new_merge_beam_to_external(external_data, beam_data)

  return, output
end 
