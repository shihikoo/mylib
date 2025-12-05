; ----------------------------------------------------------------
; Purpose: Read daily csv data files within the requested time interval into a
; matrix
; Inputs: jd_s, ndays, data_path
;
; Written by Jing Liao
; Written on 04/21/20211
; ---------------------------------------------------------------
function read_daily_csv_into_matrix, jd_s, ndays, data_path, avoid_compression_time = avoid_compression_time, avoid_2019 = avoid_2019, datatype = datatype, only_no_compress = only_no_compress
  for iday = 0l, ndays - 1 do begin ; Loop trough all days
    caldat, jd_s + iday, month, day, year ; find caledar date

    month_str = string(month, format = '(i2.2)')
    day_str = string(day, format = '(i2.2)')
    year_str = string(year, format = '(i4.4)')
    fln = data_path + 'storm_o_beam_' + year_str + month_str + day_str + '_' + datatype + '.csv'

    start_date_double = time_double(year_str + '-' + month_str + '-' + day_str)

    ; if keyword_set(avoid_compression_time) and start_date_double ge time_double('2019-04-17') and start_date_double le time_double('2019-08-17') then continue
    if keyword_set(avoid_2019) and start_date_double ge time_double('2019-01-01') and start_date_double le time_double('2019-12-31') then continue
    if keyword_set(only_no_compress) and (start_date_double lt time_double('2018-05-26') or start_date_double gt time_double('2018-09-26')) and (start_date_double lt time_double('2019-04-17') or start_date_double gt time_double('2019-08-17')) then continue

    names = FINDFILE(fln, count = ct)
    if ct eq 1 then begin
      idata_structured = read_csv(names[0], header = header)

      nterm = n_elements(header)
      tagnames = tag_names(idata_structured)

      n_avg = n_elements(idata_structured.(0))

      idata = dblarr(nterm, n_avg)
      for ifield = 0, nterm - 1 do begin
        str_element, idata_structured, tagnames[ifield], value, success = found
        if found then idata[ifield, *] = value else stop
      endfor
      if keyword_set(data) then data = [[data], [idata]] else data = idata
    endif
  endfor

  if keyword_set(only_no_compress) then begin
    index = where(data[0, *] ge time_double('2019-04-17') and data[0, *] le time_double('2019-08-17'), ct)
    if ct gt 0 then data_2019 = data[*, index]

    average_time = data[0, 1] - data[0, 0]
    at_str = strcompress(round(average_time), /remove_all)

    modified_filename = 'data/hpca_science_mode_modified'
    averaged_filename = 'data/hpca_science_mode_modified_averaged_AVG' + at_str
    varname = 'mms1_hpca_science_mode_modified'
    averaged_varname = 'mms1_hpca_science_mode_modified_AVG' + at_str
  
    print, FINDFILE(averaged_filename + '.tplot.gz', count = ct_tplot_gz)
    if ct_tplot_gz then spawn, 'gzip -df ' + averaged_filename + '.tplot.gz'
    print, FINDFILE(averaged_filename + '.tplot', count = ct_tplot)
    if ct_tplot eq 0 then begin
      print, FINDFILE(modified_filename + '.tplot.gz', count = ct_tplot_gz)
      if ct_tplot_gz then spawn, 'gzip -df ' + modified_filename + '.tplot.gz'
      print, FINDFILE(modified_filename + '.tplot', count = ct_tplot)
      if ct_tplot eq 0 then stop
      tplot_restore, filenames = modified_filename + '.tplot'

      average_tplot_variable_with_given_time, varname, average_time, data[0, *], /new
      tplot_save, averaged_varname, file = averaged_filename
    endif else tplot_restore, filenames = averaged_filename + '.tplot'

    get_data, averaged_varname, data = mode_avg_data
    if datatype eq 'external' then begin
      index_good = where(mode_avg_data.y gt 4, ct)
      match2, reform(data[0, *]), mode_avg_data.x[index_good], suba, subb
      index = where(suba ne -1, ct)
    endif else begin
      if datatype eq 'beam' then begin
        index_good = where(mode_avg_data.y gt 4, ct)
        match2, reform(data[0, *]), mode_avg_data.x[index_good], suba, subb
        index = where(suba ne -1, ct)
      endif else stop
    endelse

    if ct gt 0 then data_2018 = data[*, index]

    if keyword_set(data_2019) and keyword_set(data_2018) then data = [[data_2018], [data_2019]] else if keyword_set(data_2019) then data = data_2019 else if keyword_set(data_2018) then data = data_2018 else stop
  endif

  ntime = n_elements(data[0, *])
  if ntime eq 0 then begin
    print, 'no files found'
    RETURN, 0
  endif
  output = {header: header, data: data}
  RETURN, output
end

; -----------------------------------------------------------------------------------------
; Purpose: extract a column of data out from daily data matrix for the given column_name
; Inputs: data, headers, column_name
; Output: an array
;
; Written by Jing Liao
; Written on 04/21/2021
; -----------------------------------------------------------------------------------------
function extract_column_from_matrix, data, header, column_name
  index = where(header eq column_name, ct)
  if ct gt 1 or ct eq 0 then begin
    print, 'Error: no time column store'
    stop
  endif
  output = reform(data[index, *])

  RETURN, output
end

; ----------------------------------------------------------------------------------------
; Purpose: convert data matrix into a structure data with column name
; as tags
;
; Inputs: header, data
; ----------------------------------------------------------------------------------------
function convert_daily_data_matrix, data_matrix, header
  for icolumn = 0, n_elements(header) - 1 do begin
    column_name = header[icolumn]
    column_data = extract_column_from_matrix(data_matrix, header, column_name)

    if ~keyword_set(data) then data = create_struct(column_name, column_data) $
    else data = create_struct(data, column_name, column_data)
    ; struct_add_field, data, column_name, column_data
  endfor

  RETURN, data
end

; ----------------------------------------------------------------------------------------
; Purpose clean up data matrix
; Inputs: data, header
; Output: data
; ---------------------------------------------------------------------------------------
function clean_up_daily_data_matrix, data, header, ts, te
  ; -- convert all data into doubles
  data = double(data)

  ; -- keep only the data within the range
  index_column = where(header eq 'Time', ct)
  if ct gt 0 then begin
    index_row = where(data[index_column, *] ge ts and data[index_column, *] le te, ct)
    if ct gt 0 then data = data[*, index_row]
  endif

  ; -- set all infinite flag value to nan
  index_column = where(header eq 'Flag', ct)
  if ct gt 0 then begin
    index_row = where(~finite(data[index_column[0], *]), ct)
    if ct gt 0 then data[index_column[0], index_row] = !values.f_nan
  endif

  index_column = where(header eq 'Flag_para', ct)
  if ct gt 0 then begin
    index_row = where(~finite(data[index_column[0], *]), ct)
    if ct gt 0 then data[index_column[0], index_row] = !values.f_nan
  endif

  index_column = where(header eq 'Flag_anti', ct)
  if ct gt 0 then begin
    index_row = where(~finite(data[index_column, *]), ct)
    if ct gt 0 then data[index_column, index_row] = !values.f_nan
  endif

  ; -- require beta is valid
  index_column = where(header eq 'Beta', ct)
  if ct gt 0 then begin
    index_row = where(finite(data[index_column, *]), ct)
    if ct gt 0 then data = data[*, index_row]
  endif

  ; -- region needs to be within magnetosphere
  index_column = where(header eq 'Region', ct)
  if ct gt 0 then begin
    index_row = where(data[index_column, *] le 3, ct)
    if ct gt 0 then data = data[*, index_row]
  endif

  ; -- Distance larger than 100 Re is placeholder for error data
  index_column = where(header eq 'DIST', ct)
  if ct gt 0 then begin
    index_row = where(data[index_column, *] gt 100., ct)
    ; This is a special index system. For some reason data[index_column,
    ; index_row] gives errors
    if ct gt 0 then data[index_column, index_row, 0] = !values.f_nan
  endif

  RETURN, data
end

; Region Refinement
; The region definition during the identification process are for idnetification setting purpose only.
; Here we assess the the region within the magentosphere again for mapping purpose
; -------------
function refine_regions, data, header
  index_region = where(header eq 'Region', ct)
  index_hp = where(header eq 'H_p', ct)
  index_op = where(header eq 'O_p', ct)
  index_x = where(header eq 'GSM_X', ct)
  index_ygse = where(header eq 'GSE_Y', ct)
  index_ygsm = where(header eq 'GSM_Y', ct)
  index_mlt = where(header eq 'MLT', ct)
  index_beta = where(header eq 'Beta', ct)

  r = sqrt(data[index_x, *] ^ 2 + data[index_ygse, *] ^ 2)
  ; lobe region the definition is beta < 0.05
  index = where(data[index_region, *] le 3 and data[index_beta, *] le 0.05, ct)
  if ct gt 0 then data[index_region[0], index] = 1

  ; psbl region the definition is different for distande r=sqrt(x^2+y^2) further and closther than 15 Re.
  index = where(data[index_region, *] eq 1 and ((r gt 15 and data[index_beta, *] le 1 and data[index_beta, *] gt 0.05) or (r le 15 and data[index_beta, *] le exp(0.14 * r - 2.1) and data[index_beta, *] gt 0.05)), ct)
  if ct gt 0 then data[index_region[0], index] = 2

  ; ps region. The definition is different for distande r=sqrt(x^2+y^2) further and closther than 15 Re.
  index = where(data[index_region, *] le 2 and ((r gt 15 and data[index_beta, *] gt 1) or (r le 15 and data[index_beta, *] gt exp(0.14 * r - 2.1))), ct)
  if ct gt 0 then data[index_region[0], index] = 3

  ; some of the near Earth lobe are actually psbl. The beta creteiria only is not good enough for near Earth area. Only run if O+ pressure and H+ pressure data are avaliable
  index = where(data[index_region, *] eq 1 and data[index_x, *] gt -10 and abs(data[index_ygsm, *]) lt 10 and (data[index_op, *] + data[index_hp, *]) gt 0.02, ct)
  if ct gt 0 then data[index_region[0], index] = 2

  ; Some of the lobe near the Earth should be PSBL
  index = where(data[index_x, *] gt -10 and abs(data[index_ygsm, *]) lt 10 and (data[index_op, *] + data[index_hp, *]) gt 0.02 and data[index_region, *] eq 1, ct)
  if ct gt 0 then data[index_region[0], index] = 2

  ; Define dayside
  index = where(data[index_region, *] lt 10 and data[index_mlt, *] ge 8 and data[index_mlt, *] le 16, ct)
  if ct gt 0 then data[index_region[0], index] = 4

  RETURN, data
end

; ----------------------------------------------------------------------------------------
; Purpose: Read daily csv data
; Inputs: data, header
; Output: data
; ---------------------------------------------------------------------------------------
function read_daily_csv, jd_s, ndays, data_path, ts, te, avoid_compression_time = avoid_compression_time, avoid_2019 = avoid_2019, only_no_compress = only_no_compress, datatype = datatype
  raw_data = read_daily_csv_into_matrix(jd_s, ndays, data_path, avoid_compression_time = avoid_compression_time, avoid_2019 = avoid_2019, only_no_compress = only_no_compress, datatype = datatype)
  data_matrix = raw_data.data
  header = raw_data.header

  data_matrix = clean_up_daily_data_matrix(data_matrix, header, ts, te)
  data_matrix = refine_regions(data_matrix, header)
  data = convert_daily_data_matrix(data_matrix, header)

  output = {data: data, header: header}
  return, output
end

; ---------------------------------------------------------------------------------------------------------
; Purpose: Read the daily data and store into structure data
;
; Created by Jing Liao
; Created on 04/13/2021
; ---------------------------------------------------------------------------------------------------------
pro read_daily_data_multi, time_start, time_end, tplot_path, data_path, read_from_dat = read_from_dat, store_tplot = store_tplot, avoid_compression_time = avoid_compression_time, avoid_2019 = avoid_2019, only_no_compress = only_no_compress

  ; --- set the time info ---
  ts = time_double(time_start)
  te = time_double(time_end)
  dt = te - ts

  timespan, time_start, dt, /seconds

  ts_str = time_struct(ts)
  te_str = time_struct(te) ; time structure
  jd_s = julday(ts_str.month, ts_str.date, ts_str.year)
  jd_e = julday(te_str.month, te_str.date, te_str.year) ; julian day

  time_str = strcompress(ts_str.year, /remove_all) + string(ts_str.month, format = '(i2.2)') + string(ts_str.date, format = '(i2.2)') + '_to_' + $
    strcompress(te_str.year, /remove_all) + string(te_str.month, format = '(i2.2)') + string(te_str.date, format = '(i2.2)')
  ; number of days to be loaded, it starts from the start day and end with the ending date, unless the ending time has 00 on hours, minutes and seconds, then ending on the date before endign date
  ndays = (jd_e - jd_s) + 1
  ; Last day is not included if hour=min=sec=0
  if te_str.hour eq 0 and te_str.min eq 0 and te_str.sec eq 0 then ndays = ndays - 1

  ; --------------------------------------------------------------
  ; Read from prestore data if tplot file exists and keyword
  ; read_from_dat set to 0
  ; --------------------------------------------------------------
  fln_saved_tplot = tplot_path + 'fulldata_' + time_str

  print, FINDFILE(fln_saved_tplot + '.tplot', count = ct_tplot)
  if ct_tplot gt 0 and not keyword_set(read_from_dat) then begin
    tplot_restore, filenames = fln_saved_tplot + '.tplot'
    get_data, 'data', data = saved_data
    data = saved_data.data
  endif else begin
    external_data = read_daily_csv(jd_s, ndays, data_path, ts, te, avoid_compression_time = avoid_compression_time, avoid_2019 = avoid_2019, only_no_compress = only_no_compress, datatype = 'external')

    beam_data = read_daily_csv(jd_s, ndays, data_path, ts, te, avoid_compression_time = avoid_compression_time, avoid_2019 = avoid_2019, only_no_compress = only_no_compress, datatype = 'beam')
    ; dispersion_data = READ_DAILY_CSV(jd_s, ndays, data_path,ts,te, avoid_compression_time = avoid_compression_time , avoid_2019 = avoid_2019, only_no_compress = only_no_compress,datatype=datatype)

    combined_data = combine_data(external_data.data, beam_data.data, type = 'external_data')
    data = calculate_daily_data(combined_data)

    combined_data_full = combine_data(external_data.data, beam_data.data, type = 'beam_data')
    fulldata = calculate_daily_data(combined_data_full)

    if keyword_set(store_tplot) then begin
      store_data, 'data', data = {data: data, fulldata: fulldata}
      ; STORE_DATA, 'fulldata', data = {data:fulldata}
      spawn, 'mkdir -p ' + file_dirname(fln_saved_tplot)
      tplot_save, 'data', filename = fln_saved_tplot
      header = tag_names(data)
      write_csv, fln_saved_tplot + '.csv', data, header = header

      ; WRITE_CSV, fln_saved_tplot+'.csv', fulldata, HEADER = header
    endif
  endelse
end