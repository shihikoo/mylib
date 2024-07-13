;----------------------------------------------------------------
; Purpose: Read daily csv data files within the requested time interval into a
; matrix
; Inputs: jd_s, ndays, data_path
;
; Written by Jing Liao
; Written on 04/21/20211
;---------------------------------------------------------------
FUNCTION read_daily_csv_into_matrix, jd_s, ndays, data_path, avoid_compression_time = avoid_compression_time, avoid_2019 = avoid_2019, datatype = datatype, only_no_compress = only_no_compress

  FOR iday = 0l, ndays-1 DO BEGIN          ; Loop trough all days   
     caldat, jd_s + iday, month, day, year ; find caledar date
     
     month_str = string(month, format = '(i2.2)')
     day_str = string(day, format = '(i2.2)')
     year_str = string(year, format = '(i4.4)')
     fln = data_path + 'storm_o_beam_'+year_str+month_str+day_str +'_'+ datatype +'.csv'

     start_date_double = time_double(year_str+'-'+month_str+'-'+day_str)
     
;     if keyword_set(avoid_compression_time) and start_date_double ge time_double('2019-04-17') and start_date_double le time_double('2019-08-17') then continue
     if keyword_set(avoid_2019) and start_date_double ge time_double('2019-01-01') and start_date_double le time_double('2019-12-31') then continue
     if keyword_set(only_no_compress) and (start_date_double lt time_double('2018-05-26') or start_date_double gt time_double('2018-09-26')) and (start_date_double lt time_double('2019-04-17') or start_date_double gt time_double('2019-08-17'))   then continue
     
     names = FINDFILE(fln, count = ct)
     IF ct EQ 1 THEN BEGIN
        idata_structured = READ_CSV(names[0], HEADER = header)
        
        nterm = N_ELEMENTS(header)
        tagnames = tag_names(idata_structured)
        
        n_avg = N_ELEMENTS(idata_structured.(0))
        
        idata = DBLARR(nterm, n_avg)
        FOR ifield = 0, nterm-1 DO BEGIN 
           str_element, idata_structured, tagnames[ifield], value, success = found
           IF found THEN idata[ifield,*] = value ELSE stop
        ENDFOR
        IF KEYWORD_SET(data) THEN data = [[data],[idata]] ELSE data = idata
     ENDIF    
  ENDFOR

  if keyword_set(only_no_compress) then begin
     index = where(data[0,*] ge time_double('2019-04-17') and data[0,*] le time_double('2019-08-17'),ct)
     if ct gt 0 then data_2019 = data[*,index]

     average_time = data[0,1]-data[0,0]
     at_str = STRCOMPRESS(ROUND(average_time),  /REMOVE_ALL)
     
     modified_filename = 'data/hpca_science_mode_modified'
     averaged_filename = 'data/hpca_science_mode_modified_averaged_AVG'+at_str
     varname = 'mms1_hpca_science_mode_modified'
     averaged_varname = 'mms1_hpca_science_mode_modified_AVG'+at_str
     
     PRINT, FINDFILE(averaged_filename+'.tplot.gz', COUNT = ct_tplot_gz)
     IF ct_tplot_gz THEN spawn,'gzip -df ' + averaged_filename + '.tplot.gz'
     PRINT, FINDFILE(averaged_filename+'.tplot', COUNT = ct_tplot)
     IF ct_tplot EQ 0 THEN BEGIN     
        PRINT, FINDFILE(modified_filename+'.tplot.gz', COUNT = ct_tplot_gz)
        IF ct_tplot_gz THEN spawn,'gzip -df ' + modified_filename + '.tplot.gz'
        PRINT, FINDFILE(modified_filename+'.tplot', COUNT = ct_tplot)
        if ct_tplot eq 0 then stop
        tplot_restore, filenames = modified_filename+'.tplot'

        average_tplot_variable_with_given_time, varname, average_time, data[0,*],/new
        tplot_save, averaged_varname, file =  averaged_filename
     endif else tplot_restore, filenames = averaged_filename+'.tplot'
     
     get_data, averaged_varname, data = mode_avg_data
     if datatype eq 'external' then begin
        index_good = where(mode_avg_data.y gt 4,ct)
        match2, reform(data[0,*]), mode_avg_data.x[index_good], suba,subb
        index = where(suba ne -1, ct)
     endif else begin
        if datatype eq 'beam' then begin
           index_good = where(mode_avg_data.y gt 4,ct)
           match2, reform(data[0,*]), mode_avg_data.x[index_good], suba,subb
           index = where(suba ne -1, ct)
        endif else stop
     endelse 
     
     if ct gt 0 then data_2018 = data[*, index]

     if keyword_set(data_2019) and keyword_set(data_2018) then data = [[data_2018],[data_2019]] else if keyword_set(data_2019) then data = data_2019 else if keyword_set(data_2018) then data = data_2018 else stop
     
  endif 
  
  ntime = N_ELEMENTS(data[0,*])
  IF ntime EQ 0 THEN BEGIN 
     print, 'no files found' 
     RETURN,0
  ENDIF 
  output =  {header: header, data:data}
  RETURN, output
END 

;-----------------------------------------------------------------------------------------
; Purpose: extract a column of data out from daily data matrix for the given column_name
; Inputs: data, headers, column_name
; Output: an array
;
; Written by Jing Liao
; Written on 04/21/2021
;-----------------------------------------------------------------------------------------
FUNCTION extract_column_from_matrix, data, header, column_name
  index = where(header EQ column_name, ct)
  IF ct GT 1 OR ct EQ 0 THEN BEGIN
     print, 'Error: no time column store'
     stop
  ENDIF 
  output = REFORM(data[index,*])
  
  RETURN,output
END 

;----------------------------------------------------------------------------------------
;Purpose: convert data matrix into a structure data with column name
;as tags
;
;Inputs: header, data
;----------------------------------------------------------------------------------------
FUNCTION convert_daily_data_matrix, data_matrix, header

  FOR icolumn = 0, N_ELEMENTS(header)-1 DO BEGIN
     column_name = header[icolumn]
     column_data = extract_column_from_matrix(data_matrix, header, column_name)

     IF ~KEYWORD_SET(data) THEN data = CREATE_STRUCT(column_name, column_data) $
     ELSE data=CREATE_STRUCT(data, column_name, column_data)
;struct_add_field, data, column_name, column_data
  ENDFOR
  
  RETURN, data
END

;----------------------------------------------------------------------------------------
;Purpose clean up data matrix
;Inputs: data, header
;Output: data
;---------------------------------------------------------------------------------------
FUNCTION clean_up_daily_data_matrix, data, header, ts, te
;-- convert all data into doubles
  data = DOUBLE(data)

;-- keep only the data within the range
  index_column = WHERE(header EQ 'Time', ct)
  if ct gt 0 then begin 
     index_row = WHERE(data[index_column,*] GE ts and data[index_column,*] LE te, ct)
     IF ct GT 0 THEN data = data[ *,index_row]
  endif
  
;-- set all infinite flag value to nan
  index_column = WHERE(header EQ 'Flag_para', ct)
  if ct gt 0 then begin 
     index_row = WHERE( ~FINITE(data[index_column[0],*]), ct)
     IF ct GT 0 THEN data[index_column[0],index_row] = !VALUES.F_NAN
  endif
  
  index_column = WHERE(header EQ 'Flag_anti', ct)
  if ct gt 0 then begin
     index_row = WHERE( ~FINITE(data[index_column,*]), ct)
     IF ct GT 0 THEN data[index_column, index_row] = !VALUES.F_NAN
  endif
  
;-- require beta is valid
  index_column = WHERE(header EQ 'Beta', ct)
  if ct gt 0 then begin
     index_row = WHERE(FINITE(data[index_column,*]), ct )
     IF ct GT 0 THEN data = data[ *,index_row]
  endif
  
;-- region needs to be within magnetosphere
  index_column = WHERE(header EQ 'Region', ct)
  if ct gt 0 then begin 
     index_row = WHERE(data[index_column,*] LE 3, ct)
     IF ct GT 0 THEN data = data[*, index_row]
  endif
  
;-- Distance larger than 100 Re is placeholder for error data
  index_column = WHERE(header EQ 'DIST', ct)
  if ct gt 0 then begin 
     index_row = WHERE(data[index_column, *] GT 100., ct )
; This is a special index system. For some reason data[index_column,
; index_row] gives errors
     IF ct GT 0 THEN data[index_column, index_row, 0] = !VALUES.F_NAN
  endif
  RETURN, data
END 

; Region correction
FUNCTION correct_regions, data, header
   index_region = WHERE(header EQ 'Region', ct)
   index_hp = WHERE(header EQ 'H_p', ct)
   index_op = WHERE(header EQ 'O_p', ct)
   index_x = WHERE(header EQ 'GSM_X', ct)
   index_ygsm = WHERE(header EQ 'GSM_Y', ct)

   index = where(data[index_x,*] gt -10 AND ABS(data[index_ygsm,*]) lt 10 AND (data[index_op, *]+data[index_hp, *]) gt 0.02 AND data[index_region,*] eq 1, ct)
   
   if ct gt 0 then data[index_region[0], index] = 2

  RETURN, data

  END 

;----------------------------------------------------------------------------------------
;Purpose: Read daily csv data 
;Inputs: data, header
;Output: data
;---------------------------------------------------------------------------------------
FUNCTION read_daily_csv, jd_s, ndays, data_path,ts,te, avoid_compression_time = avoid_compression_time, avoid_2019 = avoid_2019, only_no_compress = only_no_compress, datatype = datatype
  raw_data = read_daily_csv_into_matrix(jd_s, ndays, data_path, avoid_compression_time = avoid_compression_time, avoid_2019 = avoid_2019, only_no_compress = only_no_compress, datatype = datatype)
  data_matrix = raw_data.data
  header = raw_data.header
  
  data_matrix = clean_up_daily_data_matrix(data_matrix, header, ts, te)
  data_matrix = correct_regions(data_matrix, header)
  data = convert_daily_data_matrix(data_matrix, header)

  output =  {data:data,header:header}
  return,output
END

;---------------------------------------------------------------------------------------------------------
; Purpose: Read the daily data and store into structure data
;
; Created by Jing Liao
; Created on 04/13/2021
;---------------------------------------------------------------------------------------------------------
pro read_daily_data_multi, time_start, time_end, tplot_path, data_path, read_from_dat = read_from_dat,store_tplot=store_tplot, avoid_compression_time = avoid_compression_time, avoid_2019 = avoid_2019, only_no_compress = only_no_compress

;--- set the time info ---
  ts = time_double(time_start) &  te = time_double(time_end) & dt = te-ts

  timespan, time_start, dt,  /SECONDS

  ts_str = time_struct(ts) 
  te_str = time_struct(te)      ; time structure   
  jd_s = julday(ts_str.month, ts_str.date, ts_str.year) 
  jd_e = julday(te_str.month, te_str.date, te_str.year) ; julian day

  time_str = strcompress(ts_str.year, /remove_all) + string(ts_str.month, format = '(i2.2)') + string(ts_str.date, format = '(i2.2)') + '_to_' +  $
             strcompress(te_str.year, /remove_all)  +string(te_str.month, format = '(i2.2)') + string(te_str.date, format = '(i2.2)')
; number of days to be loaded, it starts from the start day and end
; with the ending date, unless the ending time has 00 on hours,
; minutes and seconds, then ending on the date before endign date
  ndays = (jd_e - jd_s) + 1     
; Last day is not included if hour=min=sec=0
  IF te_str.hour EQ 0 AND te_str.min EQ 0 AND te_str.sec EQ 0 THEN ndays = ndays - 1

;--------------------------------------------------------------
; Read from prestore data if tplot file exists and keyword
; read_from_dat set to 0
;--------------------------------------------------------------
  fln_saved_tplot = tplot_path + 'fulldata_' + time_str
  
  PRINT, FINDFILE(fln_saved_tplot+'.tplot', COUNT = ct_tplot)
  IF ct_tplot GT 0 AND NOT  KEYWORD_SET(read_from_dat)  THEN BEGIN 
     TPLOT_RESTORE, filenames = fln_saved_tplot+'.tplot' 
     GET_DATA, 'data', data = saved_data
     data = saved_data.data
  ENDIF ELSE BEGIN
     external_data = READ_DAILY_CSV(jd_s, ndays, data_path,ts,te, avoid_compression_time = avoid_compression_time , avoid_2019 = avoid_2019, only_no_compress = only_no_compress,datatype='external')
     
     beam_data = READ_DAILY_CSV(jd_s, ndays, data_path,ts,te, avoid_compression_time = avoid_compression_time , avoid_2019 = avoid_2019, only_no_compress = only_no_compress,datatype='beam')
     ;dispersion_data = READ_DAILY_CSV(jd_s, ndays, data_path,ts,te, avoid_compression_time = avoid_compression_time , avoid_2019 = avoid_2019, only_no_compress = only_no_compress,datatype=datatype)
     
     combined_data = combine_data(external_data.data, beam_data.data, type= 'external_data')
     data = calculate_daily_data(combined_data)

     combined_data_full = combine_data(external_data.data, beam_data.data, type= 'beam_data')
     fulldata = calculate_daily_data(combined_data_full)
     
     IF KEYWORD_SET(store_tplot) THEN BEGIN
        STORE_DATA, 'data', data = {data:data, fulldata: fulldata}
;        STORE_DATA, 'fulldata', data = {data:fulldata}
        SPAWN, 'mkdir -p '+ FILE_DIRNAME(fln_saved_tplot)
        TPLOT_SAVE, 'data', filename = fln_saved_tplot
        header = TAG_NAMES(data)
        WRITE_CSV, fln_saved_tplot+'.csv', data, HEADER = header

      ;   WRITE_CSV, fln_saved_tplot+'.csv', fulldata, HEADER = header

     ENDIF  
  ENDELSE

END
