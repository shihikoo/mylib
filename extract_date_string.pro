FUNCTION extract_date_string, ts       
  date_s = STRMID(ts, 0, 4) + STRMID(ts, 5, 2) + STRMID(ts, 8, 2)                                                                                                                         
  RETURN,date_s                                                                                                                                                                         
END


