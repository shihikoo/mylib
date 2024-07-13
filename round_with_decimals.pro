FUNCTION round_with_decimals, numb, n_decimals

  IF ~FINITE(numb) THEN RETURN, 0
  output = round(numb * 10.^n_decimals)/10.^n_decimals
  RETURN, output
END
