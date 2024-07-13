function convert_structure_to_matrix, str_data, header = header
;compile_opt idl2
  tagnames = tag_names(str_data)
  ntime = n_elements(str_data.(0))
  nterm = n_elements(tagnames)

  output = dblarr(ntime, nterm)
  header = tagnames

  for iterm = 0, nterm - 1 do  output[*, iterm] = str_data.(iterm)
  
  return, output
end 
