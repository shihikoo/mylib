; ---------------------------------------------------------------------
; Purpose: Identify different regions and save in tplot var 'location'. Considering the orbits of RBSP, we divide the regions we encounter into: sheath, PS, PSBL, Lobe,
;
; Description: The definitions of the regions are defined by observing the maps and data.
; 1. Sheath is defined as
; H+ density gt 3 and H+ velocity gt 65
; or x_gse greater than 1 and ABS(z_gsm) greater than 5 and beta gt 0.05
; or x_gse less than 1 and ABS(z_gsm) greater than 10 and beta gt 1
; 2. Dayside polar region is defined as:
; x_gse gt 1 Re and ABS(z_gsm) gt 5 Re and beta gt 0.05
; 3. High latitude region is defined as:
; x_gse le 1 Re and ABS(z_gsm) gt 10 Re and beta gt 1
; 4. Solar wind region is defined as:
; x_gse gt -1 Re and and outside an ellipse decided by observing the XY gse map:
; (x_gse+1)^2/8.^2+(y_gse-1)^2/14.^2)
; 5. All other regions are considered magnetosphere.
;
; Inputs:      beta_name, h_density_name, h_velocity_name, x_gse_name,
; y_gse_name, z_gsm_name, location_name
;
; Created by J. Liao
; Created on 06/20/2023
; ------------------------------------------------------------------------

pro identify_regions, h1_density_name, h1_velocity_t_name, x_gse_name, y_gse_name, z_gsm_name, beta_name, l_name, region_name, op_name = op_name, hp_name = hp_name, y_gsm_name = y_gsm_name
  ; -- load data into arrays
  h_density = r_data(h1_density_name, /y)
  h_velocity = r_data(h1_velocity_t_name, /y)

  get_data, x_gse_name, data = data
  time_avg = data.x

  x_gse = data.y
  y_gse = r_data(y_gse_name, /y)
  z_gsm = r_data(z_gsm_name, /y)

  r = sqrt(x_gse ^ 2 + y_gse ^ 2)

  beta = r_data(beta_name, /y)

  l = r_data(l_name, /y)

  if keyword_set(op_name) then op = r_data(op_name, /y)
  if keyword_set(hp_name) then hp = r_data(hp_name, /y)
  if keyword_set(y_gsm_name) then y_gsm = r_data(y_gsm_name, /y)

  ; -- near-Earth --
  near_Earth = l le 1.5

  ; -- sheath, dayside polar region and high latitude region
  sheath_region = h_density gt 3 and h_velocity gt 65
  dayside_polar_region = x_gse gt 1 and abs(z_gsm) gt 5 and beta gt 0.05
  high_latitude_region = x_gse le 1 and abs(z_gsm) gt 10 and beta gt 1

  ; -- solar wind region
  solar_wind_region = x_gse gt -1 and ((x_gse + 1) ^ 2 / 8. ^ 2 + (y_gse - 1) ^ 2 / 14. ^ 2) gt 1

  ; -- magnetosphere region is defined the region not defined as any of above
  magnetosphere_region = (sheath_region or dayside_polar_region or high_latitude_region or solar_wind_region) eq 0

  ; -- combine region information into one string region eq 1 is magentosphere.
  region = magnetosphere_region * 1. + (sheath_region or dayside_polar_region or high_latitude_region or near_Earth) * 10. + solar_wind_region * 100.

  index = where(~finite(beta), ct)
  if ct gt 0 then region[index] = !values.f_nan

  ; -- lobe / boundary layer / plasma sheet regions
  ; The settings of the beam identification program are different for different regions, for handling the differences in the eneryg and pitch angle distribution of the beam. The main purpose of the dividing the regions into lobe, boundary layer and plasma sheet for the identificaiton program.
  ; More detailed region idientificiton will be done later during the data aggregation process.

  ; lobe region is recorded as 1. No step here.
  ; lobe_region_index = where(region eq 1 and beta le 0.05, ct)
  ; if ct gt 0 then region[lobe_region_index] = 1

  ; psbl region. The definition is different for distande r=sqrt(x^2+y^2) further and closther than 15 Re.
  bl_region_index = where(region eq 1 and ((r gt 15 and beta le 1 and beta gt 0.05) or (r le 15 and beta le exp(0.14 * r - 2.1) and beta gt 0.05)), ct)
  if ct gt 0 then region[bl_region_index] = 2

  ; ps region. The definition is different for distande r=sqrt(x^2+y^2) further and closther than 15 Re.
  ps_region_index = where(region le 2 and ((r gt 15 and beta gt 1) or (r le 15 and beta gt exp(0.14 * r - 2.1))), ct)
  if ct gt 0 then region[ps_region_index] = 3

  ; some of the near Earth lobe are actually psbl. The beta creteiria is not good enough for near Earth area. Only run if O+ pressure and H+ pressure data are avaliable
  if keyword_set(op_name) and keyword_set(hp_name) and keyword_set(y_gsm_name) then begin
    index = where(region eq 1 and x_gse gt -10 and abs(y_gsm) lt 10 and (op + hp) gt 0.02, ct)
    if ct gt 0 then region[index] = 2
  endif
; stop
  ; -- store the region information into tplot string
  str = {x: time_avg, y: region}
  lim = {yrange: [0.1, 120], ylog: 1, psym: 10}
  store_data, region_name, data = str, lim = lim
end