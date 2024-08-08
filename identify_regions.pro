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

pro identify_regions, h1_density_name, h1_velocity_t_name, x_gse_name, y_gse_name, z_gsm_name, beta_name, l_name, region_name
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

  ; -- lobe / boundary layer / plasma sheet regions with beta

  ; -- combine region information into one string
  region = magnetosphere_region * 1. + (sheath_region or dayside_polar_region or high_latitude_region or near_Earth) * 10. + solar_wind_region * 100.

  index = where(~finite(beta), ct)
  if ct gt 0 then region[index] = !values.f_nan

  bl_region_index = where((region eq 1) and (((r gt 15) and (beta le 1 and beta gt 0.05)) or ((r le 15) and (beta le exp(0.14 * r - 2.1))) and beta gt 0.05), ct)
  if ct gt 0 then region[bl_region_index] = region[bl_region_index] + 1

  ps_region_index = where((region eq 1) and (((r gt 15) and (beta gt 1)) or ((r le 15) and (beta gt exp(0.14 * r - 2.1)))), ct)
  if ct gt 0 then region[ps_region_index] = region[ps_region_index] + 2

  ; -- store the region information into tplot string
  str = {x: time_avg, y: region}
  lim = {yrange: [0.1, 120], ylog: 1, psym: 10}
  store_data, region_name, data = str, lim = lim
end