;--------------------------------------------------------------------
; Purpose: According to input property_name, load property data into property_para and property_anti,
; also load log, range, and unit
; Inputs: data, property_name, property_para, property_anti,
; property_v_log, property_v_range, property_unit
;-----------------------------------------------------------------------
PRO load_property_data, data, property_name,property_para, property_anti, property_v_log, property_v_range, property_unit, property_map_type = property_map_type

; -- Settings for graphing --
  energy_V_LOG = 1 & ENERGY_V_RANGE = [10, 4000.] & energy_unit = 'eV' & median_ENERGY_V_RANGE = [30,3000]   & minimum_ENERGY_V_RANGE   =[30,3000]
  flux_V_LOG = 1 & FLUX_V_RANGE = [1., 100.] & flux_unit = '1/cm!U-3!N-s-sr-(eV/e)'
  eflux_V_LOG = 1 & EFLUX_V_RANGE = [1000.,10000.] & eflux_unit = 'eV/cm!U-3!N-s-sr(eV/e)'
  density_v_log = 1 &  density_v_range = [0.001, 0.1] & density_unit = 'cm!U-3'
  velocity_v_log = 1 &  velocity_v_range = [20, 200] & velocity_unit = 'km/s'
  velocity_vpara_log = 1 &  velocity_vpara_range = [20, 200]
  velocity_vperp_log = 1 &  velocity_vperp_range = [20, 200]
  distfunc_v_log = 1 &  distfunc_v_range = [1e-10, 1e-6] & distfunc_unit='(s!E3!N/cm!E3!N-km!E3!N)'
  normal_distfunc_v_log=1 & normal_distfunc_v_range=[1e-10,1e-8] & normal_distfunc_unit='(s!E3!N/cm!E3!N-km!E3!N)'
  nv_v_log = 1 & nv_v_range = [0.1, 10]*1e9 & nv_unit = 'm!U-2!Ns!U-1'
  pitch_angle_v_log = 0 & pitch_angle_v_range = [0, 180] & pitch_angle_unit = 'deg'
  nvpara_over_b_v_log = 1 &  nvpara_over_b_v_range = [0.001, 0.01] & nvpara_over_b_unit = 'cm!U-3!N-km-s!U-1!N/gauss'
  temperature_v_log = 1 &  temperature_v_range = [1000, 10000] & temperature_unit = 'eV'
  densityratio_v_log = 1 &  densityratio_v_range = [0.001, 1] & densityratio_unit = ''

  density_weighted_V_RANGE = [0.0005, 0.05]

  pressure_V_LOG = 1 & pressure_V_RANGE = [0.01, 10] & pressure_unit = 'nPa'

  By_v_log = 0 & By_v_range = [-50., 50.] & By_unit = 'nT'
  Beta_v_log = 1 & Beta_v_range = [1e-4, 1.] & Beta_unit = ''

; -- load the property --
  IF property_name EQ 'distfunc' THEN BEGIN 
     property_para = data.distfunc_para & property_anti = data.distfunc_anti
     PROPERTY_v_log = distfunc_V_LOG & PROPERTY_V_RANGE = distfunc_V_RANGE 
     property_unit = distfunc_unit
  ENDIF
  IF property_name EQ 'normal_distfunc' THEN BEGIN 
     property_para = data.distfunc_para/data.mag & property_anti = data.distfunc_anti/data.mag
     PROPERTY_V_LOG = normal_distfunc_V_LOG & PROPERTY_V_RANGE = normal_distfunc_V_RANGE 
     property_unit = normal_distfunc_unit
  ENDIF
  IF property_name EQ 'energy' THEN BEGIN 
     property_para = data.en_para & property_anti = data.en_anti
     PROPERTY_V_LOG = ENERGY_V_LOG & 
     property_unit = energy_unit
     if property_map_type eq 'median' then  PROPERTY_V_RANGE = median_ENERGY_V_RANGE     $
     else if property_map_type eq 'minimum' then PROPERTY_V_RANGE = minimum_ENERGY_V_RANGE $
     else PROPERTY_V_RANGE = ENERGY_V_RANGE
     
  ENDIF 
  IF property_name EQ 'energy_v' THEN BEGIN 
     property_para = data.energy_v_para & property_anti = data.energy_v_anti
     PROPERTY_V_LOG = velocity_V_LOG & PROPERTY_V_RANGE = velocity_V_RANGE 
     property_unit = velocity_unit
  ENDIF 
  IF property_name EQ 'v_energy' THEN BEGIN 
     property_para = data.v_energy_para & property_anti = data.v_energy_anti
     PROPERTY_V_LOG = energy_V_LOG & PROPERTY_V_RANGE = energy_V_RANGE 
     property_unit = energy_unit
  ENDIF 

  IF property_name EQ 'flux' THEN BEGIN 
     property_para = data.flux_para  &  property_anti = data.flux_anti
     PROPERTY_V_LOG = FLUX_V_LOG &  PROPERTY_V_RANGE = FLUX_V_RANGE 
     property_unit = FLUX_unit
  ENDIF 
  IF property_name EQ 'eflux' THEN BEGIN 
     property_para = data.eflux_para  &  property_anti = data.eflux_anti
     PROPERTY_V_LOG = EFLUX_V_LOG &  PROPERTY_V_RANGE = EFLUX_V_RANGE 
     property_unit = EFLUX_unit
  ENDIF 

  IF property_name EQ 'total_density' THEN BEGIN 
   property_para = data.O_n &   property_anti = data.O_n
   PROPERTY_V_LOG = density_V_LOG &   PROPERTY_V_RANGE = density_V_RANGE 
   property_unit = density_unit
   ENDIF 
   IF property_name EQ 'total_velocity' THEN BEGIN 
      property_para = data.O_v &   property_anti = data.O_v
      PROPERTY_V_LOG = velocity_V_LOG &   PROPERTY_V_RANGE = velocity_V_RANGE 
      property_unit = velocity_unit
   ENDIF 

   IF property_name EQ 'total_density_ratio' THEN BEGIN 
      property_para = data.O_n / data.H_n &   property_anti = data.O_n / data.H_n
      PROPERTY_V_LOG = densityratio_V_LOG &   PROPERTY_V_RANGE = densityratio_V_RANGE 
      property_unit = densityratio_unit
   ENDIF 

   IF property_name EQ 'density' THEN BEGIN 
     property_para = data.n_para &   property_anti = data.n_anti
     PROPERTY_V_LOG = density_V_LOG &   PROPERTY_V_RANGE = density_V_RANGE 
     property_unit = density_unit
  ENDIF 

  IF property_name EQ 'density_weighted' THEN BEGIN 
   property_para = data.n_para_weighted &   property_anti = data.n_anti_weighted
   PROPERTY_V_LOG = density_V_LOG &   PROPERTY_V_RANGE = density_weighted_V_RANGE 
   property_unit = density_unit
ENDIF 

  IF property_name EQ 'velocity' THEN BEGIN 
     property_para = data.v_para &  property_anti = data.v_anti
     PROPERTY_V_LOG = velocity_V_LOG &   PROPERTY_V_RANGE = velocity_V_RANGE 
     property_unit = velocity_unit
  ENDIF 

  IF property_name EQ 'V_H' THEN BEGIN 
      property_para = data.H_v &  property_anti = data.H_v
      PROPERTY_V_LOG = velocity_V_LOG &   PROPERTY_V_RANGE = velocity_V_RANGE 
      property_unit = velocity_unit
   ENDIF 

  IF property_name EQ 'pressure' THEN BEGIN 
   property_para = data.p_para &  property_anti = data.p_anti
   PROPERTY_V_LOG = pressure_V_LOG &   PROPERTY_V_RANGE = pressure_V_RANGE 
   property_unit = pressure_unit
   ENDIF
   IF property_name EQ 'Vpar_O' THEN BEGIN 
      property_para = data.O_VPAR & property_anti = data.O_VPAR
      PROPERTY_V_LOG = velocity_Vpara_LOG &  PROPERTY_V_RANGE = velocity_Vpara_RANGE
      property_unit = velocity_unit
   ENDIF 
   IF property_name EQ 'Vperp_O' THEN BEGIN 
      property_para = data.O_VPERP & property_anti = data.O_VPERP
      PROPERTY_V_LOG = velocity_Vperp_LOG &  PROPERTY_V_RANGE = velocity_Vperp_RANGE

      property_unit = velocity_unit
   ENDIF 

   IF property_name EQ 'Vpar_H' THEN BEGIN 
      property_para = data.H_VPAR & property_anti = data.H_VPAR
      PROPERTY_V_LOG = velocity_Vpara_LOG &  PROPERTY_V_RANGE = velocity_Vpara_RANGE
      property_unit = velocity_unit
   ENDIF 
   IF property_name EQ 'Vperp_H' THEN BEGIN 
   property_para = data.H_VPERP & property_anti = data.H_VPERP
   PROPERTY_V_LOG = velocity_Vperp_LOG &  PROPERTY_V_RANGE = velocity_Vperp_RANGE

   property_unit = velocity_unit
   ENDIF 

  IF property_name EQ 'Vpar' THEN BEGIN 
     property_para = ABS(data.vpar_para) & property_anti = ABS(data.vpar_anti)
     PROPERTY_V_LOG = velocity_Vpara_LOG &   PROPERTY_V_RANGE = velocity_Vpara_RANGE 
     property_unit = velocity_unit
  ENDIF 
  IF property_name EQ 'Vperp' THEN BEGIN 
     property_para = data.Vperp_para &  property_anti = data.vperp_anti
     PROPERTY_V_LOG = velocity_Vperp_LOG &   PROPERTY_V_RANGE = velocity_Vperp_RANGE 
     property_unit = velocity_unit
  ENDIF 
  IF property_name EQ 'nV' THEN BEGIN 
     property_para = data.n_para*data.v_para*1e9 &  property_anti = data.n_anti*data.velocity_anti*1e9
     PROPERTY_V_LOG = nv_V_LOG &  PROPERTY_V_RANGE = nv_V_RANGE 
     property_unit = nv_unit
  ENDIF 
  IF property_name EQ 'pitch_angle' THEN BEGIN 
     property_para = data.pa_para & property_anti = data.pa_anti
     PROPERTY_V_LOG = pitch_angle_V_LOG & PROPERTY_V_RANGE = pitch_angle_V_RANGE 
     property_unit = pitch_angle_unit
  ENDIF 
  IF property_name EQ 'nVpara_over_B' THEN BEGIN 
     property_para = (data.n_para*ABS(data.Vpar_para))/data.mag & property_anti = (data.n_anti*ABS(data.Vpar_anti))/mag
     PROPERTY_V_LOG = nvpara_over_b_v_log & PROPERTY_V_RANGE = nvpara_over_b_v_range
     property_unit = nvpara_over_b_unit
  ENDIF 
  IF property_name EQ 'temperature' THEN BEGIN 
     property_para = ABS(data.t_para) & property_anti = ABS(data.t_anti)
     PROPERTY_V_LOG = temperature_v_log & PROPERTY_V_RANGE = temperature_v_range
     property_unit = temperature_unit
  ENDIF
  IF property_name EQ 'By' THEN BEGIN 
     property_para = data.By & property_anti = data.By ; replicate(!values.f_nan,n_elements(data.By))
     PROPERTY_V_LOG = By_v_log &  PROPERTY_V_RANGE = By_v_range

     property_unit = By_unit
  ENDIF 
  IF property_name EQ 'Beta' THEN BEGIN 
     property_para = data.Beta & property_anti = data.Beta ; replicate(!values.f_nan,n_elements(data.Beta))
     PROPERTY_V_LOG = Beta_v_log &  PROPERTY_V_RANGE = Beta_v_range
     property_unit = Beta_unit
  ENDIF  

  IF ~KEYWORD_SET(property_para) THEN BEGIN 
     print, 'no ' +property_name+' avaliabe for mapping'
     stop
  ENDIF 

END
