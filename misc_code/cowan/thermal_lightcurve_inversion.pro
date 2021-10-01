PRO thermal_lightcurve_inversion, input_file, system_name, mc_steps, max_mode, mode_threshold, resolution
; Nick Cowan
; Oct 2009
; Please cite Cowan & Agol (2008) if you use this code
;
; INPUTS
; input_file: string. The file in question should be an ASCII file
;             with four columns: time, phase, flux, flux_err
; system_name: string, used for naming figures and files
; mc_steps: number of steps in Monte Carlo error estimate
; max_mode: integer, highest-order sinusoidal mode to consider in fit
; mode_threshold: the significance a mode must have (in units of
;                 sigma) to be included in analysis
; resolution: number of xi and phi to use in output arrays and figures
;
; OUTPUTS
; log file and sav file (in fit_logs/), as well as figures (in plots/) are produced


prefix = system_name+'_mode'+ STRTRIM(STRING(max_mode),2)+'_threshold'+STRTRIM(STRING(mode_threshold),2)+'_MC'+STRTRIM(STRING(mc_steps),2)

n_params = max_mode*2 + 1

READCOL, input_file, time, phase, flux, flux_err, FORMAT="(D,D,D,D)"

; sort the data (just in case)
sorted_indices = SORT(time)
time = time[sorted_indices]
phase = phase[sorted_indices]
flux = flux[sorted_indices]
flux_err = flux_err[sorted_indices]

; remove ramp-fit contaminated data
;good_data = WHERE(phase GT 0.07)
;time = time[good_data]
;phase = phase[good_data]
;flux = flux[good_data]
;flux_err = flux_err[good_data]

; remove data during secondary eclipse
good_data = WHERE(phase LT 0.47 OR phase GT 0.53)
time = time[good_data]
phase = phase[good_data]
flux = flux[good_data]
flux_err = flux_err[good_data]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bin the data
; IF(binning GT 1) THEN BEGIN
;     unbinned_phase = phase
;     unbinned_flux = flux
;     unbinned_flux_err = flux_err

;     bin_size = binning
    
;     new_size = N_ELEMENTS(phase)/bin_size
;     rebinned_phase = MAKE_ARRAY(new_size,/DOUBLE)
;     rebinned_flux = MAKE_ARRAY(new_size,/DOUBLE)
;     rebinned_flux_err = MAKE_ARRAY(new_size,/DOUBLE)
    
;     FOR index=0.0,new_size-1.0 DO BEGIN
;         rebinned_phase[index] = MEAN(phase[index*bin_size : (index+1)*bin_size-1])
;         rebinned_flux[index] = MEAN(flux[index*bin_size : (index+1)*bin_size-1])
;         rebinned_flux_err[index] = STDDEV(flux[index*bin_size : (index+1)*bin_size-1]) / SQRT(bin_size)
;     ENDFOR

;     phase = rebinned_phase
;     flux = rebinned_flux
;     flux_err = rebinned_flux_err
; ENDIF



; name swap
; input phase goes from 0 (transit) through 0.5 (eclipse) to 1
; output phase goes from -pi (transit) through 0 (eclipse) to +pi
x = (phase+0.5) * 2*!PI
x[WHERE(x GT !PI)] = x[WHERE(x GT !PI)] - 2*!PI
y = flux - 1
y_err = flux_err
n_y = N_ELEMENTS(y)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Reality-Check before fitting 
;OPENW,2,'fit_logs/'+'unfitted_'+prefix+'_light_curve.dat'
;PRINTF,2,'Phase Flux Error'
;FOR index=0L, N_ELEMENTS(x)-1 DO BEGIN 
;    PRINTF,2, x[index], y[index], y_err[index]
;ENDFOR
;CLOSE,2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fourier Decomposition of Light Curve
; lc_parameters = MAKE_ARRAY(n_params, mc_steps, /DOUBLE)

; FOR index=0, mc_steps-1 DO BEGIN
;     this_y = y + y_err*RANDOMN(seed, n_y)
;     lc_parameters[*,index] = fourier_decomposition(x, this_y, n_params)
; ENDFOR

; best_lc_parameters = MAKE_ARRAY(n_params, /DOUBLE)
; lc_errors = MAKE_ARRAY(n_params, /DOUBLE)

; FOR index=0, n_params-1 DO BEGIN
;     best_lc_parameters[index] = MEDIAN(lc_parameters[index,*])
;     lc_errors[index] = STDDEV(lc_parameters[index,*])
; ENDFOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Use MCMC to determine lightcurve parameters
parinfo = REPLICATE({fixed:0, limited:[0,0], limits:[0D,0D], value:0D, step_size:0D}, n_params)
parinfo[0].limited[0] = 1
parinfo[0].value = MEAN(y)
parinfo.step_size = MEAN(y)/10

mcmc_best_parameters = markov_chain_monte_carlo(x, y, y_err, 'coefficients_to_sinusoids', parinfo, mc_steps, 1, "fit_logs/raw_"+prefix)

best_lc_parameters = mcmc_best_parameters.parameters
lc_errors = mcmc_best_parameters.uncertainties

; Print Best-Fit Parameters and Chi-Square
PRINT, "**************************************"
PRINT, "Raw Parameters from MCMC"
PRINT, "BEST FIT LIGHTCURVE PARAMETERS:"
PRINT, best_lc_parameters
PRINT, 'UNCERTAINTY:'
PRINT, lc_errors    
chi_square = TOTAL((coefficients_to_sinusoids(x, best_lc_parameters) - y)^2/y_err^2)
PRINT, "CHI SQUARE: ", chi_square
temp = WHERE(ABS(best_lc_parameters) GT 0, n_model_parameters)
DoF = n_y - n_model_parameters
PRINT, "DoF: ", DoF
reduced_chi_square = chi_square/DoF
PRINT, "REDUCED CHI SQUARE: ", reduced_chi_square
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Check if (supposedly invisible) odd modes are present
IF(max_mode LT 3) THEN n_odd_modes = 0
IF(max_mode EQ 3) THEN BEGIN
    n_odd_modes = 2
    odd_modes = [3,6]
ENDIF
IF(max_mode EQ 4) THEN BEGIN
    n_odd_modes = 2
    odd_modes = [3,7]
ENDIF

IF(n_odd_modes GT 0) THEN BEGIN
    IF(MIN(ABS(best_lc_parameters[odd_modes]) - lc_errors[odd_modes]*mode_threshold) GT 0) THEN BEGIN
        PRINT,'********************************************************'
        PRINT, "WARNING: ODD MODES DETECTED AT "+STRTRIM(STRING(mode_threshold),2)+"-SIGMA LEVEL"
        PRINT, "BEST FIT:"
        PRINT, best_lc_parameters
        PRINT, 'UNCERTAINTY:'
        PRINT, lc_errors    
    ENDIF
ENDIF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set Insignificant Modes to Zero
insignificant = WHERE(ABS(best_lc_parameters) - mode_threshold*lc_errors LE 0, n_insignificant)
IF(n_insignificant GT 0) THEN BEGIN
    parinfo[insignificant].value = 0.0
    parinfo[insignificant].fixed = 1
ENDIF

; Set Odd Modes to Zero
IF(n_odd_modes GT 0) THEN BEGIN
    parinfo[odd_modes].value = 0.0
    parinfo[odd_modes].fixed = 1
ENDIF

; Recompute best-fit parameters
IF(n_insignificant GT 0 OR n_odd_modes GT 0) THEN BEGIN
    mcmc_best_parameters = markov_chain_monte_carlo(x, y, y_err, 'coefficients_to_sinusoids', parinfo, mc_steps, 1, "fit_logs/"+prefix)
    best_lc_parameters = mcmc_best_parameters.parameters
    lc_errors = mcmc_best_parameters.uncertainties
ENDIF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Determine Best-Fit Map and Uncertainties
F_0 = best_lc_parameters[0]
C = best_lc_parameters[1:(n_params-1)/2]
D = best_lc_parameters[(n_params-1)/2 + 1:n_params-1]
sinusoidal_eigen, F_0, C, D, J_0, A, B, 0
best_map_parameters = [J_0, A]
best_map_parameters = [best_map_parameters, B]

F_0_err = lc_errors[0]
C_err = lc_errors[1:(n_params-1)/2]
D_err = lc_errors[(n_params-1)/2 + 1:n_params-1]
sinusoidal_eigen, F_0_err, C_err, D_err, J_0_err, A_err, B_err, 0
best_map_errors = [J_0_err, A_err]
best_map_errors = [best_map_errors, B_err]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compute Lightcurve, Map and Associated Uncertainties
meta_map = MAKE_ARRAY(resolution, mc_steps, /DOUBLE)
meta_lc = MAKE_ARRAY(resolution, mc_steps, /DOUBLE)

xi = ARRSCL(FINDGEN(resolution), -!PI, !PI)
phi = ARRSCL(FINDGEN(resolution), -!PI, !PI)

FOR index=0, mc_steps-1 DO BEGIN
    lc_parameters = best_lc_parameters + lc_errors*RANDOMN(seed, n_params)
    IF(n_odd_modes GT 0) THEN lc_parameters[odd_modes] = 0.0
    IF(n_insignificant GT 0) THEN lc_parameters[insignificant] = 0.0
    meta_lc[*,index] = coefficients_to_sinusoids(xi, lc_parameters)
    F_0 = lc_parameters[0]
    C = lc_parameters[1:(n_params-1)/2]
    D = lc_parameters[(n_params-1)/2 + 1:n_params-1]
    sinusoidal_eigen, F_0, C, D, J_0, A, B, 0
    map_parameters = [J_0, A]
    map_parameters = [map_parameters, B]
    meta_map[*,index] = coefficients_to_sinusoids(phi, map_parameters)
ENDFOR

best_lc = coefficients_to_sinusoids(xi, best_lc_parameters)
hi_lc = MAKE_ARRAY(resolution, /DOUBLE)
lo_lc = MAKE_ARRAY(resolution, /DOUBLE)

best_map = coefficients_to_sinusoids(phi, best_map_parameters)
hi_map = MAKE_ARRAY(resolution, /DOUBLE)
lo_map = MAKE_ARRAY(resolution, /DOUBLE)

FOR index=0, resolution-1 DO BEGIN
    sorted_lc = meta_lc[index,SORT(meta_lc[index,*])]
    lo_lc[index] = sorted_lc[0.15866*mc_steps]
    hi_lc[index] = sorted_lc[0.84135*mc_steps]

    sorted_map = meta_map[index,SORT(meta_map[index,*])]
    lo_map[index] = sorted_map[0.15866*mc_steps]
    hi_map[index] = sorted_map[0.84135*mc_steps]
ENDFOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Print Best-Fit Parameters
PRINT, "**************************************"
PRINT, "After Removing Insignificant and Odd Modes"
PRINT, "BEST FIT LIGHTCURVE PARAMETERS:"
PRINT, best_lc_parameters
PRINT, 'UNCERTAINTY:'
PRINT, lc_errors    

PRINT, "BEST FIT MAP PARAMETERS:"
PRINT, best_map_parameters
PRINT, 'UNCERTAINTY:'
PRINT, best_map_errors    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compute Chi-Square
chi_square = TOTAL((coefficients_to_sinusoids(x, best_lc_parameters) - y)^2/y_err^2)
PRINT, "CHI SQUARE: ", chi_square
temp = WHERE(ABS(best_lc_parameters) GT 0, n_model_parameters)
DoF = n_y - n_model_parameters
PRINT, "DoF: ", DoF
reduced_chi_square = chi_square/DoF
PRINT, "REDUCED CHI SQUARE: ", reduced_chi_square
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Make Figures
!P.THICK=2
!X.THICK=2
!Y.THICK=2
!P.CHARTHICK=2
!P.CHARSIZE=1.2
!P.FONT=-1 

SET_PLOT,'ps'
LOADCT,40

DEVICE, FILENAME="plots/"+prefix+"_lightcurve.ps",/COLOR
PLOT, xi, best_lc, XTITLE="Orbital Phase", YTITLE="Relative Flux",/XSTYLE, XTICKS=4, XTICKNAME=[TEXTOIDL('-\pi'),TEXTOIDL('-\pi/2'),TEXTOIDL('0'),TEXTOIDL('\pi/2'),TEXTOIDL('\pi')], TITLE=system_name+" (Only using >"+STRTRIM(STRING(mode_threshold),2)+TEXTOIDL("\sigma modes)")
OPLOT, xi, lo_lc, LINE=1
OPLOT, xi, hi_lc, LINE=1
OPLOTERROR, x, y, y_err, PSYM=3, COLOR=250, ERRCOLOR=250
LEGEND,[TEXTOIDL("\chi^{2} = ")+STRTRIM(STRING(chi_square),2), 'DoF = '+STRTRIM(STRING(DoF),2), TEXTOIDL("\chi_{R}^{2} = ")+STRTRIM(STRING(reduced_chi_square),2)], /BOTTOM, /LEFT, BOX=0
DEVICE,/CLOSE 

DEVICE, FILENAME="plots/"+prefix+"_map.ps",/COLOR
PLOT, phi, best_map, XTITLE="Longitude", YTITLE="Relative Flux",/XSTYLE, XTICKS=4, XTICKNAME=['180 W',TEXTOIDL('90 W'),TEXTOIDL('0'),TEXTOIDL('90 E'),TEXTOIDL(' 180 E')], TITLE=system_name+" (Only using >"+STRTRIM(STRING(mode_threshold),2)+TEXTOIDL("\sigma modes)")
OPLOT, phi, lo_map, LINE=1
OPLOT, phi, hi_map, LINE=1
LEGEND,[TEXTOIDL("\chi^{2} = ")+STRTRIM(STRING(chi_square),2), 'DoF = '+STRTRIM(STRING(DoF),2), TEXTOIDL("\chi_{R}^{2} = ")+STRTRIM(STRING(reduced_chi_square),2)], /BOTTOM, /LEFT, BOX=0
DEVICE,/CLOSE 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Save Relevant Info to File
SAVE, FILENAME="fit_logs/"+ prefix+'.sav', x, y, y_err, xi, best_lc, lo_lc, hi_lc, phi, best_map, lo_map, hi_map, best_lc_parameters, lc_errors, best_map_parameters, best_map_errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


SET_PLOT,'x'
LOADCT,0
STOP

END
