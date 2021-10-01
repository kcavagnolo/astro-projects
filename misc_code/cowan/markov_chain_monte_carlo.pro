FUNCTION markov_chain_monte_carlo, x, y, y_err, function_name, parinfo, n_steps, improve_step_size, output_file
; Markov Chain Monte Carlo
; Sept 2008
; Nick Cowan (UW)
;
; INPUTS
; x (1D or 2D array) independent variable
; y (same dimensions as x) dependent variable
; function_name (string)
; parinfo (1D or 2D array of structures) starting parameters, parameter limits, fiducial step size, etc.
; n_steps number of steps per parameter
; improve_step_size: if 1, the chain is run twice, adjusting the step
;                    size after first run-through to get 25% acceptance rate.  Second run
;                    has 10 times more steps per parameter 
; output_file (string) final names have .log or .fits added at the end
;
; PARINFO Example:
; parinfo = replicate({value:0.D, fixed:0, limited:[0,0], limits:[0.D,0], step_size:1.0}, 5)
; parinfo(0).fixed = 1
; parinfo(4).limited(0) = 1
; parinfo(4).limits(0)  = 50.D
; parinfo(*).value = [5.7D, 2.2, 500., 1.5, 2000.]
; parinfo(3).step_size = 0.01
;  
; A total of 5 parameters, with starting values of 5.7,
; 2.2, 500, 1.5, and 2000 are given.  The first parameter
; is fixed at a value of 5.7, and the last parameter is
; constrained to be above 50.

; make the first run through quick and dirty
IF(improve_step_size EQ 1) THEN n_steps = n_steps/10


; replace spurious step_size
bad_step_sizes = WHERE(parinfo.fixed EQ 0 AND parinfo.step_size EQ 0, n_bad_step_sizes, COMPLEMENT=good_step_sizes)
IF(n_bad_step_sizes GT 0) THEN BEGIN 
    parinfo[bad_step_sizes].step_size = 0.1*parinfo[bad_step_sizes].value
    PRINT,'WARNING: BAD STEP SIZE NEEDED TO BE REPLACED'    
ENDIF

; initialize some variables
n_parameters = N_ELEMENTS(parinfo.value)
free_parameters = N_ELEMENTS(WHERE(parinfo[*].fixed EQ 0))
rejected = MAKE_ARRAY(n_parameters, VALUE=0.0D0)
step = MAKE_ARRAY(n_parameters, VALUE=0.0D0)
good_parameters = REPLICATE({chi_square:0D, parameters:parinfo.value}, free_parameters*n_steps)
current_parameters = parinfo[*].value
current_y = CALL_FUNCTION(function_name, x, current_parameters)
normalized_residuals = (current_y-y)/y_err
current_chi_square = TOTAL(normalized_residuals^2)

; write useful stuff to log file
IF(improve_step_size EQ 0) THEN BEGIN
    OPENW, 2, output_file+'.log'
    PRINTF, 2, 'Steps per Parameter = ',n_steps
    PRINTF, 2, 'Start Parameters = '
    PRINTF, 2, parinfo[*].value
    PRINTF, 2, 'Fiducial Parameter Steps = '
    PRINTF, 2, parinfo[*].step_size
    PRINTF, 2, 'Starting Chi Square = ', current_chi_square
ENDIF

; THE LONG PART
FOR index=0L, n_steps*free_parameters-1 DO BEGIN
    par_index = 0
    WHILE(par_index LE n_parameters-1) DO BEGIN
        ; MAKE SURE PARAMETER ISN'T FIXED
        IF(parinfo[par_index].fixed EQ 1) THEN par_index = par_index + 1 ELSE BEGIN
            delta_parameter = RANDOMN(seed)*parinfo[par_index].step_size
            ; MAKE SURE NEW PARAMETER IS WITHIN LIMITS
            IF((parinfo[par_index].limited[0] EQ 0 OR current_parameters[par_index] + delta_parameter GE parinfo[par_index].limits[0]) AND (parinfo[par_index].limited[1] EQ 0 OR current_parameters[par_index] + delta_parameter LE parinfo[par_index].limits[1])) THEN BEGIN           
                candidate_parameters = current_parameters
                candidate_parameters[par_index] = candidate_parameters[par_index] + delta_parameter
                model_y = CALL_FUNCTION(function_name, x, candidate_parameters)
                normalized_residuals = (model_y-y)/y_err
                candidate_chi_square = TOTAL(normalized_residuals^2)

                ; HASTING-METROPOLIS TRANSITION PROBABILITY
                IF(candidate_chi_square GT current_chi_square) THEN BEGIN
                    acceptance_probability = EXP((current_chi_square - candidate_chi_square)/2)
                ENDIF ELSE BEGIN
                    acceptance_probability = 1.0
                ENDELSE        
                IF(RANDOMU(seed) LT acceptance_probability) THEN BEGIN
                    current_chi_square = candidate_chi_square
                    current_parameters = candidate_parameters
                ENDIF ELSE BEGIN                    
                    rejected[par_index] = rejected[par_index] + 1
                ENDELSE
               
                good_parameters[index].chi_square = current_chi_square
                good_parameters[index].parameters = current_parameters
                step[par_index] = step[par_index] + 1
                par_index = par_index + 1
            ENDIF
        ENDELSE
    ENDWHILE  
    ; REALITY CHECK FOR THE IMPATIENT
    IF((index MOD 10) EQ 0) THEN PRINT,'AT STEP ',index,' OF ',n_steps*free_parameters,' THE CHI SQUARE IS ',current_chi_square
    ;STOP ; stop after one iteration
ENDFOR


acceptance_probability = 1.0 - rejected/step
best_chi_square = MIN(good_parameters[*].chi_square, best_index)
DOF = N_ELEMENTS(y)-free_parameters
best_reduced_chi_square = best_chi_square/(DOF)
best_parameters = good_parameters[best_index].parameters
parinfo[*].value =  best_parameters

one_sigma_uncertainties = MAKE_ARRAY(n_parameters,/DOUBLE)
FOR index = 0, n_parameters-1 DO one_sigma_uncertainties[index] = mcmc_uncertainty_one_sigma(good_parameters[*].parameters[index])

stdev_error = MAKE_ARRAY(n_parameters, /DOUBLE)
FOR index=0,n_parameters-1 DO stdev_error[index] = STDDEV(good_parameters[*].parameters[index])

IF(improve_step_size EQ 0) THEN BEGIN
    PRINTF, 2, 'Best Chi Square = ',best_chi_square
    PRINTF, 2, 'Free Parameters = ', free_parameters
    PRINTF, 2, 'DOF = ',DOF
    PRINTF, 2, 'Best Reduced Chi Square = ',best_reduced_chi_square
    PRINTF, 2, 'Acceptance Probabilities = '
    PRINTF, 2, acceptance_probability
    PRINTF, 2, 'Best Parameters = '
    PRINTF, 2, best_parameters
    PRINTF, 2, 'Uncertainty in Parameters from 1-sigma Interval of Chain = '
    PRINTF, 2, one_sigma_uncertainties
    PRINTF, 2, 'Uncertainty in Parameters from STDDEV of Chain = '
    PRINTF, 2, stdev_error
    CLOSE, 2

    MWRFITS, good_parameters, output_file+'.fits', /CREATE    
ENDIF

best_fit = {parameters:best_parameters, uncertainties:stdev_error}

IF(improve_step_size EQ 1) THEN BEGIN
    ; Gelman et al. (2003) recommend an acceptance 
    ; probability of ~0.25 for MCMC with many parameters
    parinfo.value = best_parameters
    target_probability = REPLICATE(0.25, n_parameters)
    parinfo.step_size = parinfo.step_size * SQRT(acceptance_probability/target_probability)
    best_fit = markov_chain_monte_carlo(x, y, y_err, function_name, parinfo, 10*n_steps, 0, output_file)
ENDIF

; FOR DEBUGGING
;PRINT,'ALL DONE!'
;STOP


RETURN, best_fit

END
