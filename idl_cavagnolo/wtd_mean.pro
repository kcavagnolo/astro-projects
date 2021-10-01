FUNCTION WTD_MEAN, in_values, in_weights, _EXTRA = extra

ON_ERROR, 0

IF (N_PARAMS() ne 2) THEN MESSAGE, 'error-bad param list'

values  = DOUBLE(in_values)     ;- protect input
weights = DOUBLE(in_weights)

IF (N_ELEMENTS(values) ne N_ELEMENTS(weights)) THEN MESSAGE, 'input array size mismatch'

good_pts_val = WHERE(FINITE(values) EQ 1, count) ;- find non-NaN values
IF (count GT 0) THEN BEGIN
   values  = values[good_pts_val]
   weights = weights[good_pts_val]
ENDIF

good_pts_wts = WHERE(FINITE(weights) EQ 1, count)  ;- find also w/ corresp.
IF (count GT 0) THEN BEGIN                         ;  non-NaN weights
   values  = values[good_pts_wts]
   weights = weights[good_pts_wts]
ENDIF

tot_weights   = TOTAL(weights)
IF (tot_weights EQ 0.0) THEN MESSAGE, 'weights sum to zero'

IF (N_ELEMENTS(values) EQ 1) THEN  $               ;- calc. mean value:  IF
   mean_value = values[0]  $                       ;  only 1 good pt., mean
ELSE BEGIN                                         ;  is the value of that pt.
   weights    = weights / tot_weights
   mean_value = TOTAL(values * weights)
ENDELSE

out_mean = mean_value
RETURN, out_mean

END
